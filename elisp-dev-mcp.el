;;; elisp-dev-mcp.el --- MCP server for agentic Elisp development -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: You
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, development
;; URL: https://github.com/your-username/elisp-dev-mcp

;;; Commentary:

;; This package provides an MCP server for agentic Elisp development.

;;; Code:

(require 'mcp)
(require 'help-fns)
(require 'pp)

(defun elisp-dev-mcp--non-empty-docstring-p (doc)
  "Return t if DOC is a non-empty documentation string, nil otherwise."
  (and doc (not (string-empty-p doc))))

(defun elisp-dev-mcp--describe-function (function)
  "Get full documentation for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to describe"
  (condition-case err
      (let ((sym (intern function)))
        (if (fboundp sym)
            (with-temp-buffer
              (let ((standard-output (current-buffer)))
                (describe-function-1 sym)
                (buffer-string)))
          (mcp-tool-throw (format "Function %s is void" function))))
    (error (mcp-tool-throw (format "Error: %S" err)))))

(defun elisp-dev-mcp--process-alias-source
    (source function aliased-to file-path start-line end-line)
  "Post-process SOURCE for function aliases to return a useful defalias form.
If source is just a quoted symbol, replace it with a synthetic defalias form.
Returns a JSON encoded response with enhanced alias information.

FUNCTION is the alias function name.
ALIASED-TO is the target function name.
FILE-PATH, START-LINE, and END-LINE specify source location information."
  (let ((doc (or (documentation (intern-soft function)) "")))
    (if (and source
             (string-match-p (format "['']%s\\>" function) source)
             (not (string-match-p "defalias" source)))
        ;; Generate synthetic defalias form
        (let ((func-def
               (format "(defalias '%s #'%s %S)"
                       function
                       aliased-to
                       doc)))
          (json-encode
           `((source . ,func-def)
             (file-path . ,file-path)
             (start-line . ,start-line)
             (end-line . ,end-line))))
      ;; Pass through original source
      (json-encode
       `((source . ,source)
         (file-path . ,file-path)
         (start-line . ,start-line)
         (end-line . ,end-line))))))

(defun elisp-dev-mcp--get-function-definition-c-function (fn-name)
  "Return response for C-implemented FN-NAME in get-function-definition."
  (json-encode
   `((is-c-function . t)
     (function-name . ,fn-name)
     (message .
              ,(format
                "Function `%s` is implemented in C source code. \
Use elisp-describe-function tool to get its docstring."
                fn-name)))))

(defun elisp-dev-mcp--extract-function-body (fn has-doc)
  "Extract body from function object FN.
HAS-DOC indicates whether the function has a docstring.
Returns nil if FN is not a function."
  (if (not (functionp fn))
      nil
    (cond
     ;; Emacs 30+ interpreted-function objects
     ((eq (type-of fn) 'interpreted-function)
      ;; Extract body from interpreted-function
      ;; Format: #[args body env bytecode doc]
      (aref fn 1))
     ;; Emacs 29 and earlier cons-based functions
     ((consp fn)
      (nthcdr
       (if has-doc
           3
         2)
       fn))
     ;; Fallback for other types
     (t
      (mcp-tool-throw
       (format "Don't know how to extract body from function type: %s"
               (type-of fn)))))))

(defun elisp-dev-mcp--reconstruct-function-definition
    (fn-name args doc body)
  "Reconstruct a function definition from its runtime components.
This is used for interactively defined functions where the source file
is not available.  Creates a synthetic defun form.

FN-NAME is the function name as a string.
ARGS is the argument list.
DOC is the documentation string (can be empty).
BODY is the list of body expressions."
  (with-temp-buffer
    (insert "(defun " fn-name)
    (if args
        (insert " " (prin1-to-string args))
      (insert " ()"))
    (when (elisp-dev-mcp--non-empty-docstring-p doc)
      (insert "\n  " (prin1-to-string doc)))
    (if body
        (dolist (expr body)
          (insert "\n  " (pp-to-string expr)))
      (mcp-tool-throw
       (format "Failed to extract body for function %s" fn-name)))
    (insert ")")
    (buffer-string)))

(defun elisp-dev-mcp--get-function-definition-interactive
    (fn-name sym fn)
  "Handle interactively defined function FN-NAME.
SYM is the function symbol, FN is the function object.
Returns JSON response for an interactively defined function."
  (let* ((args (help-function-arglist sym t))
         (doc (documentation sym))
         (body
          (elisp-dev-mcp--extract-function-body
           fn (elisp-dev-mcp--non-empty-docstring-p doc)))
         (func-def
          (elisp-dev-mcp--reconstruct-function-definition
           fn-name args doc body)))
    (json-encode
     `((source . ,func-def)
       (file-path . "<interactively defined>")
       (start-line . 1)
       (end-line . 1)))))

(defun elisp-dev-mcp--describe-variable (variable)
  "Get information about Emacs Lisp VARIABLE without exposing its value.

MCP Parameters:
  variable - The name of the variable to describe"
  (unless (stringp variable)
    (mcp-tool-throw "Invalid variable name"))
  (condition-case nil
      (let* ((sym (intern variable))
             (type (type-of (symbol-value sym)))
             (doc
              (documentation-property sym 'variable-documentation))
             (file (find-lisp-object-file-name sym 'defvar))
             (custom-p (custom-variable-p sym))
             (obsolete (get sym 'byte-obsolete-variable)))
        (json-encode
         `((name . ,variable)
           (bound . t)
           (value-type . ,(symbol-name type))
           (documentation . ,doc)
           (source-file . ,(or file "<interactively defined>"))
           (is-custom
            .
            ,(if custom-p
                 t
               :json-false))
           (is-obsolete
            .
            ,(if obsolete
                 t
               :json-false))
           ,@
           (when obsolete
             `((obsolete-since . ,(nth 2 obsolete))
               (obsolete-replacement . ,(nth 0 obsolete)))))))
    (void-variable
     (mcp-tool-throw (format "Variable %s is not bound" variable)))))

(defun elisp-dev-mcp--get-function-definition-from-file
    (fn-name sym func-file is-alias aliased-to)
  "Extract function definition for FN-NAME from FUNC-FILE.
SYM is the function symbol.
IS-ALIAS and ALIASED-TO are used for special handling of aliases."
  (with-temp-buffer
    (insert-file-contents func-file)
    (goto-char (point-min))
    (let ((def-pos
           (find-function-search-for-symbol sym nil func-file)))
      (unless def-pos
        (mcp-tool-throw
         (format "Could not locate definition for %s" fn-name)))
      (goto-char (cdr def-pos))
      ;; Get function definition with any header comments
      (let* ((func-point (point))
             (func-line (line-number-at-pos))
             (start-point func-point)
             (start-line func-line)
             (end-line nil)
             (source nil))

        ;; Go back to search for comments
        (beginning-of-line)
        (forward-line -1) ;; Check line above function

        ;; If this is a comment line, it's part of the header comment
        (when (looking-at "^[ \t]*;;")

          ;; Find first line of the consecutive comment block
          (while (and (looking-at "^[ \t]*;;")
                      (> (forward-line -1) -1)))

          ;; We went one line too far back
          (forward-line 1)

          ;; Update start point to include header comments
          (setq start-point (point))
          (setq start-line (line-number-at-pos)))

        ;; Return to function start point to process the definition
        (goto-char func-point)
        (forward-sexp)
        (setq end-line (line-number-at-pos))

        ;; Extract the source code including any header comments
        (setq source
              (buffer-substring-no-properties start-point (point)))

        ;; Return the result, with special handling for aliases
        (if is-alias
            (elisp-dev-mcp--process-alias-source
             source fn-name aliased-to func-file start-line end-line)
          (json-encode
           `((source . ,source)
             (file-path . ,func-file)
             (start-line . ,start-line)
             (end-line . ,end-line))))))))

(defun elisp-dev-mcp--get-function-definition (function)
  "Get the source code definition for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to retrieve"
  (unless (stringp function)
    (mcp-tool-throw "Invalid function name"))
  (let* ((sym (intern-soft function))
         (fn (and sym (fboundp sym) (symbol-function sym)))
         (is-alias (symbolp fn))
         (aliased-to (and is-alias (symbol-name fn))))
    (unless (and sym (fboundp sym))
      (mcp-tool-throw (format "Function %s is not found" function)))

    ;; Special handling for C-implemented functions (subrp)
    (if (subrp fn)
        (elisp-dev-mcp--get-function-definition-c-function function)

      ;; Regular Elisp function handling
      (let ((func-file (find-lisp-object-file-name sym 'defun)))
        (if (not func-file)
            ;; Handle interactively defined functions with no source file
            (if is-alias
                ;; For aliases, create a synthetic defalias form
                (elisp-dev-mcp--process-alias-source
                 (format "'%s" function) ; create minimal source
                 function aliased-to "<interactively defined>" 1 1)
              ;; Regular interactively defined functions
              (elisp-dev-mcp--get-function-definition-interactive
               function sym fn))

          ;; Functions with source file
          (elisp-dev-mcp--get-function-definition-from-file
           function sym func-file is-alias aliased-to))))))

;;;###autoload
(defun elisp-dev-mcp-enable ()
  "Enable the Elisp development MCP tools."
  (mcp-register-tool
   #'elisp-dev-mcp--describe-function
   :id "elisp-describe-function"
   :description
   "Get documentation for an Emacs Lisp function or check if it exists. Returns
function documentation from the current running Emacs environment, including all
currently loaded packages and libraries.

Supports:
- Regular functions (defun), macros (defmacro), inline functions (defsubst)
- Function aliases (shows both alias info and target function docs)
- Built-in C functions (subr)
- Byte-compiled functions
- Functions with or without documentation

Returns formatted documentation including:
- Function signature with argument names
- Full docstring with parameter descriptions
- Source file location
- Function type (closure, macro, subr, etc.)

Error cases:
- Non-existent functions return 'Function X is void'
- Invalid input types return 'Error: ...'"
   :read-only t)
  (mcp-register-tool
   #'elisp-dev-mcp--get-function-definition
   :id "elisp-get-function-definition"
   :description
   "Get the source code definition of an Emacs Lisp function with any header
comments. Returns source code with file path and 1-based line numbers. For
functions defined in C, returns a suggestion to call elisp-describe-function
tool instead.

Returns JSON with:
- source: Complete function definition including header comments
- file-path: Absolute path to source file or '<interactively defined>'
- start-line: Line number where definition starts (1-based)
- end-line: Line number where definition ends

Special handling:
- Function aliases: Returns the defalias form with docstring
- C functions: Returns is-c-function=true with suggestion message
- Interactive functions: Reconstructs defun from runtime representation
- Byte-compiled functions: Retrieves original source if available

Error cases:
- Non-existent functions return 'Function X is not found'
- Non-string input returns 'Invalid function name'

Use this tool when you need to:
- View or analyze function implementation
- Extract function source for modification
- Understand function structure with comments"
   :read-only t)
  (mcp-register-tool
   #'elisp-dev-mcp--describe-variable
   :id "elisp-describe-variable"
   :description
   "Get information about an Emacs Lisp variable without exposing its value.
Returns variable documentation and metadata from the current Emacs environment."
   :read-only t))

;;;###autoload
(defun elisp-dev-mcp-disable ()
  "Disable the Elisp development MCP tools."
  (mcp-unregister-tool "elisp-describe-function")
  (mcp-unregister-tool "elisp-get-function-definition")
  (mcp-unregister-tool "elisp-describe-variable"))

(provide 'elisp-dev-mcp)
;;; elisp-dev-mcp.el ends here
