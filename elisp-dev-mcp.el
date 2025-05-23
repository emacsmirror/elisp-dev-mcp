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
              (let*
                  ((args (help-function-arglist sym t))
                   (doc (or (documentation sym) ""))
                   (body
                    (and
                     (functionp fn)
                     (cond
                      ;; Emacs 30+ interpreted-function objects
                      ((eq (type-of fn) 'interpreted-function)
                       ;; Extract body from interpreted-function
                       ;; Format: #[args body env bytecode doc]
                       (aref fn 1))
                      ;; Emacs 29 and earlier cons-based functions
                      ((consp fn)
                       (nthcdr
                        (if doc
                            3
                          2)
                        fn))
                      ;; Fallback for other types
                      (t
                       (mcp-tool-throw
                        (format
                         "Don't know how to extract body from function type: %s"
                         (type-of fn)))))))
                   ;; Format args list as a string
                   (args-str
                    (if args
                        (format " %s" (prin1-to-string args))
                      " ()"))
                   ;; Use pp for prettier formatting of the decompiled function
                   (func-def
                    (with-temp-buffer
                      ;; Start the defun with proper args
                      (insert "(defun " function args-str)
                      ;; Add the docstring
                      (when doc
                        (insert "\n  " (prin1-to-string doc)))
                      ;; Add the body with proper formatting
                      (if body
                          (dolist (expr body)
                            (insert "\n  " (pp-to-string expr)))
                        ;; Fallback if body extraction failed
                        (insert "\n  'body"))
                      ;; Close the defun
                      (insert ")")
                      (buffer-string))))
                (json-encode
                 `((source . ,func-def)
                   (file-path . "<interactively defined>")
                   (start-line . 1)
                   (end-line . 1)))))

          ;; Functions with source file
          (with-temp-buffer
            (insert-file-contents func-file)
            (goto-char (point-min))
            (let ((def-pos
                   (find-function-search-for-symbol
                    sym nil func-file)))
              (unless def-pos
                (mcp-tool-throw
                 (format "Could not locate definition for %s"
                         function)))
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
                      (buffer-substring-no-properties
                       start-point (point)))

                ;; Return the result, with special handling for aliases
                (if is-alias
                    (elisp-dev-mcp--process-alias-source
                     source
                     function
                     aliased-to
                     func-file
                     start-line
                     end-line)
                  (json-encode
                   `((source . ,source)
                     (file-path . ,func-file)
                     (start-line . ,start-line)
                     (end-line . ,end-line))))))))))))

;;;###autoload
(defun elisp-dev-mcp-enable ()
  "Enable the Elisp development MCP tools."
  (mcp-register-tool
   #'elisp-dev-mcp--describe-function
   :id "elisp-describe-function"
   :description
   "Get documentation for an Emacs Lisp function or check if it exists. Returns
function documentation from the current running Emacs environment, including all
currently loaded packages and libraries."
   :read-only t)
  (mcp-register-tool
   #'elisp-dev-mcp--get-function-definition
   :id "elisp-get-function-definition"
   :description
   "Get the source code definition of an Emacs Lisp function with any header
comments. Returns source code with file path and 1-based line numbers. For
functions defined in C, returns a suggestion to call elisp-describe-function
tool instead."
   :read-only t))

;;;###autoload
(defun elisp-dev-mcp-disable ()
  "Disable the Elisp development MCP tools."
  (mcp-unregister-tool "elisp-describe-function")
  (mcp-unregister-tool "elisp-get-function-definition"))

(provide 'elisp-dev-mcp)
;;; elisp-dev-mcp.el ends here
