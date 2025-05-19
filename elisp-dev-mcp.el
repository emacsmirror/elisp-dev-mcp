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

(defun elisp-dev-mcp--get-function-definition (function)
  "Get the source code definition for Emacs Lisp FUNCTION.

MCP Parameters:
  function - The name of the function to retrieve"
  (unless (stringp function)
    (mcp-tool-throw "Invalid function name"))
  (let ((sym (intern-soft function)))
    (unless (and sym (fboundp sym))
      (mcp-tool-throw (format "Function %s is not found" function)))

    ;; Special handling for C-implemented functions (subrp)
    (if (subrp (symbol-function sym))
        (json-encode
         `((is-c-function . t)
           (function-name . ,function)
           (message
            .
            ,(format
              "Function `%s` is implemented in C source code. \
Use elisp-describe-function tool to get its docstring."
              function))))

      ;; Regular Elisp function handling
      (let ((func-file (find-lisp-object-file-name sym 'defun)))
        (unless func-file
          (mcp-tool-throw
           (format "Could not determine file for function %s"
                   function)))
        (with-temp-buffer
          (insert-file-contents func-file)
          (goto-char (point-min))
          (let ((def-pos
                 (find-function-search-for-symbol sym nil func-file)))
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

              ;; Return the result
              (json-encode
               `((source . ,source)
                 (file-path . ,func-file)
                 (start-line . ,start-line)
                 (end-line . ,end-line))))))))))

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
