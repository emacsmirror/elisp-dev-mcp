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
    (error
     (mcp-tool-throw (format "Error: %S" err)))))

;;;###autoload
(defun elisp-dev-mcp-enable ()
  "Enable the Elisp development MCP tools."
  (mcp-register-tool
   #'elisp-dev-mcp--describe-function
   :id "elisp-describe-function"
   :description "Get full documentation for an Emacs Lisp function"
   :read-only t))

;;;###autoload
(defun elisp-dev-mcp-disable ()
  "Disable the Elisp development MCP tools."
  (mcp-unregister-tool "elisp-describe-function"))

(provide 'elisp-dev-mcp)
;;; elisp-dev-mcp.el ends here