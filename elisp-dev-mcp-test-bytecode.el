;;; elisp-dev-mcp-test-bytecode.el --- Bytecode test functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: tools, development
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; Test fixture functions that will be byte-compiled for testing the
;; elisp-dev-mcp package.  These functions are used to verify that MCP
;; tools work correctly with byte-compiled code.  Some functions
;; intentionally lack documentation for testing purposes.

;;; Code:

;; Header comment for byte-compiled function
;; This should be preserved in the function definition
(defun elisp-dev-mcp-test-bytecode--with-header (x y)
  "A byte-compiled function with header comment.
X and Y are numbers to be added."
  (+ x y))

(defun elisp-dev-mcp-test-bytecode--no-docstring (a b)
  (* a b))

(defun elisp-dev-mcp-test-bytecode--empty-docstring (n)
  ""
  (* n 2))

(provide 'elisp-dev-mcp-test-bytecode)

;; Local Variables:
;; elisp-lint-ignored-validators: ("checkdoc")
;; End:

;;; elisp-dev-mcp-test-bytecode.el ends here
