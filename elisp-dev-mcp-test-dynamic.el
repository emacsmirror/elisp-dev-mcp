;;; elisp-dev-mcp-test-dynamic.el --- Test fixtures for dynamic binding -*- lexical-binding: nil -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis
;; Version: 0.1.0
;; Keywords: tools, development
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; Test fixture functions for dynamic binding context testing.
;; This file intentionally uses dynamic binding to create
;; functions with dynamic binding behavior for testing purposes.

;;; Code:

;; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-test-dynamic--with-header-comment (arg1 arg2)
  "Sample function with a header comment in dynamic binding context.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2."
  (+ arg1 arg2))

(provide 'elisp-dev-mcp-test-dynamic)

;; Local Variables:
;; package-lint-main-file: "elisp-dev-mcp.el"
;; End:

;;; elisp-dev-mcp-test-dynamic.el ends here
