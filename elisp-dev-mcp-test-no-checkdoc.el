;;; elisp-dev-mcp-test-no-checkdoc.el --- Test fixtures exempt from checkdoc -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; Test fixture functions that are exempt from checkdoc validation.
;; This file contains functions that intentionally violate documentation
;; requirements for testing purposes.

;;; Code:

(defun elisp-dev-mcp-test-no-checkdoc--no-docstring (x y)
  (+ x y))

(provide 'elisp-dev-mcp-test-no-checkdoc)

;; Local Variables:
;; elisp-lint-ignored-validators: ("checkdoc")
;; End:

;;; elisp-dev-mcp-test-no-checkdoc.el ends here
