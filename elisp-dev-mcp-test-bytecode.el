;;; elisp-dev-mcp-test-bytecode.el --- Bytecode test functions -*- lexical-binding: t -*-

;; This file contains functions that will be byte-compiled for testing.

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
;;; elisp-dev-mcp-test-bytecode.el ends here