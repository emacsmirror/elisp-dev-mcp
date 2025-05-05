;;; elisp-dev-mcp-test.el --- Tests for elisp-dev-mcp -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Laurynas Biveinis
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: tools, development
;; URL: https://github.com/laurynas-biveinis/elisp-dev-mcp

;;; Commentary:

;; Tests for the elisp-dev-mcp package.

;;; Code:

(require 'ert)
(require 'json)
(require 'mcp)
(require 'elisp-dev-mcp)

;;; Test functions used for function definition retrieval tests. Should be the
;;; first code in the file to keep the test line numbers stable.

;; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-test--with-header-comment (arg1 arg2)
  "Sample function with a header comment.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2."
  (+ arg1 arg2))

;; This comment is separated by an empty line from the next function and should
;; not be returned together with it.

(defun elisp-dev-mcp-test--without-header-comment (value)
  "Simple function without a header comment.
VALUE is multiplied by 2."
  (* value 2))

(defmacro elisp-dev-mcp-test-with-server (&rest body)
  "Execute BODY with running MCP server and elisp-dev-mcp enabled."
  (declare (indent defun) (debug t))
  `(unwind-protect
       (progn
         (mcp-start)
         (elisp-dev-mcp-enable)
         ,@body)
     (elisp-dev-mcp-disable)
     (mcp-stop)))

;;; Helpers to create JSON requests

(defun elisp-dev-mcp-test--create-tool-req (tool-name arguments)
  "Create JSON-RPC request to call TOOL-NAME with ARGUMENTS."
  (json-encode
   `((jsonrpc . "2.0")
     (method . "tools/call")
     (id . 1)
     (params . ((name . ,tool-name) (arguments . ,arguments))))))

;;; Helpers to create tool call requests

(defun elisp-dev-mcp-test--describe-req (function-name)
  "Create a request to call `elisp-describe-function` with FUNCTION-NAME."
  (elisp-dev-mcp-test--create-tool-req
   "elisp-describe-function" `((function . ,function-name))))

(defun elisp-dev-mcp-test--definition-req (function-name)
  "Create a request to call `elisp-get-function-definition` with FUNCTION-NAME."
  (elisp-dev-mcp-test--create-tool-req
   "elisp-get-function-definition" `((function . ,function-name))))

(defun elisp-dev-mcp-test--apropos-req (pattern)
  "Create a request to call `elisp-apropos` with PATTERN."
  (elisp-dev-mcp-test--create-tool-req
   "elisp-apropos" `((pattern . ,pattern))))

(defun elisp-dev-mcp-test--send-req (request)
  "Send REQUEST to the MCP server and return parsed response data."
  (json-read-from-string (mcp-process-jsonrpc request)))

;;; Helpers to analyze response JSON

(defun elisp-dev-mcp-test--check-resp-get-text (response is-error)
  "Check that RESPONSE has expected structure and extract text.
If IS-ERROR is non-nil, checks it's an error response, otherwise a success.
Returns the text content when validation passes."
  (let ((result (assoc-default 'result response)))
    (should result)
    (should (assoc-default 'content result))
    (should (= 1 (length (assoc-default 'content result))))
    (should
     (eq
      (assoc-default 'isError result)
      (if is-error
          t
        :json-false)))
    (let ((text-item (aref (assoc-default 'content result) 0)))
      (should (string= "text" (assoc-default 'type text-item)))
      (should (stringp (assoc-default 'text text-item)))
      (assoc-default 'text text-item))))

(defun elisp-dev-mcp-test--verify-error-resp (response error-pattern)
  "Verify that RESPONSE is an error response matching ERROR-PATTERN."
  (should
   (string-match-p
    error-pattern
    (elisp-dev-mcp-test--check-resp-get-text response t))))

;;; Tests

(ert-deftest elisp-dev-mcp-test-describe-function ()
  "Test that `describe-function' MCP handler works correctly."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--describe-req "defun"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      (should (string-match-p "defun" text)))))

(ert-deftest elisp-dev-mcp-test-describe-nonexistent-function ()
  "Test that `describe-function' MCP handler handles non-existent functions."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--describe-req
             "non-existent-function-xyz"))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Function non-existent-function-xyz is void"))))

(ert-deftest elisp-dev-mcp-test-describe-invalid-function-type ()
  "Test that `describe-function' handles non-string function names properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--describe-req 123))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp resp "Error:"))))

(defun elisp-dev-mcp-test--find-tools-in-tools-list ()
  "Get the current list of MCP tools as returned by the server.
Returns a list of our registered tools in the order:
\(describe-function-tool get-definition-tool apropos-tool).
Any tool not found will be nil in the list."
  (let* ((req (mcp-create-tools-list-request))
         (resp (elisp-dev-mcp-test--send-req req))
         (result (assoc-default 'result resp))
         (tools (assoc-default 'tools result))
         (describe-function-tool nil)
         (get-definition-tool nil)
         (apropos-tool nil))

    ;; Find our tools in the list
    (dotimes (i (length tools))
      (let* ((tool (aref tools i))
             (name (assoc-default 'name tool)))
        (cond
         ((string= name "elisp-describe-function")
          (setq describe-function-tool tool))
         ((string= name "elisp-get-function-definition")
          (setq get-definition-tool tool))
         ((string= name "elisp-apropos")
          (setq apropos-tool tool)))))

    (list describe-function-tool get-definition-tool apropos-tool)))

(ert-deftest elisp-dev-mcp-test-tools-registration-and-unregistration
    ()
  "Test tools registration, annotations, and proper unregistration."
  ;; First test that tools are properly registered with annotations
  (unwind-protect
      (progn
        (mcp-start)
        (elisp-dev-mcp-enable)

        ;; Check tool registration
        (let* ((tools (elisp-dev-mcp-test--find-tools-in-tools-list))
               (describe-function-tool (nth 0 tools))
               (get-definition-tool (nth 1 tools))
               (apropos-tool (nth 2 tools)))

          ;; Verify all tools are registered
          (should describe-function-tool)
          (should get-definition-tool)
          (should apropos-tool)

          ;; Verify read-only annotations for all tools
          (dolist (tool
                   (list
                    describe-function-tool
                    get-definition-tool
                    apropos-tool))
            (let ((annotations (assoc-default 'annotations tool)))
              (should annotations)
              (should
               (eq (assoc-default 'readOnlyHint annotations) t))))

          ;; Now test unregistration
          (elisp-dev-mcp-disable)

          ;; Get updated tools list and verify tools are unregistered
          (let ((tools
                 (elisp-dev-mcp-test--find-tools-in-tools-list)))
            (should-not (nth 0 tools)) ;; describe-function should be gone
            (should-not (nth 1 tools)) ;; get-definition should be gone
            (should-not (nth 2 tools)) ;; apropos should be gone
            )))

    ;; Clean up
    (elisp-dev-mcp-disable)
    (mcp-stop)))

(ert-deftest elisp-dev-mcp-test-get-function-definition ()
  "Test that `elisp-get-function-definition' MCP handler works correctly."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--definition-req
             "elisp-dev-mcp-test--without-header-comment"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed-resp (json-read-from-string text))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp))
           (start-line (assoc-default 'start-line parsed-resp))
           (end-line (assoc-default 'end-line parsed-resp)))

      (should
       (string=
        (file-name-nondirectory file-path) "elisp-dev-mcp-test.el"))
      (should (= start-line 40))
      (should (= end-line 43))
      (should
       (string=
        source
        "(defun elisp-dev-mcp-test--without-header-comment (value)
  \"Simple function without a header comment.
VALUE is multiplied by 2.\"
  (* value 2))")))))

(ert-deftest elisp-dev-mcp-test-get-nonexistent-function-definition ()
  "Test that `elisp-get-function-definition' handles non-existent functions."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--definition-req
             "non-existent-function-xyz"))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Function non-existent-function-xyz is not found"))))

(ert-deftest elisp-dev-mcp-test-get-function-definition-invalid-type
    ()
  "Test that `elisp-get-function-definition' handles non-string names."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--definition-req 123))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Invalid function name"))))

(ert-deftest elisp-dev-mcp-test-get-c-function-definition ()
  "Test that `elisp-get-function-definition' handles C-implemented functions."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--definition-req "car"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed-resp (json-read-from-string text))
           (is-c-function (assoc-default 'is-c-function parsed-resp))
           (function-name (assoc-default 'function-name parsed-resp))
           (message (assoc-default 'message parsed-resp)))

      (should (eq is-c-function t))
      (should (string= function-name "car"))
      (should (stringp message))
      (should (string-match-p "C source code" message))
      (should (string-match-p "car" message))
      (should (string-match-p "elisp-describe-function" message))
      (should (string-match-p "docstring" message)))))

(ert-deftest elisp-dev-mcp-test-get-function-with-header-comment ()
  "Test that `elisp-get-function-definition' includes header comments."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--definition-req
             "elisp-dev-mcp-test--with-header-comment"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed-resp (json-read-from-string text))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp))
           (start-line (assoc-default 'start-line parsed-resp))
           (end-line (assoc-default 'end-line parsed-resp)))

      (should
       (string=
        (file-name-nondirectory file-path) "elisp-dev-mcp-test.el"))
      (should (= start-line 25))
      (should (= end-line 35))
      (should
       (string=
        source
        ";; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-test--with-header-comment (arg1 arg2)
  \"Sample function with a header comment.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2.\"
  (+ arg1 arg2))")))))

(ert-deftest elisp-dev-mcp-test-apropos ()
  "Test that `elisp-apropos' MCP handler works correctly."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--apropos-req "buffer-"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed-resp (json-read-from-string text)))

      ;; Verify we have matches
      (should parsed-resp)
      (should (arrayp parsed-resp))
      (should (> (length parsed-resp) 0))

      ;; Verify structure of at least one match
      (let ((first-match (aref parsed-resp 0)))
        (should (stringp (assoc-default 'name first-match)))
        (should (stringp (assoc-default 'type first-match)))
        (should (stringp (assoc-default 'description first-match)))
        (should
         (or (null (assoc-default 'package first-match))
             (stringp (assoc-default 'package first-match))))))))

(ert-deftest elisp-dev-mcp-test-apropos-no-matches ()
  "Test that `elisp-apropos' MCP handler handles patterns with no matches."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--apropos-req
             "xyzzy-not-a-real-function-name"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed-resp (json-read-from-string text)))

      ;; Verify we have an empty array
      (should parsed-resp)
      (should (arrayp parsed-resp))
      (should (= (length parsed-resp) 0)))))

(ert-deftest elisp-dev-mcp-test-apropos-invalid-pattern-type ()
  "Test that `elisp-apropos' handles non-string pattern properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--apropos-req 123))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Invalid pattern"))))

(provide 'elisp-dev-mcp-test)
;;; elisp-dev-mcp-test.el ends here
