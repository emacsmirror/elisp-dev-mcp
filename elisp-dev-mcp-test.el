;;; elisp-dev-mcp-test.el --- Tests for elisp-dev-mcp -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the elisp-dev-mcp package.

;;; Code:

(require 'ert)
(require 'json)
(require 'mcp)
(require 'elisp-dev-mcp)

(defun elisp-dev-mcp-test--create-tool-request (tool-name arguments)
  "Create JSON-RPC request to call TOOL-NAME with ARGUMENTS."
  (json-encode
   `((jsonrpc . "2.0")
     (method . "tools/call")
     (id . 1)
     (params . ((name . ,tool-name) (arguments . ,arguments))))))

(defun elisp-dev-mcp-test--send-request (request)
  "Send REQUEST to the MCP server and return parsed response data."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol)
        (json-false :json-false)
        (json-null nil))
    (json-read-from-string
     (mcp-process-jsonrpc request))))

(ert-deftest elisp-dev-mcp-test-describe-function ()
  "Test that describe-function MCP handler works correctly."
  (unwind-protect
      (progn
        ;; Start the MCP server
        (mcp-start)
        (elisp-dev-mcp-enable)
        
        ;; Test with valid function
        (let* ((request 
                (elisp-dev-mcp-test--create-tool-request
                 "elisp-describe-function"
                 `((function . "defun"))))
               (response (elisp-dev-mcp-test--send-request request))
               (result (assoc-default 'result response)))
          
          ;; Verify response has expected structure
          (should result)
          (should (assoc-default 'content result))
          (should (= 1 (length (assoc-default 'content result))))
          ;; Verify isError flag is false for successful response
          (should (eq (assoc-default 'isError result) :json-false))
          (let ((text-item (aref (assoc-default 'content result) 0)))
            (should (string= "text" (assoc-default 'type text-item)))
            (should (stringp (assoc-default 'text text-item)))
            (should (string-match-p "defun" (assoc-default 'text text-item))))))
    
    ;; Clean up
    (elisp-dev-mcp-disable)
    (mcp-stop)))

(ert-deftest elisp-dev-mcp-test-describe-nonexistent-function ()
  "Test that describe-function MCP handler handles non-existent functions."
  (unwind-protect
      (progn
        ;; Start the MCP server
        (mcp-start)
        (elisp-dev-mcp-enable)
        
        ;; Test with non-existent function
        (let* ((request 
                (elisp-dev-mcp-test--create-tool-request
                 "elisp-describe-function"
                 `((function . "non-existent-function-xyz"))))
               (response (elisp-dev-mcp-test--send-request request))
               (result (assoc-default 'result response)))
          
          ;; Verify response has expected structure
          (should result)
          (should (assoc-default 'content result))
          (should (= 1 (length (assoc-default 'content result))))
          ;; Verify isError flag is true for error response
          (should (eq (assoc-default 'isError result) t))
          (let ((text-item (aref (assoc-default 'content result) 0)))
            (should (string= "text" (assoc-default 'type text-item)))
            (should (stringp (assoc-default 'text text-item)))
            (should (string-match-p "Function non-existent-function-xyz is void" 
                                  (assoc-default 'text text-item))))))
    
    ;; Clean up
    (elisp-dev-mcp-disable)
    (mcp-stop)))

(provide 'elisp-dev-mcp-test)
;;; elisp-dev-mcp-test.el ends here