;;; elisp-dev-mcp-test.el --- Tests for elisp-dev-mcp -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laurynas Biveinis

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
(require 'mcp-server-lib)
(require 'mcp-server-lib-commands)
(require 'elisp-dev-mcp)
(require 'elisp-dev-mcp-test-no-checkdoc)

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

;; An inline function for testing describe-function with defsubst
(defsubst elisp-dev-mcp-test--inline-function (x)
  "An inline function for testing purposes.
X is the input value that will be doubled."
  (* 2 x))

(defalias
  'elisp-dev-mcp-test--aliased-function
  #'elisp-dev-mcp-test--with-header-comment
  "This is an alias for elisp-dev-mcp-test--with-header-comment.")

(defmacro elisp-dev-mcp-test-with-server (&rest body)
  "Execute BODY with running MCP server and elisp-dev-mcp enabled."
  (declare (indent defun) (debug t))
  `(unwind-protect
       (progn
         (mcp-server-lib-start)
         (elisp-dev-mcp-enable)
         ,@body)
     (elisp-dev-mcp-disable)
     (mcp-server-lib-stop)))

;;; Test variables

(defvar elisp-dev-mcp-test--undocumented-var)

(defcustom elisp-dev-mcp-test--custom-var "default"
  "A custom variable for testing."
  :type 'string
  :group 'elisp-dev-mcp)

(defcustom elisp-dev-mcp-test--custom-choice-var 'option1
  "A custom variable with choice type for testing."
  :type
  '(choice
    (const :tag "Option 1" option1)
    (const :tag "Option 2" option2)
    (string :tag "Custom string"))
  :group 'elisp-dev-mcp)

(defvaralias 'elisp-dev-mcp-test--a 'elisp-dev-mcp-test--b "x")

(defvar elisp-dev-mcp-test--b "test-value"
  "A regular variable for alias testing.")

(defvar elisp-dev-mcp-test--obsolete-var "old-value"
  "An obsolete variable for testing.")
(make-obsolete-variable
 'elisp-dev-mcp-test--obsolete-var 'elisp-dev-mcp-test--new-var "1.0")

(defvar elisp-dev-mcp-test--unbound-documented-var nil
  "A documented variable that will be unbound for testing.")
(makunbound 'elisp-dev-mcp-test--unbound-documented-var)

;;; Helpers to create JSON requests

(defun elisp-dev-mcp-test--check-closure-text (text)
  "Check TEXT containing expected closure description for current Emacs."
  (if (>= emacs-major-version 30)
      (should (string-match-p "interpreted-function" text))
    (should (string-match-p "Lisp closure" text))))

(defun elisp-dev-mcp-test--check-dynamic-text (text)
  "Check TEXT containing expected dynamic binding function description."
  (if (>= emacs-major-version 30)
      (should (string-match-p "interpreted-function" text))
    (should (string-match-p "Lisp function" text))))

(defun elisp-dev-mcp-test--check-empty-docstring (source)
  "Check SOURCE for empty docstring based on Emacs version."
  (if (>= emacs-major-version 30)
      ;; Emacs 30+ strips empty docstrings
      (should-not (string-match-p "\"\"" source))
    ;; Older versions preserve empty docstrings
    (should (string-match-p "\"\"" source))))

;;; Helpers to create tool call requests

(defun elisp-dev-mcp-test--describe-req (function-name)
  "Create a request to call `elisp-describe-function` with FUNCTION-NAME."
  (mcp-server-lib-create-tools-call-request
   "elisp-describe-function" 1 `((function . ,function-name))))

(defun elisp-dev-mcp-test--definition-req (function-name)
  "Create a request to call `elisp-get-function-definition` with FUNCTION-NAME."
  (mcp-server-lib-create-tools-call-request
   "elisp-get-function-definition" 1 `((function . ,function-name))))

(defun elisp-dev-mcp-test--send-req (request)
  "Send REQUEST to the MCP server and return parsed response data."
  (json-read-from-string (mcp-server-lib-process-jsonrpc request)))

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

(defun elisp-dev-mcp-test--get-definition-response-data
    (function-name)
  "Get function definition response data for FUNCTION-NAME.
Returns the parsed JSON response object."
  (let* ((req (elisp-dev-mcp-test--definition-req function-name))
         (resp (elisp-dev-mcp-test--send-req req))
         (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
    (json-read-from-string text)))

(defun elisp-dev-mcp-test--verify-definition-in-test-file
    (function-name
     expected-start-line expected-end-line expected-source)
  "Verify function definition for FUNCTION-NAME is in test file.
Checks that the function is defined in elisp-dev-mcp-test.el with
EXPECTED-START-LINE, EXPECTED-END-LINE and EXPECTED-SOURCE."
  (let* ((parsed-resp
          (elisp-dev-mcp-test--get-definition-response-data
           function-name))
         (source (assoc-default 'source parsed-resp))
         (file-path (assoc-default 'file-path parsed-resp))
         (start-line (assoc-default 'start-line parsed-resp))
         (end-line (assoc-default 'end-line parsed-resp)))

    (should
     (string=
      (file-name-nondirectory file-path) "elisp-dev-mcp-test.el"))
    (should (= start-line expected-start-line))
    (should (= end-line expected-end-line))
    (should (string= source expected-source))))

(defun elisp-dev-mcp-test--verify-interactive-definition
    (function-name expected-patterns)
  "Verify interactive function definition for FUNCTION-NAME.
EXPECTED-PATTERNS is a list of regex patterns that should match in the source."
  (let* ((parsed-resp
          (elisp-dev-mcp-test--get-definition-response-data
           function-name))
         (source (assoc-default 'source parsed-resp))
         (file-path (assoc-default 'file-path parsed-resp)))

    ;; Basic verifications
    (should source)
    (should (string-match-p "defun" source))
    (should (string-match-p function-name source))
    (should (string-match-p "interactive" file-path))

    ;; Check all expected patterns
    (dolist (pattern expected-patterns)
      (should (string-match-p pattern source)))))

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
      (elisp-dev-mcp-test--verify-error-resp
       resp "Invalid function name"))))

(ert-deftest elisp-dev-mcp-test-describe-empty-string-function ()
  "Test that `describe-function' MCP handler handles empty string properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--describe-req ""))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Empty function name"))))

(ert-deftest elisp-dev-mcp-test-describe-variable-as-function ()
  "Test that `describe-function' MCP handler handles variable names properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--describe-req "user-emacs-directory"))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Function user-emacs-directory is void"))))

(ert-deftest elisp-dev-mcp-test-describe-macro ()
  "Test that `describe-function' MCP handler works correctly with macros."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--describe-req "when"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      (should (string-match-p "when" text))
      (should (string-match-p "macro" text)))))

(ert-deftest elisp-dev-mcp-test-describe-inline-function ()
  "Test that `describe-function' MCP handler works with inline functions."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--describe-req
             "elisp-dev-mcp-test--inline-function"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      (should
       (string-match-p "elisp-dev-mcp-test--inline-function" text))
      (should (string-match-p "inline" text)))))

(ert-deftest elisp-dev-mcp-test-describe-regular-function ()
  "Test that `describe-function' MCP handler works with regular defun."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--describe-req
             "elisp-dev-mcp-test--with-header-comment"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-test--with-header-comment" text))
      ;; Should show it's in the test file
      (should (string-match-p "elisp-dev-mcp-test\\.el" text))
      ;; Should show the function signature with arguments
      (should
       (string-match-p
        "elisp-dev-mcp-test--with-header-comment ARG1 ARG2)" text))
      ;; Should include the docstring
      (should
       (string-match-p "Sample function with a header comment" text))
      ;; Should include parameter documentation
      (should (string-match-p "ARG1 is the first argument" text)))))

(ert-deftest elisp-dev-mcp-test-describe-function-no-docstring ()
  "Test `describe-function' MCP handler with undocumented functions."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--describe-req
             "elisp-dev-mcp-test-no-checkdoc--no-docstring"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-test-no-checkdoc--no-docstring" text))
      (elisp-dev-mcp-test--check-closure-text text)
      ;; Should show argument list (uppercase) in the signature
      (should
       (string-match-p
        "elisp-dev-mcp-test-no-checkdoc--no-docstring X Y)" text))
      ;; Should indicate lack of documentation
      (should (string-match-p "Not documented" text)))))

(ert-deftest elisp-dev-mcp-test-describe-function-empty-docstring ()
  "Test `describe-function' MCP handler with empty docstring functions."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--describe-req
             "elisp-dev-mcp-test-no-checkdoc--empty-docstring"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-test-no-checkdoc--empty-docstring" text))
      (elisp-dev-mcp-test--check-closure-text text)
      ;; Should show argument list (uppercase) in the signature
      (should
       (string-match-p
        "elisp-dev-mcp-test-no-checkdoc--empty-docstring X Y)" text))
      ;; Should show it's in the test file
      (should
       (string-match-p "elisp-dev-mcp-test-no-checkdoc\\.el" text))
      ;; Should show the function signature with arguments
      (should
       (string-match-p
        "elisp-dev-mcp-test-no-checkdoc--empty-docstring X Y)"
        text)))))

(defun elisp-dev-mcp-test--find-tools-in-tools-list ()
  "Get the current list of MCP tools as returned by the server.
Returns a list of our registered tools in the order:
\(describe-function-tool get-definition-tool describe-variable-tool
info-lookup-tool read-source-file-tool).
Any tool not found will be nil in the list."
  (let* ((req (mcp-server-lib-create-tools-list-request))
         (resp (elisp-dev-mcp-test--send-req req))
         (result (assoc-default 'result resp))
         (tools (assoc-default 'tools result))
         (describe-function-tool nil)
         (get-definition-tool nil)
         (describe-variable-tool nil)
         (info-lookup-tool nil)
         (read-source-file-tool nil))

    ;; Find our tools in the list
    (dotimes (i (length tools))
      (let* ((tool (aref tools i))
             (name (assoc-default 'name tool)))
        (cond
         ((string= name "elisp-describe-function")
          (setq describe-function-tool tool))
         ((string= name "elisp-get-function-definition")
          (setq get-definition-tool tool))
         ((string= name "elisp-describe-variable")
          (setq describe-variable-tool tool))
         ((string= name "elisp-info-lookup-symbol")
          (setq info-lookup-tool tool))
         ((string= name "elisp-read-source-file")
          (setq read-source-file-tool tool)))))

    (list
     describe-function-tool
     get-definition-tool
     describe-variable-tool
     info-lookup-tool
     read-source-file-tool)))

(ert-deftest elisp-dev-mcp-test-tools-registration-and-unregistration
    ()
  "Test tools registration, annotations, and proper unregistration."
  ;; First test that tools are properly registered with annotations
  (unwind-protect
      (progn
        (mcp-server-lib-start)
        (elisp-dev-mcp-enable)

        ;; Check tool registration
        (let* ((tools (elisp-dev-mcp-test--find-tools-in-tools-list))
               (describe-function-tool (nth 0 tools))
               (get-definition-tool (nth 1 tools))
               (describe-variable-tool (nth 2 tools))
               (info-lookup-tool (nth 3 tools))
               (read-source-file-tool (nth 4 tools)))

          ;; Verify all tools are registered
          (should describe-function-tool)
          (should get-definition-tool)
          (should describe-variable-tool)
          (should info-lookup-tool)
          (should read-source-file-tool)

          ;; Verify read-only annotations for all tools
          (dolist (tool
                   (list
                    describe-function-tool
                    get-definition-tool
                    describe-variable-tool
                    info-lookup-tool
                    read-source-file-tool))
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
            (should-not (nth 2 tools)) ;; describe-variable should be gone
            (should-not (nth 3 tools)) ;; info-lookup should be gone
            (should-not (nth 4 tools)) ;; read-source-file should be gone
            )))

    ;; Clean up
    (elisp-dev-mcp-disable)
    (mcp-server-lib-stop)))

(ert-deftest elisp-dev-mcp-test-get-function-definition ()
  "Test that `elisp-get-function-definition' MCP handler works correctly."
  (elisp-dev-mcp-test-with-server
    (elisp-dev-mcp-test--verify-definition-in-test-file
     "elisp-dev-mcp-test--without-header-comment" 42 45
     "(defun elisp-dev-mcp-test--without-header-comment (value)
  \"Simple function without a header comment.
VALUE is multiplied by 2.\"
  (* value 2))")))

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
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data "car"))
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
    (elisp-dev-mcp-test--verify-definition-in-test-file
     "elisp-dev-mcp-test--with-header-comment" 27 37
     ";; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-test--with-header-comment (arg1 arg2)
  \"Sample function with a header comment.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2.\"
  (+ arg1 arg2))")))

(ert-deftest elisp-dev-mcp-test-get-interactively-defined-function ()
  "Test interactively defined functions with get-function-definition."
  (elisp-dev-mcp-test-with-server
    ;; Define a function interactively (not from a file)
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-function")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function
            (eval
             `(defun ,sym ()
                "An interactively defined function with no file location."
                'interactive-result))

            ;; Now try to get its definition
            (elisp-dev-mcp-test--verify-interactive-definition
             test-function-name '()))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-get-interactive-function-with-args ()
  "Test interactively defined functions with complex args."
  (elisp-dev-mcp-test-with-server
    ;; Define a function interactively with special parameter specifiers
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-complex-args")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function
            (eval
             `(defun ,sym (a b &optional c &rest d)
                "A function with complex argument list.
A and B are required arguments.
C is optional.
D captures remaining arguments."
                (list a b c d)))

            ;; Now try to get its definition
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     test-function-name))
                   (source (assoc-default 'source parsed-resp))
                   (file-path (assoc-default 'file-path parsed-resp)))

              ;; Verify that we get a definition back with correct argument list
              (should source)
              (should (string-match-p "defun" source))
              (should (string-match-p test-function-name source))
              (should
               (string-match-p "(a b &optional c &rest d)" source))
              (should (string-match-p "list a b c d" source))
              (should (string-match-p "A and B are required" source))
              (should (string-match-p "interactive" file-path))))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-describe-function-alias ()
  "Test that `describe-function' MCP handler works with function aliases."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (elisp-dev-mcp-test--describe-req
             "elisp-dev-mcp-test--aliased-function"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))

      ;; Should indicate it's an alias
      (should (string-match-p "alias" text))

      ;; Should mention the original function
      (should
       (string-match-p
        "elisp-dev-mcp-test--with-header-comment" text))

      ;; Should include the custom docstring of the alias
      (should (string-match-p "This is an alias for" text)))))

(ert-deftest elisp-dev-mcp-test-get-function-definition-alias ()
  "Test that `elisp-get-function-definition' works with function aliases."
  (elisp-dev-mcp-test-with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-test--aliased-function"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp)))


      ;; The source should include a complete defalias form with all components:
      ;; 1. The defalias keyword
      (should (string-match-p "defalias" source))
      ;; 2. The alias function name (quoted)
      (should
       (string-match-p
        "'elisp-dev-mcp-test--aliased-function" source))
      ;; 3. The target function name (quoted with hash)
      (should
       (string-match-p
        "#'elisp-dev-mcp-test--with-header-comment" source))
      ;; 4. The docstring (important component)
      (should (string-match-p "\"This is an alias for" source))

      ;; Verify file path is correct
      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-test.el")))))

(ert-deftest elisp-dev-mcp-test-get-special-form-definition ()
  "Test that `elisp-get-function-definition' handles special forms correctly."
  (elisp-dev-mcp-test-with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data "if"))
           (is-c-function (assoc-default 'is-c-function parsed-resp))
           (function-name (assoc-default 'function-name parsed-resp))
           (message (assoc-default 'message parsed-resp)))

      (should (eq is-c-function t))
      (should (string= function-name "if"))
      (should (stringp message))
      (should (string-match-p "C source code" message))
      (should (string-match-p "if" message))
      (should (string-match-p "elisp-describe-function" message))
      (should (string-match-p "docstring" message)))))

(ert-deftest elisp-dev-mcp-test-get-empty-string-function-definition
    ()
  "Test that `elisp-get-function-definition' handles empty string properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--definition-req ""))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Empty function name"))))

(ert-deftest elisp-dev-mcp-test-get-variable-as-function-definition ()
  "Test that `elisp-get-function-definition' handles variable names properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req (elisp-dev-mcp-test--definition-req "load-path"))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Function load-path is not found"))))

(ert-deftest elisp-dev-mcp-test-get-function-definition-no-docstring
    ()
  "Test `elisp-get-function-definition' with undocumented functions."
  (elisp-dev-mcp-test-with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-test-no-checkdoc--no-docstring"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp)))

      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-test-no-checkdoc.el"))
      (should
       (string=
        source
        "(defun elisp-dev-mcp-test-no-checkdoc--no-docstring (x y)
  (+ x y))")))))

(ert-deftest
    elisp-dev-mcp-test-get-function-definition-empty-docstring
    ()
  "Test `elisp-get-function-definition' with empty docstring functions."
  (elisp-dev-mcp-test-with-server
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-test-no-checkdoc--empty-docstring"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp)))

      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-test-no-checkdoc.el"))
      (should
       (string=
        source
        "(defun elisp-dev-mcp-test-no-checkdoc--empty-docstring (x y)
  \"\"
  (+ x y))")))))

(ert-deftest
    elisp-dev-mcp-test-get-interactive-function-definition-no-docstring
    ()
  "Test get-function-definition for dynamically defined function w/o docstring."
  (elisp-dev-mcp-test-with-server
    ;; Define a function interactively without a docstring
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-no-docstring")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function without docstring
            (eval
             `(defun ,sym (a b)
                (+ a b)))

            ;; Now try to get its definition and verify specific patterns
            (elisp-dev-mcp-test--verify-interactive-definition
             test-function-name '("(a b)" "(\\+ a b)"))

            ;; Additional check: Should not contain a docstring
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     test-function-name))
                   (source (assoc-default 'source parsed-resp)))
              (should-not (string-match-p "\"" source))))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest
    elisp-dev-mcp-test-get-interactive-function-definition-empty-docstring
    ()
  "Test get-function-definition for dynamically defined function w/ empty doc."
  (elisp-dev-mcp-test-with-server
    ;; Define a function interactively with an empty docstring
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-empty-docstring")
           (sym (intern test-function-name)))
      (unwind-protect
          (progn
            ;; Define the test function with empty docstring
            (eval
             `(defun ,sym (a b)
                ""
                (+ a b)))

            ;; Now try to get its definition and verify specific patterns
            (elisp-dev-mcp-test--verify-interactive-definition
             test-function-name '("(a b)" "(\\+ a b)"))

            ;; Additional check: Should contain an empty docstring
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     test-function-name))
                   (source (assoc-default 'source parsed-resp)))
              (elisp-dev-mcp-test--check-empty-docstring source)))

        ;; Clean up - remove the test function
        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-describe-dynamic-binding-function ()
  "Test `describe-function' with functions from lexical-binding: nil files."
  (elisp-dev-mcp-test-with-server
    ;; Load the dynamic binding test file
    (require 'elisp-dev-mcp-test-dynamic)

    ;; Test describe-function with dynamic binding function
    (let* ((req
            (elisp-dev-mcp-test--describe-req
             "elisp-dev-mcp-test-dynamic--with-header-comment"))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))

      ;; Should contain the function name
      (should
       (string-match-p
        "elisp-dev-mcp-test-dynamic--with-header-comment" text))
      ;; Should show it's in the dynamic binding test file
      (should (string-match-p "elisp-dev-mcp-test-dynamic\\.el" text))
      ;; Should show as "Lisp function" not "Lisp closure"
      (elisp-dev-mcp-test--check-dynamic-text text)
      (should-not (string-match-p "closure" text))
      ;; Should include the docstring
      (should
       (string-match-p "Sample function with a header comment" text))
      ;; Should include parameter documentation
      (should (string-match-p "ARG1 is the first argument" text)))))

(ert-deftest
    elisp-dev-mcp-test-get-dynamic-binding-function-definition
    ()
  "Test 'get-function-definition' with lexical-binding: nil functions."
  (elisp-dev-mcp-test-with-server
    ;; Load the dynamic binding test file
    (require 'elisp-dev-mcp-test-dynamic)

    ;; Test get-function-definition with dynamic binding function
    (let* ((parsed-resp
            (elisp-dev-mcp-test--get-definition-response-data
             "elisp-dev-mcp-test-dynamic--with-header-comment"))
           (source (assoc-default 'source parsed-resp))
           (file-path (assoc-default 'file-path parsed-resp))
           (start-line (assoc-default 'start-line parsed-resp))
           (end-line (assoc-default 'end-line parsed-resp)))

      ;; Verify file path is the dynamic binding file
      (should
       (string=
        (file-name-nondirectory file-path)
        "elisp-dev-mcp-test-dynamic.el"))
      (should (= start-line 19))
      (should (= end-line 29))
      (should
       (string=
        source
        ";; This is a header comment that should be included
;; when extracting the function definition
(defun elisp-dev-mcp-test-dynamic--with-header-comment (arg1 arg2)
  \"Sample function with a header comment in dynamic binding context.
Demonstrates comment extraction capabilities.

ARG1 is the first argument.
ARG2 is the second argument.

Returns the sum of ARG1 and ARG2.\"
  (+ arg1 arg2))")))))

(ert-deftest elisp-dev-mcp-test-describe-interactive-dynamic-function
    ()
  "Test 'describe-function' with interactively defined dynamic function."
  (elisp-dev-mcp-test-with-server
    (let* ((test-function-name
            "elisp-dev-mcp-test--interactive-dynamic-func")
           (sym (intern test-function-name))
           (lexical-binding nil))
      (unwind-protect
          (progn
            (eval `(defun ,sym (x y)
                     "A dynamically scoped interactive function.
X and Y are dynamically scoped arguments."
                     (let ((z (+ x y)))
                       (* z 2)))
                  nil)

            (let* ((req
                    (elisp-dev-mcp-test--describe-req
                     test-function-name))
                   (resp (elisp-dev-mcp-test--send-req req))
                   (text
                    (elisp-dev-mcp-test--check-resp-get-text
                     resp nil)))
              (should (string-match-p test-function-name text))
              (elisp-dev-mcp-test--check-dynamic-text text)
              (should
               (string-match-p "dynamically scoped interactive" text))
              (should
               (string-match-p
                (format "%s X Y)" test-function-name) text))))

        (fmakunbound sym)))))

(ert-deftest elisp-dev-mcp-test-describe-variable ()
  "Test that `describe-variable' MCP handler works correctly."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable" 1 `((variable . "load-path"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      ;; Basic checks for a well-known variable
      (should (string= (assoc-default 'name parsed) "load-path"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "cons"))
      (should (stringp (assoc-default 'documentation parsed))))))

(ert-deftest elisp-dev-mcp-test-describe-nonexistent-variable ()
  "Test that `describe-variable' MCP handler handles non-existent variables."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "non-existent-variable-xyz"))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Variable non-existent-variable-xyz is not bound"))))

(ert-deftest elisp-dev-mcp-test-describe-invalid-variable-type ()
  "Test that `describe-variable' handles non-string variable names properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable" 1 `((variable . 123))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Invalid variable name"))))

(ert-deftest elisp-dev-mcp-test-describe-empty-string-variable ()
  "Test that `describe-variable' MCP handler handles empty string properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable" 1 `((variable . ""))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Empty variable name"))))

(ert-deftest elisp-dev-mcp-test-describe-variable-no-docstring ()
  "Test `describe-variable' MCP handler with undocumented variables."
  (elisp-dev-mcp-test-with-server
    ;; Create a variable without documentation
    (setq elisp-dev-mcp-test--undocumented-var 42)
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--undocumented-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      ;; Check the response
      (should
       (string=
        (assoc-default 'name parsed)
        "elisp-dev-mcp-test--undocumented-var"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "integer"))
      ;; Documentation should be null for undocumented variables
      (should (null (assoc-default 'documentation parsed)))
      ;; Should NOT be a custom variable
      (should (eq (assoc-default 'is-custom parsed) :json-false)))))

(ert-deftest elisp-dev-mcp-test-describe-variable-empty-docstring ()
  "Test `describe-variable' MCP handler with empty docstring variables."
  (elisp-dev-mcp-test-with-server
    (let*
        ((req
          (mcp-server-lib-create-tools-call-request
           "elisp-describe-variable" 1
           `((variable
              .
              "elisp-dev-mcp-test-no-checkdoc--empty-docstring-var"))))
         (resp (elisp-dev-mcp-test--send-req req))
         (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
         (parsed (json-read-from-string text)))
      ;; Check the response
      (should
       (string=
        (assoc-default 'name parsed)
        "elisp-dev-mcp-test-no-checkdoc--empty-docstring-var"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "symbol"))
      ;; Empty docstring should be returned as empty string
      (should (string= (assoc-default 'documentation parsed) "")))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable ()
  "Test `describe-variable' MCP handler with custom variables."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--custom-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      ;; Check the response
      (should
       (string=
        (assoc-default 'name parsed)
        "elisp-dev-mcp-test--custom-var"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "string"))
      (should
       (string=
        (assoc-default 'documentation parsed)
        "A custom variable for testing."))
      ;; Should show it's in the test file
      (let ((source-file (assoc-default 'source-file parsed)))
        (should (stringp source-file))
        (should
         (string-match-p "elisp-dev-mcp-test\\.el" source-file)))
      ;; Should indicate it's a custom variable
      (should (eq (assoc-default 'is-custom parsed) t)))))

(ert-deftest elisp-dev-mcp-test-describe-interactive-variable ()
  "Test `describe-variable' MCP handler with interactively defined variables."
  (elisp-dev-mcp-test-with-server
    ;; Define a variable interactively (not from a file)
    (eval
     '(defvar elisp-dev-mcp-test--interactive-var 123
        "An interactively defined variable."))
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--interactive-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      ;; Check the response
      (should
       (string=
        (assoc-default 'name parsed)
        "elisp-dev-mcp-test--interactive-var"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "integer"))
      (should
       (string=
        (assoc-default 'documentation parsed)
        "An interactively defined variable."))
      (should
       (string=
        (assoc-default 'source-file parsed)
        "<interactively defined>")))))

(ert-deftest elisp-dev-mcp-test-describe-obsolete-variable ()
  "Test `describe-variable' MCP handler with obsolete variables."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--obsolete-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      ;; Check the response
      (should
       (string=
        (assoc-default 'name parsed)
        "elisp-dev-mcp-test--obsolete-var"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "string"))
      (should
       (string=
        (assoc-default 'documentation parsed)
        "An obsolete variable for testing."))
      ;; Should indicate it's obsolete
      (should (eq (assoc-default 'is-obsolete parsed) t))
      ;; Should include obsolete metadata
      (should (string= (assoc-default 'obsolete-since parsed) "1.0"))
      (should
       (string=
        (assoc-default 'obsolete-replacement parsed)
        "elisp-dev-mcp-test--new-var")))))

(ert-deftest elisp-dev-mcp-test-describe-unbound-documented-variable
    ()
  "Test `describe-variable' MCP handler with unbound but documented variables."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable" 1
             `((variable
                . "elisp-dev-mcp-test--unbound-documented-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should
       (string=
        (assoc-default 'name parsed)
        "elisp-dev-mcp-test--unbound-documented-var"))
      (should (eq (assoc-default 'bound parsed) :json-false))
      (should-not (assoc-default 'value-type parsed))
      (should
       (string=
        (assoc-default 'documentation parsed)
        "A documented variable that will be unbound for testing."))
      (let ((source-file (assoc-default 'source-file parsed)))
        (should (stringp source-file))
        (should
         (string-match-p "elisp-dev-mcp-test\\.el" source-file)))
      (should (eq (assoc-default 'is-custom parsed) :json-false))
      (should (eq (assoc-default 'is-obsolete parsed) :json-false)))))

(ert-deftest elisp-dev-mcp-test-describe-variable-alias ()
  "Test `describe-variable' MCP handler with variable aliases."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--a"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should
       (string= (assoc-default 'name parsed) "elisp-dev-mcp-test--a"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "string"))
      (should (string= (assoc-default 'documentation parsed) "x"))
      (should (eq (assoc-default 'is-alias parsed) t))
      (should
       (string=
        (assoc-default 'alias-target parsed)
        "elisp-dev-mcp-test--b")))))

(ert-deftest elisp-dev-mcp-test-describe-special-variable ()
  "Test `describe-variable' MCP handler with special variables."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable" 1 `((variable . "load-path"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (string= (assoc-default 'name parsed) "load-path"))
      (should (eq (assoc-default 'bound parsed) t))
      (should (string= (assoc-default 'value-type parsed) "cons"))
      (should (eq (assoc-default 'is-special parsed) t)))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable-group ()
  "Test `describe-variable' returns custom group for defcustom variables."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--custom-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'is-custom parsed) t))
      (should
       (string=
        (assoc-default 'custom-group parsed) "elisp-dev-mcp")))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable-type ()
  "Test `describe-variable' returns custom type for defcustom variables."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--custom-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'is-custom parsed) t))
      (should
       (string= (assoc-default 'custom-type parsed) "string")))))

(ert-deftest elisp-dev-mcp-test-describe-custom-variable-complex-type
    ()
  "Test `describe-variable' returns complex custom type for defcustom vars."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-describe-variable"
             1
             `((variable . "elisp-dev-mcp-test--custom-choice-var"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'is-custom parsed) t))
      (let ((custom-type (assoc-default 'custom-type parsed)))
        (should (stringp custom-type))
        (should (string-match-p "choice" custom-type))
        (should (string-match-p "option1" custom-type))
        (should (string-match-p "option2" custom-type))))))

(ert-deftest elisp-dev-mcp-test-info-lookup-symbol ()
  "Test that `elisp-info-lookup-symbol' MCP handler works correctly."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-info-lookup-symbol" 1 `((symbol . "defun"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'found parsed) t))
      (should (string= (assoc-default 'symbol parsed) "defun"))
      (should (stringp (assoc-default 'node parsed)))
      (should (string= (assoc-default 'manual parsed) "elisp"))
      (should (stringp (assoc-default 'content parsed)))
      (should
       (string-match-p "defun" (assoc-default 'content parsed)))
      (should (stringp (assoc-default 'info-ref parsed)))
      (should
       (string-match-p "(elisp)" (assoc-default 'info-ref parsed))))))

(ert-deftest elisp-dev-mcp-test-info-lookup-nonexistent-symbol ()
  "Test that `elisp-info-lookup-symbol' handles non-existent symbols."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-info-lookup-symbol"
             1
             `((symbol . "non-existent-symbol-xyz"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'found parsed) :json-false))
      (should
       (string=
        (assoc-default 'symbol parsed) "non-existent-symbol-xyz"))
      (should (stringp (assoc-default 'message parsed)))
      (should
       (string-match-p
        "not found" (assoc-default 'message parsed))))))

(ert-deftest elisp-dev-mcp-test-info-lookup-empty-string ()
  "Test that `elisp-info-lookup-symbol' handles empty string properly."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-info-lookup-symbol" 1 `((symbol . ""))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Empty symbol name"))))

(ert-deftest elisp-dev-mcp-test-info-lookup-invalid-type ()
  "Test that `elisp-info-lookup-symbol' handles non-string symbols."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-info-lookup-symbol" 1 `((symbol . 123))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Invalid symbol name"))))

(ert-deftest elisp-dev-mcp-test-info-lookup-function ()
  "Test `elisp-info-lookup-symbol' with a well-known function."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-info-lookup-symbol" 1 `((symbol . "mapcar"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'found parsed) t))
      (should (string= (assoc-default 'symbol parsed) "mapcar"))
      (should (stringp (assoc-default 'node parsed)))
      (let ((content (assoc-default 'content parsed)))
        (should (stringp content))
        (should (string-match-p "mapcar" content))
        (should (string-match-p "FUNCTION" content))
        (should (string-match-p "SEQUENCE" content))))))

(ert-deftest elisp-dev-mcp-test-info-lookup-special-form ()
  "Test `elisp-info-lookup-symbol' with a special form."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-info-lookup-symbol" 1 `((symbol . "let"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'found parsed) t))
      (should (string= (assoc-default 'symbol parsed) "let"))
      (let ((content (assoc-default 'content parsed)))
        (should (stringp content))
        (should (string-match-p "let" content))
        (should (string-match-p "binding" content))))))

(ert-deftest elisp-dev-mcp-test-info-lookup-variable ()
  "Test `elisp-info-lookup-symbol' with a variable."
  (elisp-dev-mcp-test-with-server
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-info-lookup-symbol" 1 `((symbol . "load-path"))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil))
           (parsed (json-read-from-string text)))
      (should (eq (assoc-default 'found parsed) t))
      (should (string= (assoc-default 'symbol parsed) "load-path"))
      (let ((content (assoc-default 'content parsed)))
        (should (stringp content))
        (should (string-match-p "load-path" content))
        (should (string-match-p "directories" content))))))

(ert-deftest elisp-dev-mcp-test-read-source-file-elpa ()
  "Test that `elisp-read-source-file` can read installed ELPA package files."
  (elisp-dev-mcp-test-with-server
    ;; Test reading mcp-server-lib main file - it's a mandatory dependency
    (let* ((mcp-dir
            (car
             (directory-files
              (expand-file-name "elpa/" user-emacs-directory)
              t "^mcp-server-lib\\(-\\|$\\)")))
           (elpa-path (expand-file-name "mcp-server-lib.el" mcp-dir))
           (req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file" 1 `((file-path . ,elpa-path))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      ;; Should contain the file header
      (should (string-match-p ";;; mcp-server-lib.el" text))
      ;; Should contain package metadata
      (should (string-match-p "Model Context Protocol" text))
      ;; Should end with proper footer
      (should
       (string-match-p ";;; mcp-server-lib.el ends here" text)))))

(ert-deftest elisp-dev-mcp-test-read-source-file-system ()
  "Test that `elisp-read-source-file` can read Emacs system files."
  (elisp-dev-mcp-test-with-server
    ;; Test reading a system file that should exist in all Emacs installations
    (let* ((system-file
            (concat
             (file-name-sans-extension
              (locate-library "subr"))
             ".el"))
           (req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file"
             1
             `((file-path . ,system-file))))
           (resp (elisp-dev-mcp-test--send-req req))
           (text (elisp-dev-mcp-test--check-resp-get-text resp nil)))
      ;; Verify that subr.el doesn't exist but subr.el.gz does
      (should-not (file-exists-p system-file))
      (should (file-exists-p (concat system-file ".gz")))
      ;; Should contain typical Emacs system file content
      ;; This verifies that .gz files are handled transparently
      (should (string-match-p ";;; subr.el" text))
      (should (string-match-p "GNU Emacs" text)))))

(ert-deftest elisp-dev-mcp-test-read-source-file-invalid-format ()
  "Test that `elisp-read-source-file` rejects invalid formats."
  (elisp-dev-mcp-test-with-server
    ;; Test relative path
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file"
             1
             `((file-path . "relative/path.el"))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp
       "Invalid path format: must be absolute path ending in .el"))

    ;; Test path not ending in .el
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file"
             1
             `((file-path . "/absolute/path/file.elc"))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp
       "Invalid path format: must be absolute path ending in .el"))

    ;; Test path with .. traversal
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file"
             1
             `((file-path . "/some/path/../../../etc/passwd.el"))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Path contains illegal '..' traversal"))))

(ert-deftest elisp-dev-mcp-test-read-source-file-not-found ()
  "Test that `elisp-read-source-file` handles missing files gracefully."
  (elisp-dev-mcp-test-with-server
    ;; Test non-existent file in a valid directory
    (let* ((elpa-dir
            (file-name-directory (locate-library "mcp-server-lib")))
           (non-existent-path
            (expand-file-name "non-existent-file.el" elpa-dir))
           (req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file"
             1
             `((file-path . ,non-existent-path))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "File not found: .* (tried .el and .el.gz)"))))

(ert-deftest elisp-dev-mcp-test-read-source-file-security ()
  "Test that `elisp-read-source-file` enforces security restrictions."
  (elisp-dev-mcp-test-with-server
    ;; Test access outside allowed directories
    (let* ((req
            (mcp-server-lib-create-tools-call-request
             "elisp-read-source-file"
             1
             `((file-path . "/etc/passwd.el"))))
           (resp (elisp-dev-mcp-test--send-req req)))
      (elisp-dev-mcp-test--verify-error-resp
       resp "Access denied: path outside allowed directories"))))


(ert-deftest elisp-dev-mcp-test-describe-bytecode-function ()
  "Test `describe-function' with byte-compiled functions."
  (let* ((source-file
          (expand-file-name "elisp-dev-mcp-test-bytecode.el"))
         (bytecode-file (byte-compile-dest-file source-file)))
    (unwind-protect
        (progn
          (should (byte-compile-file source-file))
          (should (load bytecode-file nil t t))

          (elisp-dev-mcp-test-with-server
            (let* ((req
                    (elisp-dev-mcp-test--describe-req
                     "elisp-dev-mcp-test-bytecode--with-header"))
                   (resp (elisp-dev-mcp-test--send-req req))
                   (text
                    (elisp-dev-mcp-test--check-resp-get-text
                     resp nil)))

              (should
               (string-match-p
                "elisp-dev-mcp-test-bytecode--with-header" text))
              (should (string-match-p "byte-compiled" text))
              (should
               (string-match-p
                "A byte-compiled function with header comment" text))
              (should
               (string-match-p
                "elisp-dev-mcp-test-bytecode\\.el" text)))))

      (when (file-exists-p bytecode-file)
        (delete-file bytecode-file)))))

(ert-deftest
    elisp-dev-mcp-test-get-bytecode-function-definition-with-header
    ()
  "Test `get-function-definition' with byte-compiled function with header."
  (let* ((source-file
          (expand-file-name "elisp-dev-mcp-test-bytecode.el"))
         (bytecode-file (byte-compile-dest-file source-file)))
    (unwind-protect
        (progn
          (should (byte-compile-file source-file))
          (should (load bytecode-file nil t t))

          (elisp-dev-mcp-test-with-server
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     "elisp-dev-mcp-test-bytecode--with-header"))
                   (source (assoc-default 'source parsed-resp))
                   (file-path (assoc-default 'file-path parsed-resp))
                   (start-line
                    (assoc-default 'start-line parsed-resp))
                   (end-line (assoc-default 'end-line parsed-resp)))

              (should
               (string=
                (file-name-nondirectory file-path)
                "elisp-dev-mcp-test-bytecode.el"))
              (should (= start-line 20))
              (should (= end-line 25))
              (should (string-match-p ";; Header comment" source))
              (should
               (string-match-p ";; This should be preserved" source))
              (should
               (string-match-p
                "defun elisp-dev-mcp-test-bytecode--with-header"
                source)))))

      (when (file-exists-p bytecode-file)
        (delete-file bytecode-file)))))

(ert-deftest
    elisp-dev-mcp-test-get-bytecode-function-definition-no-docstring
    ()
  "Test `get-function-definition' with byte-compiled function w/o docstring."
  (let* ((source-file
          (expand-file-name "elisp-dev-mcp-test-bytecode.el"))
         (bytecode-file (byte-compile-dest-file source-file)))
    (unwind-protect
        (progn
          (should (byte-compile-file source-file))
          (should (load bytecode-file nil t t))

          (elisp-dev-mcp-test-with-server
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     "elisp-dev-mcp-test-bytecode--no-docstring"))
                   (source (assoc-default 'source parsed-resp)))

              (should
               (string=
                source
                (concat
                 "(defun elisp-dev-mcp-test-bytecode--no-docstring "
                 "(a b)\n  (* a b))"))))))

      (when (file-exists-p bytecode-file)
        (delete-file bytecode-file)))))

(ert-deftest elisp-dev-mcp-test-get-bytecode-function-empty-docstring
    ()
  "Test `get-function-definition' with byte-compiled empty docstring function."
  (let* ((source-file
          (expand-file-name "elisp-dev-mcp-test-bytecode.el"))
         (bytecode-file (byte-compile-dest-file source-file)))
    (unwind-protect
        (progn
          (should (byte-compile-file source-file))
          (should (load bytecode-file nil t t))

          (elisp-dev-mcp-test-with-server
            (let* ((parsed-resp
                    (elisp-dev-mcp-test--get-definition-response-data
                     "elisp-dev-mcp-test-bytecode--empty-docstring"))
                   (source (assoc-default 'source parsed-resp)))

              (should
               (string=
                source
                (concat
                 "(defun elisp-dev-mcp-test-bytecode--empty-docstring "
                 "(n)\n  \"\"\n  (* n 2))"))))))

      (when (file-exists-p bytecode-file)
        (delete-file bytecode-file)))))

(provide 'elisp-dev-mcp-test)
;;; elisp-dev-mcp-test.el ends here
