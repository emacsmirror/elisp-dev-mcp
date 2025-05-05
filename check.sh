#!/bin/bash

STATUS=0

echo "Checking Elisp syntax via byte compilation..."
if ! emacs -Q --batch \
	--eval "(setq byte-compile-warnings nil)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/elpa/mcp/\"))" \
	--eval "(dolist (file '(\"elisp-dev-mcp.el\" \"elisp-dev-mcp-test.el\"))
      (message \"Checking syntax of %s...\" file)
      (if (not (byte-compile-file file))
          (kill-emacs 1)))"; then
	echo "Elisp byte compilation check failed"
	STATUS=1
fi

# Only run auto-formatting if there are no errors so far
if [ $STATUS -eq 0 ]; then
	echo "Running elisp-autofmt on Elisp files..."
	if ! emacs -batch \
		--eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/elpa/mcp/\"))" \
		--eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/elpa/elisp-autofmt-20250421.1112\"))" \
		--eval "(progn
                     (add-to-list 'load-path \".\")
                     (require 'elisp-autofmt)
                     (dolist (file '(\"elisp-dev-mcp.el\" \"elisp-dev-mcp-test.el\"))
                       (message \"Formatting %s...\" file)
                       (find-file file)
                       (elisp-autofmt-buffer-to-file)
                       (message \"Formatted %s\" file)))"; then
		echo "elisp-autofmt failed"
		STATUS=1
	fi
else
	echo "Skipping automatic formatting due to syntax errors"
fi

# Remove existing byte-compiled files to avoid warnings about newer source files
echo "Removing any existing .elc files..."
rm -f ./*.elc

echo "Running elisp-lint on Emacs Lisp files..."
if ! emacs -Q --batch \
	--eval "(package-initialize)" \
	--eval "(require 'elisp-lint)" \
	-f elisp-lint-files-batch elisp-dev-mcp.el elisp-dev-mcp-test.el; then
	echo "elisp-lint failed"
	STATUS=1
fi

echo "Running all tests..."
if ! emacs -Q -batch \
	--eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/elpa/mcp/\"))" \
	--eval "(add-to-list 'load-path \".\")" \
	-l elisp-dev-mcp.el -l elisp-dev-mcp-test.el \
	--eval '(ert-run-tests-batch-and-exit)'; then
	echo "ERT tests failed"
	STATUS=1
fi

echo "Running markdownlint..."
if ! mdl ./*.md; then
	STATUS=1
fi

echo "Running textlint..."
if ! textlint --rule terminology ./*.md; then
	STATUS=1
fi

echo "Running prettier..."
if ! prettier --check ./*.md; then
	STATUS=1
fi

echo "Running prettier on GitHub workflows..."
if ! prettier --check .github/workflows/*.yml; then
	STATUS=1
fi

echo "Running actionlint on GitHub workflows..."
if ! actionlint .github/workflows/*.yml; then
	STATUS=1
fi

echo "Running shellcheck..."
if ! shellcheck ./*.sh; then
	STATUS=1
fi

if [ $STATUS -eq 0 ]; then
	echo "Running shfmt..."
	shfmt -w ./*.sh
	echo "OK to continue"
	exit 0
else
	echo "Issues found, fix before continuing"
	exit 1
fi
