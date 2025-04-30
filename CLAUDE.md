# Development Guidelines for Claude

## Test-Driven Development Flow

1. Write user documentation first (README.org)
1. Write ERT tests that verify the documented functionality
1. Run tests to confirm they fail as expected
1. Implement the minimal source code needed to make tests pass
1. Repeat for each new feature or change

## Process Details

- Always start with documenting the feature in README.org
- Create detailed test cases in ERT format that thoroughly test the documented behavior
- Verify tests fail before implementing code
- Write only the minimal code needed to make tests pass
- Do not write obvious comments
- Always run tests after implementation to verify functionality
- **ALWAYS run ./check.sh after every step to ensure code quality standards are maintained**

## Commands to Run

- Run ERT tests: `(ert-run-tests-interactively "elisp-dev-mcp")`
- Byte compile: `(byte-compile-file "elisp-dev-mcp.el")`
