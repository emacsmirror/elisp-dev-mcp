# Plan: Implement elisp-describe-variable Tool

## Overview

Implement a new MCP tool that returns information about Elisp variables
without exposing potentially sensitive values.

## Design Considerations

### Information to Include (Safe)

1. **Variable name and type**

   - Whether it's defvar, defcustom, defconst
   - Whether it's currently bound
   - Type of the value (symbol, string, list, etc.) without exposing the value

1. **Documentation**

   - Docstring
   - Custom group (for defcustom)
   - Custom type specification

1. **Source location**

   - File where defined
   - Whether interactively defined

1. **Variable properties**
   - Buffer-local status
   - Obsolescence information
   - Alias information
   - Whether it's a special (dynamically scoped) variable

### Information to Exclude (Potentially Sensitive)

- Current value
- Default value
- Custom-set values
- History of values

## Implementation Approach

```elisp
(defun elisp-dev-mcp--describe-variable (variable)
  "Get information about Emacs Lisp VARIABLE without exposing its value.

MCP Parameters:
  variable - The name of the variable to describe"
  (condition-case err
      (let ((sym (intern variable)))
        (if (boundp sym)
            (let* ((type (type-of (symbol-value sym)))
                   (doc (documentation-property sym 'variable-documentation))
                   (custom-p (custom-variable-p sym))
                   (local-p (local-variable-p sym))
                   (obsolete (get sym 'byte-obsolete-variable))
                   (file (find-lisp-object-file-name sym 'defvar)))
              ;; Build safe description without values
              (json-encode
               `((name . ,variable)
                 (bound . t)
                 (value-type . ,(symbol-name type))
                 (documentation . ,(or doc "Not documented"))
                 (is-custom . ,custom-p)
                 (is-local . ,local-p)
                 (is-obsolete . ,(if obsolete t :json-false))
                 (source-file . ,(or file "<interactively defined>"))
                 ,@(when obsolete
                     `((obsolete-since . ,(nth 0 obsolete))
                       (obsolete-replacement . ,(nth 1 obsolete)))))))
          ;; Variable exists but is unbound
          (json-encode
           `((name . ,variable)
             (bound . :json-false)
             (documentation . ,(documentation-property
                               sym 'variable-documentation))))))
    (error (mcp-tool-throw (format "Error: %S" err)))))
```

## Key Design Decisions

1. **Value Type Instead of Value**: Return `(value-type . "string")`
   instead of the actual string value
1. **Structured JSON Response**: Similar to get-function-definition,
   return structured data
1. **Handle Unbound Variables**: Variables can exist (have documentation)
   but be unbound
1. **Custom Variable Information**: Include whether it's customizable but
   not the custom choices

## Test Scenarios

- Regular variables (defvar)
- Constants (defconst)
- Custom variables (defcustom)
- Buffer-local variables
- Obsolete variables
- Unbound but documented variables
- Variable aliases
- Special variables with lexical-binding

## Tool Registration

```elisp
(mcp-register-tool
 #'elisp-dev-mcp--describe-variable
 :id "elisp-describe-variable"
 :description
 "Get information about an Emacs Lisp variable without exposing its value.
Returns variable documentation and metadata from the current Emacs environment.

Returns JSON with:
- name: Variable name
- bound: Whether variable is currently bound
- value-type: Type of the value (symbol, string, list, etc.)
- documentation: Variable docstring
- is-custom: Whether it's a customizable variable
- is-local: Whether it's buffer-local
- is-obsolete: Whether it's marked obsolete
- source-file: Where the variable is defined

Note: Does NOT return the actual value to avoid exposing sensitive data
like API keys, passwords, or personal information.

Error cases:
- Invalid input types return 'Error: ...'"
 :read-only t)
```

## Integration Points

1. Add to `elisp-dev-mcp-enable` function
1. Add to `elisp-dev-mcp-disable` function
1. Create comprehensive test suite following existing patterns

This tool would complement the existing function tools nicely and provide
safe introspection of the Elisp environment's state.
