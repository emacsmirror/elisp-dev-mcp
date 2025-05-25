# Design Document: elisp-info-lookup-symbol Tool

## Overview

This document outlines the design for a new MCP tool that provides Info documentation lookups for Elisp symbols. The tool will allow AI agents to access the authoritative Emacs Lisp Reference Manual directly.

## Tool Specification

### Function Signature

```elisp
(defun elisp-dev-mcp--info-lookup-symbol (symbol)
  "Look up SYMBOL in Elisp Info documentation.
  
MCP Parameters:
  symbol - The symbol to look up (string)"
  ...)
```

### Return Format

#### Success Response
```json
{
  "found": true,
  "symbol": "defun",
  "node": "Defining Functions", 
  "manual": "elisp",
  "content": "<!-- Complete node content -->\n\n12.4 Defining Functions\n=======================\n\nWe usually give a name to a function when it is first created.  This\nis called \"defining a function\", and we usually do it with the\n'defun' macro...\n\n[entire node content continues...]",
  "info-ref": "(elisp)Defining Functions"
}
```

#### Not Found Response
```json
{
  "found": false,
  "symbol": "nonexistent-symbol",
  "message": "Symbol 'nonexistent-symbol' not found in Elisp Info documentation"
}
```

## Implementation Plan

### Core Functions

1. **Main lookup function**:
```elisp
(defun elisp-dev-mcp--info-lookup-symbol (symbol)
  "Look up SYMBOL in Elisp Info documentation and return complete node content."
  (condition-case err
      (let* ((sym (if (stringp symbol) symbol (symbol-name symbol)))
             (result (elisp-dev-mcp--perform-info-lookup sym)))
        (if result
            (json-encode result)
          (json-encode
           `((found . :json-false)
             (symbol . ,sym)
             (message . ,(format "Symbol '%s' not found in Elisp Info documentation" sym))))))
    (error (mcp-tool-throw (format "Error: %S" err)))))
```

2. **Info lookup implementation**:
```elisp
(defun elisp-dev-mcp--perform-info-lookup (symbol)
  "Perform the actual Info lookup for SYMBOL.
Returns an alist with lookup results or nil if not found."
  ;; Implementation will:
  ;; 1. Create a temporary Info buffer
  ;; 2. Use info-lookup internals to find the symbol
  ;; 3. Navigate to the Info node
  ;; 4. Extract the complete node content
  ;; 5. Clean up and return structured data
  ...)
```

3. **Content extraction**:
```elisp
(defun elisp-dev-mcp--extract-info-node-content ()
  "Extract the complete content of the current Info node.
Assumes we're in an Info buffer at the correct node."
  (let ((content (buffer-string))
        (start nil)
        (end nil))
    ;; Find the start of actual content (after the node header)
    (goto-char (point-min))
    (when (re-search-forward "^File: [^,]+,  Node: [^,\n]+.*\n" nil t)
      (setq start (point)))
    
    ;; Find the end of content (before the next node marker or end of buffer)
    (if (re-search-forward "^" nil t)
        (setq end (1- (point)))
      (setq end (point-max)))
    
    ;; Extract and clean up the content
    (when (and start end)
      (elisp-dev-mcp--clean-info-content 
       (buffer-substring-no-properties start end)))))
```

4. **Content cleanup**:
```elisp
(defun elisp-dev-mcp--clean-info-content (content)
  "Clean up Info formatting from CONTENT.
Removes navigation markers while preserving documentation structure."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    
    ;; Remove footnote references like (*note ...)
    (while (re-search-forward "\\*[Nn]ote[ \n][^:]*::" nil t)
      (replace-match "[See: \\&]"))
    
    ;; Additional cleanup as needed
    (buffer-string)))
```

### Tool Registration

```elisp
(mcp-register-tool
 #'elisp-dev-mcp--info-lookup-symbol
 :id "elisp-info-lookup-symbol"
 :description
 "Look up Elisp symbols in Info documentation and return the complete
documentation node. Returns the full content of the Info node containing
the symbol's documentation from the Emacs Lisp Reference Manual.

Parameters:
  symbol - The Elisp symbol to look up (string)

Returns JSON with:
  found - Whether documentation was found (boolean)
  symbol - The symbol that was looked up (string)  
  node - The Info node name containing the documentation (string, when found)
  manual - The Info manual name, typically 'elisp' (string, when found)
  content - The complete Info node content including all examples,
            cross-references, and related information (string, when found)
  info-ref - Info reference like '(elisp)Node Name' for direct access
             (string, when found)
  message - Error or not-found message (string, when not found)

The content field contains the entire Info node, ensuring you have full
context including:
- Complete function/variable descriptions
- All code examples and usage patterns  
- Cross-references to related concepts
- Any warnings, notes, or special considerations

Common symbols that can be looked up:
- Special forms: defun, defvar, let, if, cond, lambda
- Functions: mapcar, apply, funcall, concat
- Macros: when, unless, dolist, defmacro
- Variables: load-path, emacs-version
- Concepts: 'lexical binding', 'dynamic binding'

Error cases:
- Symbol not found in documentation
- Invalid symbol name
- Info system unavailable"
 :read-only t)
```

## Key Design Decisions

1. **Elisp-specific**: No mode parameter needed, always searches Elisp documentation
2. **Complete nodes**: Returns entire Info nodes rather than excerpts for full context
3. **Non-interactive**: Works entirely in the background without UI
4. **Structured output**: JSON format with consistent schema
5. **Read-only**: This is a documentation lookup tool, no modifications

## Implementation Notes

### Buffer Management
- Create temporary Info buffers for lookups
- Ensure proper cleanup with `unwind-protect`
- Avoid interfering with user's Info sessions

### Error Handling
- Handle symbols not found in Info
- Deal with Info system not available
- Validate input (string type, non-empty)

### Performance Considerations
- Info lookups are generally fast
- Consider caching if the same symbols are looked up repeatedly
- Buffer creation/cleanup overhead is minimal

### Testing Strategy

Test with various symbol types:
- **Built-in functions**: `car`, `cdr`, `mapcar`
- **Special forms**: `let`, `if`, `defun`
- **Macros**: `when`, `unless`, `defmacro`
- **Variables**: `load-path`, `emacs-version`
- **Concepts**: `lexical binding` (multi-word entries)
- **Edge cases**: 
  - Empty string
  - Non-existent symbols
  - Symbols with special characters
  - Very common words that might have multiple matches

## Benefits

1. **Authoritative source**: Direct access to official Emacs documentation
2. **Complete context**: Full nodes ensure nothing is missed
3. **AI-friendly**: Structured data that LLMs can process effectively
4. **Integration**: Fits seamlessly with existing elisp-dev-mcp tools

## Future Enhancements

1. **Cross-reference resolution**: Follow "See also" references
2. **Index search**: Fallback to index when direct lookup fails
3. **Multiple matches**: Handle symbols that appear in multiple nodes
4. **Caching layer**: Cache frequently accessed nodes
5. **Related symbols**: Return related functions/variables mentioned in the node