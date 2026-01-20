# C Tools

A modular C/C++ parsing and FFI binding generation toolkit for Scheme.

## Overview

C Tools provides independently reusable components for working with C and C++ code:

- **Lexer** - Tokenize C/C++ source with trivia preservation
- **Parser** - Build ASTs from declarations
- **Preprocessor** - Handle macros, includes, conditionals
- **FFI Generator** - Generate Chez Scheme or Racket FFI bindings
- **Effects System** - Composable effect handlers for extensibility

## Quick Start

Generate FFI bindings from C/C++ headers:

```bash
# Chez Scheme FFI (default)
./tools/ffi.ss mylib.h -l mylib -o bindings.sls

# Racket FFI
./tools/ffi.ss mylib.h --racket -o bindings.rkt

# C++ with name mangling
./tools/ffi.ss myclass.hpp -x c++ --racket -o bindings.rkt
```

## Installation

Add to your Scheme library path:

```bash
export CHEZSCHEMELIBDIRS="/path/to/c-tools/lib:${CHEZSCHEMELIBDIRS}"
```

## FFI Generation Features

### C Bindings
- Functions with automatic type mapping
- Structs (full definitions or opaque pointers)
- Enums as constants
- Typedefs

### C++ Bindings
- Namespaces (fully supported)
- Classes as opaque pointer types
- Methods with automatic Itanium C++ ABI name mangling
- Template specializations
- Generic templates gracefully skipped

### Supported Targets

**Chez Scheme** (`--chez`, default):
```scheme
(library (ffi mylib)
  (export c-add c-multiply)
  (import (chezscheme))

  (define c-add
    (foreign-procedure "mylib" "add"
                      (int int) int)))
```

**Racket** (`--racket`):
```racket
#lang racket/base
(require ffi/unsafe ffi/unsafe/define)
(provide c-add c-multiply)

(define lib (ffi-lib "mylib"))
(define-ffi-definer define-ffi lib)

(define-ffi c-add (_fun _int _int -> _int) #:c-id "add")
```

## Library Usage

### Basic Parsing (No Preprocessing)

```scheme
(import (c-tools lexer c)
        (c-tools parser c))

(let* ([tokens (tokenize-file "simple.h")]
       [decls (parse-declarations tokens)])
  decls)
```

### Full Pipeline with Effects

```scheme
(import (c-tools preprocess c)
        (c-tools parser c)
        (c-tools codegen chez ffi)
        (c-tools effects files)
        (c-tools effects cpp core))

(with-file-system #f "."
  (lambda ()
    (with-effects '((cpp-include ())
                    cpp-macros
                    cpp-conditional)
      (lambda ()
        (let* ([tokens (preprocess-file "header.h")]
               [decls (parse-declarations tokens)]
               [ffi (generate-ffi-code decls "libname")])
          ffi)))))
```

### Diagnostics Collection

```scheme
(import (c-tools lexer c))

(call-with-values
  (lambda ()
    (call-with-input-file "header.h"
      (lambda (port)
        (tokenize-with-diagnostics port "header.h"))))
  (lambda (cst-nodes diagnostics)
    (for-each display-diagnostic diagnostics)
    cst-nodes))
```

## Library Structure

### Core `(c-tools core ...)`
- `tokens` - Token types and predicates
- `cst` - Concrete Syntax Trees with trivia
- `conditions` - Exception hierarchy
- `limits` - Security limits (DoS prevention)

### AST `(c-tools ast ...)`
- `c` - C type system and declarations
- `cpp` - C++ extensions (namespaces, classes, templates)

### Lexer `(c-tools lexer ...)`
- `c` - C tokenizer
- `cpp` - C++ tokenizer with raw strings, binary literals, etc.
- Three API levels: tokens, CST nodes, diagnostics

### Parser `(c-tools parser ...)`
- `c` - C parser (closure pattern)
- `cpp` - C++ parser with template support and >> splitting

### Preprocessor `(c-tools preprocess ...)`
- `c` - C preprocessor
- `cpp` - C++ preprocessor

### Codegen `(c-tools codegen ...)`
- `chez/ffi` - Chez Scheme C FFI
- `chez/cpp-ffi` - Chez Scheme C++ FFI
- `racket/ffi` - Racket C FFI
- `racket/cpp-ffi` - Racket C++ FFI
- `mangle` - C++ name mangling (Itanium ABI)

### Effects `(c-tools effects ...)`
- `core` - Effect primitive (shift/reset)
- `registry` - Effect composition
- `files` - File I/O
- `cpp/...` - Preprocessor effects
- `cpp-lang/...` - Language context tracking

## Known Limitations

- C++ constructors/destructors skipped (requires C wrapper functions)
- Generic templates skipped (only specializations supported)
- Virtual functions work but require proper object layout
- Operator overloading not yet supported

## Use Cases

- Generate FFI bindings for C/C++ libraries
- Build syntax highlighters or formatters
- Create code analysis tools
- Parse headers without full preprocessing
- Generate documentation from headers
