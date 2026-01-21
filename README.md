# C Tools

A modular C/C++ parsing and FFI binding generation toolkit for Schemes and Lisps.

## Overview

C Tools provides independently reusable components for working with C and C++ code:

- **Lexer** - Tokenize C/C++ source with trivia preservation
- **Parser** - Build ASTs from declarations
- **Preprocessor** - Handle macros, includes, conditionals
- **FFI Generator** - Generate FFI bindings for Chez, Racket, Guile, Chicken, and Gambit Scheme
- **Effects System** - Composable effect handlers for extensibility

Example uses may include:

- Generate FFI bindings for C/C++ libraries
- Build syntax highlighters or formatters
- Create code analysis tools
- Parse headers without full preprocessing
- Generate documentation from headers

## Quick Start

Generate FFI bindings from C/C++ headers:

```bash
# Chez Scheme FFI (default)
./ffi.ss mylib.h -l mylib -o bindings.sls

# Racket FFI
./ffi.ss mylib.h --racket -o bindings.rkt

# Guile FFI
./ffi.ss mylib.h --guile -o bindings.scm

# Chicken Scheme FFI
./ffi.ss mylib.h --chicken -o bindings.scm

# Gambit Scheme FFI
./ffi.ss mylib.h --gambit -o bindings.scm

# C++ with name mangling (any target)
./ffi.ss myclass.hpp -x c++ --guile -o bindings.scm
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

### Backend-Specific Features
- **Chez**: `foreign-procedure`, `define-ftype` for structs
- **Racket**: `ffi/unsafe`, `_fun` types, `define-ffi` helpers
- **Guile**: `foreign-library-function`, struct layouts as lists
- **Chicken**: `foreign-lambda`, `foreign-declare` for headers
- **Gambit**: `c-lambda`, `c-declare` for inline C

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

**Guile** (`--guile`):
```scheme
(define-module (ffi mylib)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:export (c-add c-multiply))

(define-foreign-library libmylib
  (dynamic-link "mylib"))

(define c-add
  (foreign-library-function libmylib "add"
    #:return-type int
    #:arg-types (list int int)))
```

**Chicken Scheme** (`--chicken`):
```scheme
(module mylib (c-add c-multiply)
  (import scheme chicken foreign)

  (foreign-declare "#include <mylib.h>")

  (define c-add
    (foreign-lambda int "add" int int)))
```

**Gambit Scheme** (`--gambit`):
```scheme
;; Gambit FFI bindings for mylib
(c-declare "#include <mylib.h>")

(define c-add
  (c-lambda (int int) int "add"))
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

## Limitations

- C++ constructors/destructors skipped (requires C wrapper functions)
- Generic templates skipped (only specializations supported)
- Virtual functions work but require proper object layout
- Operator overloading not yet supported

## Portability

The library is written in (mostly) R6RS Scheme with minimal implementation-specific dependencies:

- **Core libraries**: Pure R6RS (lexer, parser, AST, effects system)
- **Utility library**: R6RS-compatible implementations of `format`, `box`, `make-parameter`, etc.
- **Chez-specific**: Only file I/O operations (`lib/c-tools/effects/files.sls`)
