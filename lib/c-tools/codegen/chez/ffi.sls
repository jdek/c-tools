;; SPDX-License-Identifier: WTFPL
;; FFI Code Generator - AST to Chez Scheme FFI
;; Generates foreign-procedure and foreign-struct definitions

(library (c-tools codegen chez ffi)
  (export generate-ffi-code
          ast->ffi-type
          declaration->ffi-form)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs lists)
          (c-tools ast c)
          (except (c-tools utility) extract-exports))

  ;;; Type Mapping: C types to Chez Scheme FFI types

  ;; Convert C basic type to Chez FFI type
  (define (basic-type->ffi type-name)
    (case type-name
      [(void) 'void]
      [(char) 'char]
      [(signed-char) 'char]
      [(unsigned-char) 'unsigned-char]
      [(short) 'short]
      [(unsigned-short) 'unsigned-short]
      [(int) 'int]
      [(unsigned) 'unsigned]
      [(long) 'long]
      [(unsigned-long) 'unsigned-long]
      [(long-long) 'long-long]
      [(unsigned-long-long) 'unsigned-long-long]
      [(float) 'float]
      [(double) 'double]
      [else
       ;; Unknown basic type - treat as int
       'int]))

  ;; Convert AST type to Chez FFI type specification
  (define (ast->ffi-type type)
    (cond
      ;; Basic type
      [(basic-type? type)
       (basic-type->ffi (basic-type-name type))]

      ;; Pointer type
      [(pointer-type? type)
       (let ([pointee (pointer-type-pointee type)])
         (cond
           ;; char* -> string (for convenience)
           [(and (basic-type? pointee)
                 (eq? (basic-type-name pointee) 'char))
            'string]
           ;; void* -> void*
           [(and (basic-type? pointee)
                 (eq? (basic-type-name pointee) 'void))
            'void*]
           ;; struct foo* -> (* struct-foo)
           [(named-type? pointee)
            (case (named-type-kind pointee)
              [(struct)
               (list '* (symbol-append 'struct- (named-type-name pointee)))]
              [(union)
               (list '* (symbol-append 'union- (named-type-name pointee)))]
              [else
               (list '* (ast->ffi-type pointee))])]
           ;; Other pointers: (* T)
           [else
            (list '* (ast->ffi-type pointee))]))]

      ;; Named type (struct, union, enum, typedef)
      [(named-type? type)
       (case (named-type-kind type)
         [(struct)
          ;; Reference to struct type by name
          (symbol-append 'struct- (named-type-name type))]
         [(union)
          ;; Reference to union type by name
          (symbol-append 'union- (named-type-name type))]
         [(enum)
          ;; Enums are integers
          'int]
         [(typedef)
          ;; Use the typedef name as-is (assumes it's defined elsewhere)
          (named-type-name type)])]

      ;; Qualified type (const, volatile)
      [(qualified-type? type)
       ;; Ignore qualifiers in FFI
       (ast->ffi-type (qualified-type-type type))]

      ;; Array type
      [(array-type? type)
       ;; Arrays decay to pointers in FFI
       (list '* (ast->ffi-type (array-type-element type)))]

      ;; Function type (function pointers)
      [(function-type? type)
       ;; Function pointers are just void* for now
       'void*]

      ;; Unknown type
      [else 'void*]))

  ;;; Declaration to FFI Form Generation

  ;; Generate foreign-procedure form from function-decl
  (define (function-decl->ffi-form decl lib-name)
    (let* ([name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [variadic? (function-decl-variadic? decl)]
           [ffi-return (ast->ffi-type return-type)]
           [ffi-params (map (lambda (p) (ast->ffi-type (param-type p)))
                           params)]
           [scheme-name (symbol-append 'c- name)])

      (if variadic?
          ;; Variadic functions need special handling - skip for now
          `(comment ,(format "Skipping variadic function: ~a" name))
          ;; Regular function
          `(define ,scheme-name
             (foreign-procedure ,lib-name ,(symbol->string name)
                              ,ffi-params ,ffi-return)))))

  ;; Generate typedef form
  (define (typedef->ffi-form decl)
    (let* ([name (typedef-name decl)]
           [type (typedef-type decl)]
           [ffi-type (ast->ffi-type type)])
      `(define-ffi-type ,name ,ffi-type)))

  ;; Generate struct definition form using define-ftype
  (define (struct-decl->ffi-form decl)
    (let ([name (struct-decl-name decl)]
          [fields (struct-decl-fields decl)])
      (let ([ftype-name (symbol-append 'struct- name)]
            [field-specs (map (lambda (field)
                               (list (field-name field)
                                     (ast->ffi-type (field-type field))))
                             fields)])
        `(define-ftype ,ftype-name
           (struct ,@field-specs)))))

  ;; Generate union definition form using define-ftype
  (define (union-decl->ffi-form decl)
    (let ([name (union-decl-name decl)]
          [fields (union-decl-fields decl)])
      (let ([ftype-name (symbol-append 'union- name)]
            [field-specs (map (lambda (field)
                               (list (field-name field)
                                     (ast->ffi-type (field-type field))))
                             fields)])
        `(define-ftype ,ftype-name
           (union ,@field-specs)))))

  ;; Generate enum definition form
  (define (enum-decl->ffi-form decl)
    (let ([name (enum-decl-name decl)]
          [enumerators (enum-decl-enumerators decl)])
      ;; Generate define forms for each enumerator
      (cons 'begin
            (map (lambda (e)
                   `(define ,(enumerator-name e) ,(enumerator-value e)))
                 enumerators))))

  ;; Convert a single declaration to FFI form
  (define (declaration->ffi-form decl lib-name)
    (cond
      [(function-decl? decl)
       (function-decl->ffi-form decl lib-name)]
      [(typedef? decl)
       (typedef->ffi-form decl)]
      [(struct-decl? decl)
       (struct-decl->ffi-form decl)]
      [(union-decl? decl)
       (union-decl->ffi-form decl)]
      [(enum-decl? decl)
       (enum-decl->ffi-form decl)]
      [else
       '(comment "Unknown declaration type")]))

  ;;; Top-level API

  ;; Generate complete FFI code from list of declarations
  (define (generate-ffi-code declarations lib-name)
    ;;   Generate FFI bindings for a list of C declarations
    (let ([forms (map (lambda (decl) (declaration->ffi-form decl lib-name))
                     declarations)])
      ;; Wrap in a library form
      `(library (ffi ,(string->symbol lib-name))
         (export ,@(extract-exports forms))
         (import (chezscheme))

         ,@forms)))

  ;; Extract exported identifiers from generated forms
  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              ;; define form - export the name
              [(and (pair? form) (eq? (car form) 'define))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; define-ftype form - export the type name
              [(and (pair? form) (eq? (car form) 'define-ftype))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; begin form - recurse
              [(and (pair? form) (eq? (car form) 'begin))
               (loop (append (cdr form) (cdr forms)) exports)]
              ;; Other forms - skip
              [else
               (loop (cdr forms) exports)])))))

)
