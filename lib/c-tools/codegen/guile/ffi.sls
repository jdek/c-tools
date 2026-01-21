;; SPDX-License-Identifier: WTFPL
;; FFI Code Generator - AST to Guile Scheme FFI
;; Generates bindings using (system foreign)

(library (c-tools codegen guile ffi)
  (export generate-ffi-code
          ast->ffi-type
          declaration->ffi-form)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs lists)
          (rnrs io ports)
          (c-tools ast c)
          (c-tools utility))

  ;;=======================================================================
  ;; Main FFI Generation

  ;; generate-ffi-code : list-of-decl string => guile-module
  ;;   Generates complete Guile FFI module from declarations.
  (define (generate-ffi-code decls lib-name)
    (let* ([forms (filter-map
                    (lambda (decl) (declaration->ffi-form decl lib-name))
                    decls)]
           [exports (extract-exports forms)])
      (guile-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (guile-module-wrapper lib-name exports forms)
    (string-append
      "(define-module (ffi " lib-name ")\n"
      "  #:use-module (system foreign)\n"
      "  #:use-module (system foreign-library)\n"
      "  #:export (" (string-join (map symbol->string exports) "\n           ") "))\n\n"
      "(define-foreign-library lib" lib-name "\n"
      "  (dynamic-link \"" lib-name "\"))\n\n"
      (string-join (map guile-form->string forms) "\n\n")))

  (define (guile-form->string form)
    (call-with-string-output-port
      (lambda (port)
        (pretty-print-guile form port))))

  (define (pretty-print-guile form port)
    ;; Simple pretty printer for Guile forms
    (cond
      [(pair? form)
       (display "(" port)
       (let loop ([lst form] [first? #t])
         (unless (null? lst)
           (unless first? (display " " port))
           (if (pair? (car lst))
               (pretty-print-guile (car lst) port)
               (display (guile-value->string (car lst)) port))
           (loop (cdr lst) #f)))
       (display ")" port)]
      [else
       (display (guile-value->string form) port)]))

  (define (guile-value->string val)
    (cond
      [(symbol? val) (symbol->string val)]
      [(string? val) (format "~s" val)]
      [(number? val) (number->string val)]
      [(boolean? val) (if val "#t" "#f")]
      [else "#<unknown>"]))

  ;;=======================================================================
  ;; Declaration Conversion

  (define (declaration->ffi-form decl lib-name)
    (cond
      [(function-decl? decl)
       (function->ffi-form decl lib-name)]
      [(struct-decl? decl)
       (struct->ffi-form decl)]
      [(union-decl? decl)
       (union->ffi-form decl)]
      [(enum-decl? decl)
       (enum->ffi-form decl)]
      [(typedef? decl)
       (typedef->ffi-form decl)]
      [else #f]))

  ;;-----------------------------------------------------------------------
  ;; Function FFI

  (define (function->ffi-form decl lib-name)
    (let* ([name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [param-types (map (lambda (p) (ast->ffi-type (param-type p))) params)]
           [guile-name (symbol-append 'c- name)])
      (list 'define guile-name
            (list 'foreign-library-function
                  (string->symbol (string-append "lib" lib-name))
                  (symbol->string name)
                  (string->symbol "#:return-type")
                  (ast->ffi-type return-type)
                  (string->symbol "#:arg-types")
                  (cons 'list param-types)))))

  ;;-----------------------------------------------------------------------
  ;; Struct FFI

  (define (struct->ffi-form decl)
    (let* ([name (struct-decl-name decl)]
           [fields (struct-decl-fields decl)])
      (if (null? fields)
          ;; Opaque struct - just a comment
          `(comment ,(format "Opaque struct: ~a" name))
          ;; Full struct - generate layout using list syntax
          (let ([layout-name (string->symbol
                               (string-append (symbol->string name) "-layout"))])
            `(define ,layout-name
               (list ,@(map (lambda (field)
                             (list 'list
                                   (list 'quote (field-name field))
                                   (ast->ffi-type (field-type field))))
                           fields)))))))

  ;;-----------------------------------------------------------------------
  ;; Union FFI

  (define (union->ffi-form decl)
    ;; Guile doesn't have native union support - just comment
    (let ([name (union-decl-name decl)])
      `(comment ,(format "Union not directly supported: ~a" name))))

  ;;-----------------------------------------------------------------------
  ;; Enum FFI

  (define (enum->ffi-form decl)
    (let ([name (enum-decl-name decl)]
          [enumerators (enum-decl-enumerators decl)])
      (cons 'begin
            (map (lambda (e)
                   (list 'define (enumerator-name e) (enumerator-value e)))
                 enumerators))))

  ;;-----------------------------------------------------------------------
  ;; Typedef FFI

  (define (typedef->ffi-form decl)
    (let ([name (typedef-name decl)])
      `(comment ,(format "typedef ~a" name))))

  ;;=======================================================================
  ;; Type Mapping: C AST -> Guile FFI Types

  ;; ast->ffi-type : type => guile-ffi-type
  ;;   Maps C type AST to Guile FFI type symbol.
  (define (ast->ffi-type type)
    (cond
      ;; Basic types
      [(basic-type? type)
       (basic-type->guile type)]

      ;; Pointer types
      [(pointer-type? type)
       (pointer-type->guile type)]

      ;; Array types (treat as pointers)
      [(array-type? type)
       ''*]

      ;; Function types (function pointers)
      [(function-type? type)
       ''*]

      ;; Named types (struct, union, enum, typedef)
      [(named-type? type)
       (named-type->guile type)]

      ;; Qualified types (const, volatile)
      [(qualified-type? type)
       (ast->ffi-type (qualified-type-type type))]

      ;; Unknown - default to pointer
      [else ''*]))

  (define (basic-type->guile type)
    (case (basic-type-name type)
      [(void) 'void]
      [(char) 'int8]
      [(signed-char) 'int8]
      [(unsigned-char) 'uint8]
      [(short) 'int16]
      [(unsigned-short) 'uint16]
      [(int) 'int]
      [(unsigned unsigned-int) 'unsigned-int]
      [(long) 'long]
      [(unsigned-long) 'unsigned-long]
      [(long-long) 'int64]
      [(unsigned-long-long) 'uint64]
      [(float) 'float]
      [(double) 'double]
      [(long-double) 'double]  ;; Guile doesn't have long double
      [(bool) 'uint8]  ;; bool as uint8
      [else 'int]))

  (define (pointer-type->guile type)
    (let ([pointee (pointer-type-pointee type)])
      (cond
        ;; void* => '*
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'void))
         ''*]
        ;; char* => '* (Guile uses bytevectors for strings)
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'char))
         ''*]
        ;; T* => '*
        [else ''*])))

  (define (named-type->guile type)
    (case (named-type-kind type)
      [(struct union)
       ;; Pointers to structs/unions
       ''*]
      [(enum)
       ;; Enums are integers
       'int]
      [(typedef)
       ;; Try to map known typedefs
       (case (named-type-name type)
         [(int8_t) 'int8]
         [(uint8_t) 'uint8]
         [(int16_t) 'int16]
         [(uint16_t) 'uint16]
         [(int32_t) 'int32]
         [(uint32_t) 'uint32]
         [(int64_t) 'int64]
         [(uint64_t) 'uint64]
         [(size_t) 'size_t]
         [(ssize_t) 'ssize_t]
         [else ''*])]
      [else ''*]))

  ) ;; end library
