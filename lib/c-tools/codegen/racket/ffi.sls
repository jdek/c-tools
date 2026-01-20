;; SPDX-License-Identifier: WTFPL
;; FFI Code Generator - AST to Racket FFI
;; Generates ffi/unsafe bindings for Racket

(library (c-tools codegen racket ffi)
  (export generate-ffi-code
          ast->ffi-type
          declaration->ffi-form)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs io ports)
          (c-tools ast c)
          (only (chezscheme) format pretty-print display))

  ;;=======================================================================
  ;; Main FFI Generation

  ;; generate-ffi-code : list-of-decl string => racket-module
  ;;   effects: none
  ;;   Generates complete Racket FFI module from declarations.
  (define (generate-ffi-code decls lib-name)
    (let* ([forms (filter-map
                    (lambda (decl) (declaration->ffi-form decl lib-name))
                    decls)]
           [exports (extract-exports forms)])
      (racket-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (racket-module-wrapper lib-name exports forms)
    (string-append
      "#lang racket/base\n"
      "(require ffi/unsafe ffi/unsafe/define)\n"
      "(provide " (string-join (map symbol->string exports) " ") ")\n\n"
      "(define lib (ffi-lib \"" lib-name "\"))\n"
      "(define-ffi-definer define-ffi lib)\n\n"
      (string-join (map racket-form->string forms) "\n\n")))

  (define (racket-form->string form)
    (call-with-string-output-port
      (lambda (port)
        (pretty-print-racket form port))))

  (define (pretty-print-racket form port)
    ;; Simple pretty printer for Racket forms
    (cond
      [(pair? form)
       (display "(" port)
       (let loop ([lst form] [first? #t])
         (unless (null? lst)
           (unless first? (display " " port))
           (if (pair? (car lst))
               (pretty-print-racket (car lst) port)
               (display (racket-value->string (car lst)) port))
           (loop (cdr lst) #f)))
       (display ")" port)]
      [else
       (display (racket-value->string form) port)]))

  (define (racket-value->string val)
    (cond
      [(symbol? val)
       (let ([str (symbol->string val)])
         (if (and (> (string-length str) 0)
                  (char=? (string-ref str 0) #\#))
             ;; Symbol starting with # (like #lang) - output as-is
             str
             (symbol->string val)))]
      [(string? val) (format "~s" val)]
      [(number? val) (number->string val)]
      [(boolean? val) (if val "#t" "#f")]
      [else "#<unknown>"]))

  (define (string-join strs sep)
    (if (null? strs)
        ""
        (let loop ([strs (cdr strs)] [result (car strs)])
          (if (null? strs)
              result
              (loop (cdr strs) (string-append result sep (car strs)))))))

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
           [racket-name (symbol-append 'c- name)])
      (list 'define-ffi
            racket-name
            (append '(_fun)
                    param-types
                    (list '-> (ast->ffi-type return-type)))
            (string->symbol "#:c-id")
            (symbol->string name))))

  ;;-----------------------------------------------------------------------
  ;; Struct FFI

  (define (struct->ffi-form decl)
    (let* ([name (struct-decl-name decl)]
           [fields (struct-decl-fields decl)]
           [racket-name (symbol-append '_ name)])
      (if (null? fields)
          ;; Opaque struct - just define pointer type
          `(define-cpointer-type ,racket-name)
          ;; Full struct definition
          `(define-cstruct ,racket-name
             ,(map (lambda (field)
                     (list (field-name field)
                           (ast->ffi-type (field-type field))))
                   fields)))))

  ;;-----------------------------------------------------------------------
  ;; Union FFI

  (define (union->ffi-form decl)
    ;; Racket doesn't have native union support - use opaque pointer
    (let ([name (union-decl-name decl)])
      `(define-cpointer-type ,(symbol-append '_ name))))

  ;;-----------------------------------------------------------------------
  ;; Enum FFI

  (define (enum->ffi-form decl)
    (let ([name (enum-decl-name decl)]
          [enumerators (enum-decl-enumerators decl)])
      (if name
          ;; Named enum - generate constants
          (cons 'begin
                (map (lambda (e)
                       (list 'define (enumerator-name e) (enumerator-value e)))
                     enumerators))
          ;; Anonymous enum
          (cons 'begin
                (map (lambda (e)
                       (list 'define (enumerator-name e) (enumerator-value e)))
                     enumerators)))))

  ;;-----------------------------------------------------------------------
  ;; Typedef FFI

  (define (typedef->ffi-form decl)
    (let ([name (typedef-name decl)]
          [type (typedef-type decl)])
      ;; For now, just define as comment
      `(comment ,(format "typedef ~a" name))))

  ;;=======================================================================
  ;; Type Mapping: C AST -> Racket FFI Types

  ;; ast->ffi-type : type => racket-ffi-type
  ;;   Maps C type AST to Racket FFI type symbol.
  (define (ast->ffi-type type)
    (cond
      ;; Basic types
      [(basic-type? type)
       (basic-type->racket type)]

      ;; Pointer types
      [(pointer-type? type)
       (pointer-type->racket type)]

      ;; Array types (treat as pointers)
      [(array-type? type)
       (let ([elem (array-type-element type)])
         (if (basic-type? elem)
             (case (basic-type-name elem)
               [(char) '_string]  ;; char[] => string
               [else '_pointer])
             '_pointer))]

      ;; Function types (function pointers)
      [(function-type? type)
       '_pointer]

      ;; Named types (struct, union, enum, typedef)
      [(named-type? type)
       (named-type->racket type)]

      ;; Qualified types (const, volatile)
      [(qualified-type? type)
       (ast->ffi-type (qualified-type-type type))]

      ;; Unknown - default to pointer
      [else '_pointer]))

  (define (basic-type->racket type)
    (case (basic-type-name type)
      [(void) '_void]
      [(char) '_int8]
      [(signed-char) '_int8]
      [(unsigned-char) '_uint8]
      [(short) '_short]
      [(unsigned-short) '_ushort]
      [(int) '_int]
      [(unsigned unsigned-int) '_uint]
      [(long) '_long]
      [(unsigned-long) '_ulong]
      [(long-long) '_llong]
      [(unsigned-long-long) '_ullong]
      [(float) '_float]
      [(double) '_double]
      [(long-double) '_double]  ;; Racket doesn't have long double
      [(bool) '_bool]
      [else '_int]))

  (define (pointer-type->racket type)
    (let ([pointee (pointer-type-pointee type)])
      (cond
        ;; void* => _pointer
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'void))
         '_pointer]
        ;; char* => _string (null-terminated)
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'char))
         '_string]
        ;; T* => _pointer (simplified - could use _cpointer)
        [else '_pointer])))

  (define (named-type->racket type)
    (case (named-type-kind type)
      [(struct union)
       ;; struct Foo => _Foo-pointer
       (string->symbol
         (string-append "_" (symbol->string (named-type-name type)) "-pointer"))]
      [(enum)
       ;; enum => just use int
       '_int]
      [(typedef)
       ;; Try to map known typedefs, otherwise pointer
       (case (named-type-name type)
         [(int8_t) '_int8]
         [(uint8_t) '_uint8]
         [(int16_t) '_int16]
         [(uint16_t) '_uint16]
         [(int32_t) '_int32]
         [(uint32_t) '_uint32]
         [(int64_t) '_int64]
         [(uint64_t) '_uint64]
         [(size_t) '_ulong]
         [(ssize_t) '_long]
         [(intptr_t) '_intptr]
         [(uintptr_t) '_uintptr]
         [else '_pointer])]
      [else '_pointer]))

  ;;=======================================================================
  ;; Helpers

  (define (symbol-append . syms)
    (string->symbol
      (apply string-append (map symbol->string syms))))

  (define (filter-map proc lst)
    (let loop ([lst lst] [result '()])
      (cond
        [(null? lst) (reverse result)]
        [else
         (let ([val (proc (car lst))])
           (if val
               (loop (cdr lst) (cons val result))
               (loop (cdr lst) result)))])))

  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              [(and (pair? form) (eq? (car form) 'define-ffi))
               (loop (cdr forms) (cons (cadr form) exports))]
              [(and (pair? form) (eq? (car form) 'define-cstruct))
               ;; Export struct name and pointer type
               (let ([name (cadr form)])
                 (loop (cdr forms)
                       (cons (string->symbol
                               (string-append (symbol->string name) "-pointer"))
                             exports)))]
              [(and (pair? form) (eq? (car form) 'define-cpointer-type))
               (loop (cdr forms) (cons (cadr form) exports))]
              [(and (pair? form) (eq? (car form) 'begin))
               ;; Extract from begin block
               (loop (append (cdr form) (cdr forms)) exports)]
              [(and (pair? form) (eq? (car form) 'define))
               ;; Regular define (enums)
               (loop (cdr forms) (cons (cadr form) exports))]
              [else
               (loop (cdr forms) exports)])))))

  ) ;; end library
