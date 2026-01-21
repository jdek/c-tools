;; SPDX-License-Identifier: WTFPL
;; FFI Code Generator - AST to Gambit Scheme FFI
;; Generates c-lambda bindings

(library (c-tools codegen gambit ffi)
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

  ;; generate-ffi-code : list-of-decl string => gambit-module
  ;;   Generates complete Gambit FFI code from declarations.
  (define (generate-ffi-code decls lib-name)
    (let* ([forms (filter-map
                    (lambda (decl) (declaration->ffi-form decl lib-name))
                    decls)]
           [exports (extract-exports forms)])
      (gambit-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (gambit-module-wrapper lib-name exports forms)
    (string-append
      ";; Gambit FFI bindings for " lib-name "\n"
      "(c-declare \"#include <" lib-name ".h>\")\n\n"
      (string-join (map gambit-form->string forms) "\n\n")))

  (define (gambit-form->string form)
    (call-with-string-output-port
      (lambda (port)
        (pretty-print-gambit form port))))

  (define (pretty-print-gambit form port)
    ;; Simple pretty printer for Gambit forms
    (cond
      [(pair? form)
       (display "(" port)
       (let loop ([lst form] [first? #t])
         (unless (null? lst)
           (unless first? (display " " port))
           (if (pair? (car lst))
               (pretty-print-gambit (car lst) port)
               (display (gambit-value->string (car lst)) port))
           (loop (cdr lst) #f)))
       (display ")" port)]
      [else
       (display (gambit-value->string form) port)]))

  (define (gambit-value->string val)
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
           [gambit-name (symbol-append 'c- name)])
      (list 'define gambit-name
            (list 'c-lambda
                  (cons 'list param-types)
                  (ast->ffi-type return-type)
                  (symbol->string name)))))

  ;;-----------------------------------------------------------------------
  ;; Struct FFI

  (define (struct->ffi-form decl)
    (let* ([name (struct-decl-name decl)]
           [fields (struct-decl-fields decl)])
      `(comment ,(format "Struct ~a - use c-define-type" name))))

  ;;-----------------------------------------------------------------------
  ;; Union FFI

  (define (union->ffi-form decl)
    (let ([name (union-decl-name decl)])
      `(comment ,(format "Union ~a - use c-define-type" name))))

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
  ;; Type Mapping: C AST -> Gambit FFI Types

  ;; ast->ffi-type : type => gambit-ffi-type
  ;;   Maps C type AST to Gambit FFI type symbol.
  (define (ast->ffi-type type)
    (cond
      ;; Basic types
      [(basic-type? type)
       (basic-type->gambit type)]

      ;; Pointer types
      [(pointer-type? type)
       (pointer-type->gambit type)]

      ;; Array types (treat as pointers)
      [(array-type? type)
       'pointer]

      ;; Function types (function pointers)
      [(function-type? type)
       'pointer]

      ;; Named types (struct, union, enum, typedef)
      [(named-type? type)
       (named-type->gambit type)]

      ;; Qualified types (const, volatile)
      [(qualified-type? type)
       (ast->ffi-type (qualified-type-type type))]

      ;; Unknown - default to pointer
      [else 'pointer]))

  (define (basic-type->gambit type)
    (case (basic-type-name type)
      [(void) 'void]
      [(char) 'char]
      [(signed-char) 'signed-char]
      [(unsigned-char) 'unsigned-char]
      [(short) 'short]
      [(unsigned-short) 'unsigned-short]
      [(int) 'int]
      [(unsigned unsigned-int) 'unsigned-int]
      [(long) 'long]
      [(unsigned-long) 'unsigned-long]
      [(long-long) 'long-long]
      [(unsigned-long-long) 'unsigned-long-long]
      [(float) 'float]
      [(double) 'double]
      [(long-double) 'double]
      [(bool) 'bool]
      [else 'int]))

  (define (pointer-type->gambit type)
    (let ([pointee (pointer-type-pointee type)])
      (cond
        ;; void* => pointer
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'void))
         'pointer]
        ;; char* => UTF-8-string or nonnull-UTF-8-string
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'char))
         'nonnull-UTF-8-string]
        ;; T* => pointer
        [else 'pointer])))

  (define (named-type->gambit type)
    (case (named-type-kind type)
      [(struct union)
       'pointer]
      [(enum)
       'int]
      [(typedef)
       ;; Try to map known typedefs
       (case (named-type-name type)
         [(int8_t) 'int8]
         [(uint8_t) 'unsigned-int8]
         [(int16_t) 'int16]
         [(uint16_t) 'unsigned-int16]
         [(int32_t) 'int32]
         [(uint32_t) 'unsigned-int32]
         [(int64_t) 'int64]
         [(uint64_t) 'unsigned-int64]
         [(size_t) 'size_t]
         [(ssize_t) 'ssize_t]
         [else 'pointer])]
      [else 'pointer]))

  ) ;; end library
