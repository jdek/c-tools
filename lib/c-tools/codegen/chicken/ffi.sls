;; SPDX-License-Identifier: WTFPL
;; FFI Code Generator - AST to Chicken Scheme FFI
;; Generates foreign-lambda bindings

(library (c-tools codegen chicken ffi)
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

  ;; generate-ffi-code : list-of-decl string => chicken-module
  ;;   Generates complete Chicken FFI module from declarations.
  (define (generate-ffi-code decls lib-name)
    (let* ([forms (filter-map
                    (lambda (decl) (declaration->ffi-form decl lib-name))
                    decls)]
           [exports (extract-exports forms)])
      (chicken-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (chicken-module-wrapper lib-name exports forms)
    (string-append
      "(module " lib-name " (" (string-join (map symbol->string exports) " ") ")\n"
      "  (import scheme chicken foreign)\n\n"
      "  (foreign-declare \"#include <" lib-name ".h>\")\n\n"
      (string-join (map chicken-form->string forms) "\n\n")
      "\n)\n"))

  (define (chicken-form->string form)
    (call-with-string-output-port
      (lambda (port)
        (pretty-print-chicken form port))))

  (define (pretty-print-chicken form port)
    ;; Simple pretty printer for Chicken forms
    (cond
      [(pair? form)
       (display "(" port)
       (let loop ([lst form] [first? #t])
         (unless (null? lst)
           (unless first? (display " " port))
           (if (pair? (car lst))
               (pretty-print-chicken (car lst) port)
               (display (chicken-value->string (car lst)) port))
           (loop (cdr lst) #f)))
       (display ")" port)]
      [else
       (display (chicken-value->string form) port)]))

  (define (chicken-value->string val)
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
           [chicken-name (symbol-append 'c- name)])
      (list 'define chicken-name
            (cons 'foreign-lambda
                  (cons (ast->ffi-type return-type)
                        (cons (symbol->string name)
                              param-types))))))

  ;;-----------------------------------------------------------------------
  ;; Struct FFI

  (define (struct->ffi-form decl)
    (let* ([name (struct-decl-name decl)]
           [fields (struct-decl-fields decl)])
      (if (null? fields)
          ;; Opaque struct - just a comment
          `(comment ,(format "Opaque struct: ~a" name))
          ;; Full struct - Chicken doesn't have define-cstruct, use c-struct
          `(comment ,(format "Struct ~a - manual binding needed" name)))))

  ;;-----------------------------------------------------------------------
  ;; Union FFI

  (define (union->ffi-form decl)
    (let ([name (union-decl-name decl)])
      `(comment ,(format "Union ~a - manual binding needed" name))))

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
  ;; Type Mapping: C AST -> Chicken FFI Types

  ;; ast->ffi-type : type => chicken-ffi-type
  ;;   Maps C type AST to Chicken FFI type symbol.
  (define (ast->ffi-type type)
    (cond
      ;; Basic types
      [(basic-type? type)
       (basic-type->chicken type)]

      ;; Pointer types
      [(pointer-type? type)
       (pointer-type->chicken type)]

      ;; Array types (treat as pointers)
      [(array-type? type)
       'c-pointer]

      ;; Function types (function pointers)
      [(function-type? type)
       'c-pointer]

      ;; Named types (struct, union, enum, typedef)
      [(named-type? type)
       (named-type->chicken type)]

      ;; Qualified types (const, volatile)
      [(qualified-type? type)
       (ast->ffi-type (qualified-type-type type))]

      ;; Unknown - default to pointer
      [else 'c-pointer]))

  (define (basic-type->chicken type)
    (case (basic-type-name type)
      [(void) 'void]
      [(char) 'char]
      [(signed-char) 'byte]
      [(unsigned-char) 'unsigned-byte]
      [(short) 'short]
      [(unsigned-short) 'unsigned-short]
      [(int) 'int]
      [(unsigned unsigned-int) 'unsigned-int]
      [(long) 'long]
      [(unsigned-long) 'unsigned-long]
      [(long-long) 'integer64]
      [(unsigned-long-long) 'unsigned-integer64]
      [(float) 'float]
      [(double) 'double]
      [(long-double) 'double]
      [(bool) 'bool]
      [else 'int]))

  (define (pointer-type->chicken type)
    (let ([pointee (pointer-type-pointee type)])
      (cond
        ;; void* => c-pointer
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'void))
         'c-pointer]
        ;; char* => c-string
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'char))
         'c-string]
        ;; T* => c-pointer
        [else 'c-pointer])))

  (define (named-type->chicken type)
    (case (named-type-kind type)
      [(struct union)
       'c-pointer]
      [(enum)
       'int]
      [(typedef)
       ;; Try to map known typedefs
       (case (named-type-name type)
         [(int8_t) 'byte]
         [(uint8_t) 'unsigned-byte]
         [(int16_t) 'short]
         [(uint16_t) 'unsigned-short]
         [(int32_t) 'integer32]
         [(uint32_t) 'unsigned-integer32]
         [(int64_t) 'integer64]
         [(uint64_t) 'unsigned-integer64]
         [(size_t) 'size_t]
         [(ssize_t) 'ssize_t]
         [else 'c-pointer])]
      [else 'c-pointer]))

  ) ;; end library
