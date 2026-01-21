;; SPDX-License-Identifier: WTFPL
;; FFI Code Generator - AST to Common Lisp CFFI
;; Generates CFFI bindings for Common Lisp

(library (c-tools codegen cffi ffi)
  (export generate-ffi-code
          ast->ffi-type
          declaration->ffi-form)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs lists)
          (rnrs io ports)
          (c-tools ast c)
          (except (c-tools utility) extract-exports))

  ;;=======================================================================
  ;; Main FFI Generation

  ;; generate-ffi-code : list-of-decl string => string
  ;;   Generates complete CFFI package from declarations.
  (define (generate-ffi-code decls lib-name)
    (let* ([forms (filter-map
                    (lambda (decl) (declaration->ffi-form decl lib-name))
                    decls)]
           [exports (extract-exports forms)])
      (cffi-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (cffi-module-wrapper lib-name exports forms)
    (call-with-string-output-port
      (lambda (port)
        ;; Package definition
        (put-string port "(defpackage #:ffi-")
        (put-string port lib-name)
        (newline port)
        (put-string port "  (:use #:cl #:cffi)")
        (newline port)
        (put-string port "  (:export")
        (for-each (lambda (sym)
                    (put-string port " #:")
                    (put-string port (symbol->string sym)))
                  exports)
        (put-string port "))")
        (newline port)
        (newline port)

        ;; Package switch
        (put-string port "(in-package #:ffi-")
        (put-string port lib-name)
        (put-string port ")")
        (newline port)
        (newline port)

        ;; Library loading
        (put-string port "(define-foreign-library lib")
        (put-string port lib-name)
        (newline port)
        (put-string port "  (:darwin \"lib")
        (put-string port lib-name)
        (put-string port ".dylib\")")
        (newline port)
        (put-string port "  (:unix \"lib")
        (put-string port lib-name)
        (put-string port ".so\")")
        (newline port)
        (put-string port "  (t (:default \"lib")
        (put-string port lib-name)
        (put-string port "\")))")
        (newline port)
        (newline port)
        (put-string port "(use-foreign-library lib")
        (put-string port lib-name)
        (put-string port ")")
        (newline port)
        (newline port)

        ;; Forms
        (for-each (lambda (form)
                    (cffi-form->port form port)
                    (newline port)
                    (newline port))
                  forms))))

  (define (cffi-form->port form port)
    (cond
      [(pair? form)
       (put-char port #\()
       (let loop ([lst form] [first? #t])
         (unless (null? lst)
           (unless first? (put-char port #\space))
           (if (pair? (car lst))
               (cffi-form->port (car lst) port)
               (put-string port (cffi-value->string (car lst))))
           (loop (cdr lst) #f)))
       (put-char port #\))]
      [else
       (put-string port (cffi-value->string form))]))

  (define (cffi-value->string val)
    (cond
      [(symbol? val)
       (let ([str (symbol->string val)])
         (if (and (> (string-length str) 0)
                  (char=? (string-ref str 0) #\:))
             str  ;; Already a keyword
             str))]
      [(string? val) (format "~s" val)]
      [(number? val) (number->string val)]
      [(boolean? val) (if val "t" "nil")]
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
           [lisp-name (symbol->lisp-name name)])
      (cons 'defcfun
            (cons (list (symbol->string name) lisp-name)
                  (cons (ast->ffi-type return-type)
                        (let loop ([ps params] [idx 0])
                          (if (null? ps)
                              '()
                              (let* ([p (car ps)]
                                     [pname (param-name p)]
                                     [arg-name (if pname
                                                   (symbol->lisp-name pname)
                                                   (string->symbol
                                                     (string-append "arg" (number->string idx))))])
                                (cons (list arg-name (ast->ffi-type (param-type p)))
                                      (loop (cdr ps) (+ idx 1)))))))))))

  ;;-----------------------------------------------------------------------
  ;; Struct FFI

  (define (struct->ffi-form decl)
    (let* ([name (struct-decl-name decl)]
           [fields (struct-decl-fields decl)])
      (if (null? fields)
          ;; Opaque struct - just a comment
          `(comment ,(format "Opaque struct: ~a" name))
          ;; Full struct - generate defcstruct
          (cons 'defcstruct
                (cons (symbol->lisp-name name)
                      (map (lambda (field)
                             (list (symbol->lisp-name (field-name field))
                                   (ast->ffi-type (field-type field))))
                           fields))))))

  ;;-----------------------------------------------------------------------
  ;; Union FFI

  (define (union->ffi-form decl)
    ;; CFFI has defcunion but it's complex - skip for Phase 1
    (let ([name (union-decl-name decl)])
      `(comment ,(format "Union not supported: ~a (use defcunion manually)" name))))

  ;;-----------------------------------------------------------------------
  ;; Enum FFI

  (define (enum->ffi-form decl)
    (let ([name (enum-decl-name decl)]
          [enumerators (enum-decl-enumerators decl)])
      (cons 'defcenum
            (cons (symbol->lisp-name name)
                  (map (lambda (e)
                         (list (symbol->keyword (enumerator-name e))
                               (enumerator-value e)))
                       enumerators)))))

  ;;-----------------------------------------------------------------------
  ;; Typedef FFI

  (define (typedef->ffi-form decl)
    (let ([name (typedef-name decl)])
      `(comment ,(format "typedef ~a" name))))

  ;;=======================================================================
  ;; Type Mapping: C AST -> CFFI Types

  ;; ast->ffi-type : type => cffi-type
  ;;   Maps C type AST to CFFI type keyword.
  (define (ast->ffi-type type)
    (cond
      ;; Basic types
      [(basic-type? type)
       (basic-type->cffi type)]

      ;; Pointer types
      [(pointer-type? type)
       (pointer-type->cffi type)]

      ;; Array types (treat as pointers)
      [(array-type? type)
       ':pointer]

      ;; Function types (function pointers)
      [(function-type? type)
       ':pointer]

      ;; Named types (struct, union, enum, typedef)
      [(named-type? type)
       (named-type->cffi type)]

      ;; Qualified types (const, volatile)
      [(qualified-type? type)
       (ast->ffi-type (qualified-type-type type))]

      ;; Unknown - default to pointer
      [else ':pointer]))

  (define (basic-type->cffi type)
    (case (basic-type-name type)
      [(void) ':void]
      [(char) ':char]
      [(signed-char) ':int8]
      [(unsigned-char) ':uint8]
      [(short) ':short]
      [(unsigned-short) ':unsigned-short]
      [(int) ':int]
      [(unsigned unsigned-int) ':unsigned-int]
      [(long) ':long]
      [(unsigned-long) ':unsigned-long]
      [(long-long) ':long-long]
      [(unsigned-long-long) ':unsigned-long-long]
      [(float) ':float]
      [(double) ':double]
      [(long-double) ':long-double]
      [(bool _Bool) ':boolean]
      [else ':int]))

  (define (pointer-type->cffi type)
    (let ([pointee (pointer-type-pointee type)])
      (cond
        ;; void* => :pointer
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'void))
         ':pointer]
        ;; char* => :string
        [(and (basic-type? pointee)
              (eq? (basic-type-name pointee) 'char))
         ':string]
        ;; T* => :pointer
        [else ':pointer])))

  (define (named-type->cffi type)
    (case (named-type-kind type)
      [(struct union)
       ;; Pointers to structs/unions - use :pointer for now
       ':pointer]
      [(enum)
       ;; Enums are integers
       ':int]
      [(typedef)
       ;; Try to map known typedefs
       (case (named-type-name type)
         [(int8_t) ':int8]
         [(uint8_t) ':uint8]
         [(int16_t) ':int16]
         [(uint16_t) ':uint16]
         [(int32_t) ':int32]
         [(uint32_t) ':uint32]
         [(int64_t) ':int64]
         [(uint64_t) ':uint64]
         [(size_t) ':size]
         [(ssize_t) ':ssize]
         [else ':pointer])]
      [else ':pointer]))

  ;;=======================================================================
  ;; Helpers

  ;; symbol->lisp-name : symbol => symbol
  ;;   Converts C identifier to Lisp naming convention (underscore to hyphen).
  (define (symbol->lisp-name sym)
    (string->symbol
      (list->string
        (map (lambda (c)
               (if (char=? c #\_) #\- c))
             (string->list (symbol->string sym))))))

  ;; symbol->keyword : symbol => symbol
  ;;   Converts symbol to keyword (:symbol).
  (define (symbol->keyword sym)
    (string->symbol
      (string-append ":" (symbol->string (symbol->lisp-name sym)))))

  ;; extract-exports : list-of-form => list-of-symbol
  ;;   Extracts export names from FFI forms.
  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              ;; defcfun - export the lisp name
              [(and (pair? form) (eq? (car form) 'defcfun))
               (let ([names (cadr form)])
                 (if (pair? names)
                     (loop (cdr forms) (cons (cadr names) exports))
                     (loop (cdr forms) exports)))]
              ;; defcstruct - export the struct name
              [(and (pair? form) (eq? (car form) 'defcstruct))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; defcenum - export the enum name
              [(and (pair? form) (eq? (car form) 'defcenum))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; comment - skip
              [(and (pair? form) (eq? (car form) 'comment))
               (loop (cdr forms) exports)]
              [else
               (loop (cdr forms) exports)])))))

  ) ;; end library
