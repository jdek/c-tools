;; SPDX-License-Identifier: WTFPL
;; C++ FFI Code Generator - AST to Common Lisp CFFI
;; Generates CFFI bindings with C++ name mangling

(library (c-tools codegen cffi cpp-ffi)
  (export generate-cpp-ffi-code)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs lists)
          (c-tools ast c)
          (c-tools ast cpp)
          (c-tools codegen cffi ffi)
          (c-tools codegen mangle)
          (except (c-tools utility) extract-exports))

  ;;=======================================================================
  ;; Main C++ FFI Generation

  (define (generate-cpp-ffi-code decls lib-name)
    (let* ([forms (declarations->ffi-forms decls '() lib-name)]
           [exports (extract-exports forms)])
      (cffi-cpp-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (cffi-cpp-module-wrapper lib-name exports forms)
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
  ;; Declaration Processing

  (define (declarations->ffi-forms decls namespaces lib-name)
    (let loop ([decls decls] [forms '()])
      (if (null? decls)
          (reverse forms)
          (let ([decl (car decls)])
            (cond
              ;; Namespace - process inner declarations with updated namespace
              [(namespace-decl? decl)
               (let* ([ns-name (namespace-decl-name decl)]
                      [inner-decls (namespace-decl-decls decl)]
                      [new-namespaces (cons ns-name namespaces)]
                      [inner-forms (declarations->ffi-forms inner-decls new-namespaces lib-name)])
                 (loop (cdr decls) (append (reverse inner-forms) forms)))]

              ;; Class - generate opaque pointer + methods
              [(class-decl? decl)
               (let ([class-forms (class->ffi-forms decl namespaces lib-name)])
                 (loop (cdr decls) (append (reverse class-forms) forms)))]

              ;; Template - check if specialization
              [(template-decl? decl)
               (if (null? (template-decl-params decl))
                   ;; Empty params = full specialization - treat as concrete type
                   (let* ([inner-pair (template-decl-decl decl)]
                          [inner-decl (if (pair? inner-pair) (cdr inner-pair) inner-pair)])
                     (cond
                       [(class-decl? inner-decl)
                        (let ([class-forms (class->ffi-forms inner-decl namespaces lib-name)])
                          (loop (cdr decls) (append (reverse class-forms) forms)))]
                       [(function-decl? inner-decl)
                        (let ([form (function->ffi-form inner-decl namespaces #f lib-name)])
                          (loop (cdr decls) (cons form forms)))]
                       [else
                        (loop (cdr decls)
                              (cons `(comment ,(format "Skipping specialized: ~a"
                                                       (template-summary decl)))
                                    forms))]))
                   ;; Non-empty params = generic template, skip
                   (loop (cdr decls)
                         (cons `(comment ,(format "Skipping template: ~a"
                                                  (template-summary decl)))
                               forms)))]

              ;; Function - generate with mangled name
              [(function-decl? decl)
               (let ([form (function->ffi-form decl namespaces #f lib-name)])
                 (loop (cdr decls) (cons form forms)))]

              ;; Other declarations - use base FFI generator
              [(or (typedef? decl) (struct-decl? decl)
                   (union-decl? decl) (enum-decl? decl))
               (let ([form (declaration->ffi-form decl lib-name)])
                 (if form
                     (loop (cdr decls) (cons form forms))
                     (loop (cdr decls) forms)))]

              ;; Unknown - skip
              [else
               (loop (cdr decls) forms)])))))

  ;;=======================================================================
  ;; Class FFI Generation

  (define (class->ffi-forms decl namespaces lib-name)
    (let* ([name (class-decl-name decl)]
           [members (class-decl-members decl)]
           [public-members (filter
                             (lambda (m)
                               (eq? (member-decl-access m) 'public))
                             members)])
      ;; Generate comment for opaque type + public methods
      (cons
        `(comment ,(format "Class ~a (opaque pointer)" name))
        (filter-map
          (lambda (member)
            (member->ffi-form member decl namespaces lib-name))
          public-members))))

  (define (constructor? fn class-name)
    (and (function-decl? fn)
         (eq? (function-decl-name fn) class-name)))

  (define (destructor? fn class-name)
    (and (function-decl? fn)
         (symbol? (function-decl-name fn))
         (let ([name-str (symbol->string (function-decl-name fn))])
           (and (> (string-length name-str) 0)
                (char=? (string-ref name-str 0) #\~)))))

  (define (member->ffi-form member class-decl namespaces lib-name)
    (let ([kind (member-decl-kind member)]
          [class-name (class-decl-name class-decl)])
      (case kind
        [(method)
         (let ([method-decl (member-decl-decl member)])
           ;; Ensure we have a function-decl
           (if (not (function-decl? method-decl))
               #f
               ;; Check if it's a constructor or destructor by name
               (if (or (constructor? method-decl class-name)
                       (destructor? method-decl class-name))
                   #f  ;; Skip for now
                   (function->ffi-form method-decl namespaces class-decl lib-name))))]
        ;; Skip constructors and destructors
        [(constructor destructor) #f]
        [else #f])))

  ;;=======================================================================
  ;; Function/Method FFI Generation

  (define (function->ffi-form decl namespaces class-decl lib-name)
    (let* ([name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [mangled (if class-decl
                        (mangle-method decl
                                      (class-decl-name class-decl)
                                      namespaces
                                      #f)  ;; not const
                        (mangle-function decl namespaces))]
           [lisp-name (if class-decl
                          (string->symbol
                            (string-append (symbol->string (symbol->lisp-name (class-decl-name class-decl)))
                                          "-"
                                          (symbol->string (symbol->lisp-name name))))
                          (string->symbol
                            (string-append "c-" (symbol->string (symbol->lisp-name name)))))]
           [param-types (if class-decl
                           ;; Methods have implicit 'this' pointer
                           (cons ':pointer
                                 (map (lambda (p) (ast->ffi-type (param-type p))) params))
                           (map (lambda (p) (ast->ffi-type (param-type p))) params))])
      (cons 'defcfun
            (cons (list mangled lisp-name)
                  (cons (ast->ffi-type return-type)
                        (let loop ([types param-types] [idx 0])
                          (if (null? types)
                              '()
                              (let ([arg-name (if (and class-decl (= idx 0))
                                                  'this
                                                  (string->symbol
                                                    (string-append "arg" (number->string (if class-decl (- idx 1) idx)))))])
                                (cons (list arg-name (car types))
                                      (loop (cdr types) (+ idx 1)))))))))))

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

  (define (template-summary decl)
    (let ([inner (template-decl-decl decl)])
      (let ([actual-inner (if (pair? inner) (cdr inner) inner)])
        (cond
          [(function-decl? actual-inner) (function-decl-name actual-inner)]
          [(class-decl? actual-inner) (class-decl-name actual-inner)]
          [else "unknown"]))))

  ) ;; end library
