;; SPDX-License-Identifier: WTFPL
;; C++ FFI Code Generator - AST to Racket FFI
;; Generates ffi/unsafe bindings for Racket with C++ name mangling

(library (c-tools codegen racket cpp-ffi)
  (export generate-cpp-ffi-code)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs lists)
          (rnrs mutable-pairs)
          (c-tools ast c)
          (c-tools ast cpp)
          (c-tools codegen racket ffi)
          (c-tools codegen mangle)
          (except (c-tools utility) extract-exports))

  ;;=======================================================================
  ;; Main C++ FFI Generation

  ;; generate-cpp-ffi-code : list-of-decl string => racket-module
  ;;   Generates complete Racket FFI module from C++ declarations.
  (define (generate-cpp-ffi-code decls lib-name)
    (let* ([forms (declarations->ffi-forms decls '() lib-name)]
           [exports (extract-exports forms)])
      (racket-cpp-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (racket-cpp-module-wrapper lib-name exports forms)
    (string-append
      "#lang racket/base\n"
      "(require ffi/unsafe ffi/unsafe/define)\n"
      "(provide " (string-join (map symbol->string exports) " ") ")\n\n"
      (format "(define lib (ffi-lib ~s))\n" lib-name)
      "(define-ffi-definer define-ffi lib)\n\n"
      (string-join (map racket-form->string forms) "\n\n")))

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
               (let ([class-forms (class->ffi-forms decl namespaces)])
                 (loop (cdr decls) (append (reverse class-forms) forms)))]

              ;; Template - check if specialization
              [(template-decl? decl)
               (if (null? (template-decl-params decl))
                   ;; Empty params = full specialization - treat as concrete type
                   (let* ([inner-pair (template-decl-decl decl)]
                          [inner-decl (if (pair? inner-pair) (cdr inner-pair) inner-pair)])
                     (cond
                       [(class-decl? inner-decl)
                        (let ([class-forms (class->ffi-forms inner-decl namespaces)])
                          (loop (cdr decls) (append (reverse class-forms) forms)))]
                       [(function-decl? inner-decl)
                        (let ([form (function->ffi-form inner-decl namespaces #f)])
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
               (let ([form (function->ffi-form decl namespaces #f)])
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

  (define (class->ffi-forms decl namespaces)
    (let* ([name (class-decl-name decl)]
           [members (class-decl-members decl)]
           [ptr-type (string->symbol (string-append (symbol->string name) "-ptr"))]
           [public-members (filter
                             (lambda (m)
                               (eq? (member-decl-access m) 'public))
                             members)])
      ;; Generate opaque pointer type + public methods
      (cons
        (list 'define-cpointer-type ptr-type)
        (filter-map
          (lambda (member)
            (member->ffi-form member decl namespaces))
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

  (define (member->ffi-form member class-decl namespaces)
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
                   (function->ffi-form method-decl namespaces class-decl))))]
        ;; Skip constructors and destructors
        [(constructor destructor) #f]
        [else #f])))

  ;;=======================================================================
  ;; Function/Method FFI Generation

  (define (function->ffi-form decl namespaces class-decl)
    (let* ([name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [mangled (if class-decl
                        (mangle-method decl
                                      (class-decl-name class-decl)
                                      namespaces
                                      #f)  ;; not const - would need to check member specifiers
                        (mangle-function decl namespaces))]
           [racket-name (if class-decl
                           (string->symbol
                             (string-append (symbol->string (class-decl-name class-decl))
                                           "-"
                                           (symbol->string name)))
                           (string->symbol
                             (string-append "c-" (symbol->string name))))]
           [param-types (if class-decl
                           ;; Methods have implicit 'this' pointer
                           (cons (string->symbol
                                   (string-append (symbol->string (class-decl-name class-decl)) "-ptr"))
                                 (map (lambda (p) (ast->ffi-type (param-type p))) params))
                           (map (lambda (p) (ast->ffi-type (param-type p))) params))])
      (list 'define-ffi
            racket-name
            (append '(_fun) param-types (list '-> (ast->ffi-type return-type)))
            (string->symbol "#:c-id")
            mangled)))

  ;;=======================================================================
  ;; Helpers

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
      [(symbol? val) (symbol->string val)]
      [(string? val) (format "~s" val)]
      [(number? val) (number->string val)]
      [(boolean? val) (if val "#t" "#f")]
      [else "#<unknown>"]))

  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              [(and (pair? form) (eq? (car form) 'define-ffi))
               (loop (cdr forms) (cons (cadr form) exports))]
              [(and (pair? form) (eq? (car form) 'define-cpointer-type))
               (loop (cdr forms) (cons (cadr form) exports))]
              [(and (pair? form) (eq? (car form) 'begin))
               ;; Extract from begin block (enums)
               (loop (append (cdr form) (cdr forms)) exports)]
              [(and (pair? form) (eq? (car form) 'define))
               ;; Regular define (enum values)
               (loop (cdr forms) (cons (cadr form) exports))]
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
