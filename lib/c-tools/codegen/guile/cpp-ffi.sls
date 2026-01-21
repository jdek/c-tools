;; SPDX-License-Identifier: WTFPL
;; C++ FFI Code Generator - AST to Guile FFI
;; Generates bindings using (system foreign) with C++ name mangling

(library (c-tools codegen guile cpp-ffi)
  (export generate-cpp-ffi-code)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs lists)
          (c-tools ast c)
          (c-tools ast cpp)
          (c-tools codegen guile ffi)
          (c-tools codegen mangle)
          (c-tools utility))

  ;;=======================================================================
  ;; Main C++ FFI Generation

  (define (generate-cpp-ffi-code decls lib-name)
    (let* ([forms (declarations->ffi-forms decls '() lib-name)]
           [exports (extract-exports forms)])
      (guile-cpp-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (guile-cpp-module-wrapper lib-name exports forms)
    (string-append
      "(define-module (ffi " lib-name ")\n"
      "  #:use-module (system foreign)\n"
      "  #:use-module (system foreign-library)\n"
      "  #:export (" (string-join (map symbol->string exports) "\n           ") "))\n\n"
      "(define-foreign-library lib" lib-name "\n"
      "  (dynamic-link \"" lib-name "\"))\n\n"
      (string-join (map guile-form->string forms) "\n\n")))

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
           [guile-name (if class-decl
                          (string->symbol
                            (string-append (symbol->string (class-decl-name class-decl))
                                          "-"
                                          (symbol->string name)))
                          (string->symbol
                            (string-append "c-" (symbol->string name))))]
           [param-types (if class-decl
                           ;; Methods have implicit 'this' pointer
                           (cons ''*
                                 (map (lambda (p) (ast->ffi-type (param-type p))) params))
                           (map (lambda (p) (ast->ffi-type (param-type p))) params))])
      (list 'define guile-name
            (list 'foreign-library-function
                  (string->symbol (string-append "lib" lib-name))
                  mangled
                  (string->symbol "#:return-type")
                  (ast->ffi-type return-type)
                  (string->symbol "#:arg-types")
                  (cons 'list param-types)))))

  ;;=======================================================================
  ;; Helpers

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

  (define (template-summary decl)
    (let ([inner (template-decl-decl decl)])
      (let ([actual-inner (if (pair? inner) (cdr inner) inner)])
        (cond
          [(function-decl? actual-inner) (function-decl-name actual-inner)]
          [(class-decl? actual-inner) (class-decl-name actual-inner)]
          [else "unknown"]))))

  ) ;; end library
