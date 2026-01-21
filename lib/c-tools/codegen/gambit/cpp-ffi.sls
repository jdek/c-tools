;; SPDX-License-Identifier: WTFPL
;; C++ FFI Code Generator - AST to Gambit Scheme FFI
;; Generates c-lambda bindings with C++ name mangling

(library (c-tools codegen gambit cpp-ffi)
  (export generate-cpp-ffi-code)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs io ports)
          (rnrs lists)
          (c-tools ast c)
          (c-tools ast cpp)
          (c-tools codegen gambit ffi)
          (c-tools codegen mangle)
          (c-tools utility))

  ;;=======================================================================
  ;; Main C++ FFI Generation

  (define (generate-cpp-ffi-code decls lib-name)
    (let* ([forms (declarations->ffi-forms decls '() lib-name)]
           [exports (extract-exports forms)])
      (gambit-cpp-module-wrapper lib-name exports forms)))

  ;;=======================================================================
  ;; Module Generation

  (define (gambit-cpp-module-wrapper lib-name exports forms)
    (string-append
      ";; Gambit C++ FFI bindings for " lib-name "\n"
      "(c-declare \"#include <" lib-name ".hpp>\")\n\n"
      (string-join (map gambit-form->string forms) "\n\n")))

  ;;=======================================================================
  ;; Declaration Processing

  (define (declarations->ffi-forms decls namespaces lib-name)
    (let loop ([decls decls] [forms '()])
      (if (null? decls)
          (reverse forms)
          (let ([decl (car decls)])
            (cond
              ;; Namespace
              [(namespace-decl? decl)
               (let* ([ns-name (namespace-decl-name decl)]
                      [inner-decls (namespace-decl-decls decl)]
                      [new-namespaces (cons ns-name namespaces)]
                      [inner-forms (declarations->ffi-forms inner-decls new-namespaces lib-name)])
                 (loop (cdr decls) (append (reverse inner-forms) forms)))]

              ;; Class
              [(class-decl? decl)
               (let ([class-forms (class->ffi-forms decl namespaces)])
                 (loop (cdr decls) (append (reverse class-forms) forms)))]

              ;; Template
              [(template-decl? decl)
               (if (null? (template-decl-params decl))
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
                   (loop (cdr decls)
                         (cons `(comment ,(format "Skipping template: ~a"
                                                  (template-summary decl)))
                               forms)))]

              ;; Function
              [(function-decl? decl)
               (let ([form (function->ffi-form decl namespaces #f)])
                 (loop (cdr decls) (cons form forms)))]

              ;; Other declarations
              [(or (typedef? decl) (struct-decl? decl)
                   (union-decl? decl) (enum-decl? decl))
               (let ([form (declaration->ffi-form decl lib-name)])
                 (if form
                     (loop (cdr decls) (cons form forms))
                     (loop (cdr decls) forms)))]

              [else
               (loop (cdr decls) forms)])))))

  ;;=======================================================================
  ;; Class FFI Generation

  (define (class->ffi-forms decl namespaces)
    (let* ([name (class-decl-name decl)]
           [members (class-decl-members decl)]
           [public-members (filter
                             (lambda (m)
                               (eq? (member-decl-access m) 'public))
                             members)])
      (cons
        `(comment ,(format "Class ~a" name))
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
           (if (not (function-decl? method-decl))
               #f
               (if (or (constructor? method-decl class-name)
                       (destructor? method-decl class-name))
                   #f
                   (function->ffi-form method-decl namespaces class-decl))))]
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
                                      #f)
                        (mangle-function decl namespaces))]
           [gambit-name (if class-decl
                           (string->symbol
                             (string-append (symbol->string (class-decl-name class-decl))
                                           "-"
                                           (symbol->string name)))
                           (string->symbol
                             (string-append "c-" (symbol->string name))))]
           [param-types (if class-decl
                           ;; Methods have implicit 'this' pointer
                           (cons 'pointer
                                 (map (lambda (p) (ast->ffi-type (param-type p))) params))
                           (map (lambda (p) (ast->ffi-type (param-type p))) params))])
      (list 'define gambit-name
            (list 'c-lambda
                  (cons 'list param-types)
                  (ast->ffi-type return-type)
                  mangled))))

  ;;=======================================================================
  ;; Helpers

  (define (gambit-form->string form)
    (call-with-string-output-port
      (lambda (port)
        (pretty-print-gambit form port))))

  (define (pretty-print-gambit form port)
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

  (define (template-summary decl)
    (let ([inner (template-decl-decl decl)])
      (let ([actual-inner (if (pair? inner) (cdr inner) inner)])
        (cond
          [(function-decl? actual-inner) (function-decl-name actual-inner)]
          [(class-decl? actual-inner) (class-decl-name actual-inner)]
          [else "unknown"]))))

  ) ;; end library
