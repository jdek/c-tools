;; SPDX-License-Identifier: WTFPL
;; DSL Core - define-backend macro and backend registry

(library (c-tools codegen dsl core)
  (export define-backend
          get-backend
          register-backend!)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs syntax-case))

  ;;=======================================================================
  ;; Backend Registry

  ;; Global registry of compiled backends
  (define *backends* '())

  ;; register-backend! : symbol backend-spec => void
  ;;   effects: modifies *backends*
  ;;   Registers a compiled backend.
  (define (register-backend! name spec)
    (set! *backends* (cons (cons name spec) *backends*)))

  ;; get-backend : symbol => backend-spec
  ;;   raises: if backend not found
  ;;   Retrieves backend spec by name.
  (define (get-backend name)
    (let ([entry (assoc name *backends*)])
      (if entry
          (cdr entry)
          (error 'get-backend "unknown backend: ~a" name))))

  ;;=======================================================================
  ;; define-backend Macro

  ;; define-backend : syntax
  ;;   Defines a backend with declarative spec.
  ;;   Syntax:
  ;;     (define-backend name
  ;;       #:module-header (template "...")
  ;;       #:type-map [(pattern → result) ...]
  ;;       #:declarations [(decl-type #:template "..." ...) ...]
  ;;       #:custom-handlers [(pred? handler) ...])
  (define-syntax define-backend
    (lambda (stx)
      (syntax-case stx ()
        [(_ name clause ...)
         #'(begin
             (define backend-spec
               (compile-backend-spec 'name '(clause ...)))
             (register-backend! 'name backend-spec))])))

  ;; compile-backend-spec : symbol list => backend-spec
  ;;   Compiles backend definition clauses into runtime spec.
  (define (compile-backend-spec name clauses)
    (let ([spec (make-empty-spec name)])
      (process-clauses clauses spec)))

  ;; make-empty-spec : symbol => backend-spec
  ;;   Creates empty backend spec structure.
  (define (make-empty-spec name)
    (list (cons 'name name)
          (cons 'module-header #f)
          (cons 'type-map '())
          (cons 'declarations '())
          (cons 'custom-handlers '())))

  ;; process-clauses : list backend-spec => backend-spec
  ;;   Processes definition clauses, updating spec.
  (define (process-clauses clauses spec)
    (if (null? clauses)
        spec
        (let ([clause (car clauses)])
          (cond
            [(and (pair? clause) (eq? (car clause) '#:module-header))
             (process-clauses (cdr clauses)
                             (update-spec spec 'module-header (cadr clause)))]

            [(and (pair? clause) (eq? (car clause) '#:type-map))
             (process-clauses (cdr clauses)
                             (update-spec spec 'type-map (cdr clause)))]

            [(and (pair? clause) (eq? (car clause) '#:declarations))
             (process-clauses (cdr clauses)
                             (update-spec spec 'declarations (cdr clause)))]

            [(and (pair? clause) (eq? (car clause) '#:custom-handlers))
             (process-clauses (cdr clauses)
                             (update-spec spec 'custom-handlers (cdr clause)))]

            [else
             (error 'process-clauses "unknown clause: ~a" clause)]))))

  ;; update-spec : backend-spec symbol value => backend-spec
  ;;   Updates field in backend spec.
  (define (update-spec spec key value)
    (map (lambda (entry)
           (if (eq? (car entry) key)
               (cons key value)
               entry))
         spec))

  ;; get-spec-field : backend-spec symbol => value
  ;;   Retrieves field from backend spec.
  (define (get-spec-field spec key)
    (let ([entry (assoc key spec)])
      (if entry
          (cdr entry)
          (error 'get-spec-field "missing field: ~a" key))))

) ;; end library
