;; SPDX-License-Identifier: WTFPL
;; Template Engine for FFI Backend DSL
;; Generates s-expressions from templates

(library (c-tools codegen dsl template)
  (export expand-template
          template-lambda)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs syntax-case))

  ;;=======================================================================
  ;; Template Macros

  ;; template-lambda : (var ...) body => lambda
  ;;   Macro for writing templates that extract variables from environment.
  ;;   Usage: (template-lambda (name type) `(define ,name ,type))
  ;;   Expands to: (lambda (env)
  ;;                 (let ([name (cdr (assoc 'name env))]
  ;;                       [type (cdr (assoc 'type env))])
  ;;                   `(define ,name ,type)))
  (define-syntax template-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ (var ...) body)
         #'(lambda (env)
             (let ([var (cdr (assoc 'var env))] ...)
               body))])))

  ;;=======================================================================
  ;; Template Expansion

  ;; expand-template : template alist => s-expression
  ;;   Expands template (just calls it if it's a procedure).
  (define (expand-template template env)
    (if (procedure? template)
        (template env)
        template))


) ;; end library
