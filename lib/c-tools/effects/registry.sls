;; SPDX-License-Identifier: WTFPL
;; Effect Handler Registry
;; Registration system for composable effect handlers
;;
;;=======================================================================
;; Usage pattern
;;
;; 1. Define a handler wrapper function:
;;    (define (with-my-effect param thunk)
;;      (with-handler 'my-effect
;;        (lambda (data k loop)
;;          (handle-it data param k loop))
;;        (thunk)))
;;
;; 2. Register it:
;;    (register-effect! 'my-effect
;;      (lambda (spec thunk)
;;        (if (pair? spec)
;;            ;; Parameterized: (my-effect arg1 arg2)
;;            (with-my-effect (cadr spec) thunk)
;;            ;; Simple: my-effect
;;            (with-my-effect default-param thunk))))
;;
;; 3. Compose multiple effects:
;;    (with-effects '(cpp-macros cpp-symbols (cpp-include "."))
;;      (lambda ()
;;        (preprocess-file "header.h")))
;;
;;=======================================================================
;; Effect composition order
;;
;; Effects are applied right-to-left (innermost to outermost):
;;   (with-effects '(A B C) thunk)
;; expands to:
;;   (with-A (lambda ()
;;     (with-B (lambda ()
;;       (with-C thunk)))))
;;
;; This means C handles effects first, then B, then A.
;; For cpp effects, typical order is:
;;   '(cpp-symbols cpp-macros ...)
;; So macros (inner) handles cpp-define/cpp-expand,
;; and symbols (outer) can intercept to track state.

(library (c-tools effects registry)
  (export register-effect!
          with-effects
          apply-handlers)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs hashtables)
          (c-tools core conditions))

  ;;=========================================================================
  ;; Registry State

  (define effect-handlers (make-eq-hashtable))

  ;;=========================================================================
  ;; Registration Interface

  ;; register-effect! : symbol procedure => void
  ;;   effects: mutation
  ;;   Registers an effect handler. Handler receives spec and thunk.
  ;;   For simple effects, spec is a symbol.
  ;;   For parameterized effects, spec is a list (name arg ...).
  (define (register-effect! name handler)
    (hashtable-set! effect-handlers name handler))

  ;;=========================================================================
  ;; Composition

  ;; apply-handlers : list procedure => values
  ;;   raises: &cpp-effect-error on unknown effect handler
  ;;   Applies handlers from right to left (innermost to outermost).
  (define (apply-handlers specs thunk)
    (if (null? specs)
        (thunk)
        (let ([spec (car specs)]
              [rest (cdr specs)])
          (let ([name (if (pair? spec) (car spec) spec)])
            (let ([handler (hashtable-ref effect-handlers name #f)])
              (if handler
                  (handler spec (lambda () (apply-handlers rest thunk)))
                  (raise
                    (condition
                      (make-cpp-effect-error name)
                      (make-message-condition
                        (string-append "Unknown effect handler: "
                                      (symbol->string name)))
                      (make-irritants-condition (list name))))))))))

  ;; with-effects : list procedure => values
  ;;   Compose multiple effect handlers into a single call.
  ;;
  ;;   Example:
  ;;     (with-effects '(citations footnotes (includes "."))
  ;;       (lambda () ...))
  ;;
  ;;   Equivalent to:
  ;;     (with-citations
  ;;       (lambda ()
  ;;         (with-footnotes
  ;;           (lambda ()
  ;;             (with-includes "."
  ;;               (lambda () ...))))))
  (define-syntax with-effects
    (syntax-rules ()
      [(_ specs thunk)
       (apply-handlers specs thunk)])))
