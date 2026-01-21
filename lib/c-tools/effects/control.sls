;; SPDX-License-Identifier: WTFPL
;; Delimited Continuations (shift/reset)
;; Filinski meta-continuation approach

(library (c-tools effects control)
  (export reset shift)
  (import (rnrs base)
          (rnrs records syntactic)
          (only (c-tools utility) make-parameter))

  ;;===========================================================================
  ;; Multiple value wrapper for passing through call/cc continuations

  (define-record-type multi-vals
    (fields vals))

  ;;===========================================================================
  ;; Meta-continuation
  ;;
  ;; Represents "the rest of the computation" outside all resets.
  ;; Initially identity function, saved/restored by reset.
  ;; Using parameter makes it thread-local.

  (define *meta-k* (make-parameter (lambda (v) v)))

  ;;=========================================================================
  ;; Exported

  ;; reset : body ... => values
  ;;   Establishes a delimiter/prompt.
  ;;   Uses call/cc to capture the delimiter boundary. When shift aborts,
  ;;   it invokes the meta-continuation which jumps back here.
  (define-syntax reset
    (syntax-rules ()
      [(reset body ...)
       (let ([saved-mc (*meta-k*)])
         (let ([result
                (call/cc
                  (lambda (k-reset)
                    ;; Set meta-k to function that restores and jumps to delimiter
                    ;; Package multiple values in wrapper for continuations
                    (*meta-k* (lambda args
                               (*meta-k* saved-mc)
                               (k-reset (if (= (length args) 1)
                                           (car args)
                                           (make-multi-vals args)))))
                    ;; Evaluate body - if shift aborts, meta-k will jump back to k-reset
                    (call-with-values
                      (lambda () body ...)
                      (lambda result-list
                        ;; Body completed normally - restore meta-k and return values
                        (*meta-k* saved-mc)
                        (if (= (length result-list) 1)
                            (car result-list)
                            (make-multi-vals result-list))))))])
           ;; Unpack multi-value wrapper if present
           (if (multi-vals? result)
               (apply values (multi-vals-vals result))
               result)))]))

  ;; shift : k body ... => values
  ;;   Captures delimited continuation up to nearest reset.
  ;;   The captured continuation k, when invoked, re-installs the reset.
  ;;   This provides proper delimited continuation semantics.
  (define-syntax shift
    (syntax-rules ()
      [(shift k body ...)
       (call/cc
         (lambda (k-internal)
           (let ([saved-mc (*meta-k*)])
             ;; Replace meta-k with continuation that restores and returns to reset
             ;; Package multiple values in wrapper for continuations
             (*meta-k*
               (lambda args
                 (*meta-k* saved-mc)
                 ;; Package multiple args in wrapper
                 (k-internal (if (= (length args) 1)
                                (car args)
                                (make-multi-vals args)))))
             ;; Capture the meta-k we just set - this represents the delimited continuation
             (let* ([captured-mc (*meta-k*)]
                    [k (lambda args
                         ;; Re-wrap in reset for proper delimited semantics
                         (reset (apply captured-mc args)))])
               ;; Invoke the shift body with the continuation
               ;; Result goes to saved-mc (which returns to enclosing reset)
               (let ([result (begin body ...)])
                 (if (multi-vals? result)
                     (apply saved-mc (multi-vals-vals result))
                     (saved-mc result)))))))]))
)
