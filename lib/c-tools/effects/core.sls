;; Effect system using delimited continuations
;; Algebraic effects via shift/reset with multi-value handling
;;
;;=======================================================================
;; Effect handler composition pattern
;;
;; When writing effect handlers that need to pass effects to nested handlers,
;; you must use the `loop` parameter. This is critical for proper effect
;; composition.
;;
;; Handler signature: (lambda (data k loop) ...)
;;   data: The effect data (from effect-data)
;;   k: The delimited continuation (resume with result)
;;   loop: Re-enters the handler context for nested effects
;;
;; Pattern 1: Handle and return immediately
;;   (with-handler 'my-effect
;;     (lambda (data k loop)
;;       (k (handle-it data)))
;;     body)
;;
;; Pattern 2: Perform another effect, then continue
;;   (with-handler 'outer-effect
;;     (lambda (data k loop)
;;       (loop (lambda ()
;;         (let ([result (perform (make-effect 'inner-effect data))])
;;           (k result)))))
;;     body)
;;
;; Pattern 3: Intercept and pass through to nested handler
;;   (with-handler 'cpp-define
;;     (lambda (data k loop)
;;       (track-symbol! (car data))
;;       ;; must use loop to let nested handler process it too
;;       (loop (lambda () (k #f))))
;;     body)
;;
;;=======================================================================
;; Why loop is required
;;
;; The `loop` parameter re-enters the reset delimiter, allowing nested
;; effect handlers to see and handle the effects performed within the lambda.
;; Without `loop`, effects performed in the lambda won't be caught by any
;; handler and will escape to the top level.
;;
;; Example: Symbol tracking wraps macro expansion
;;   (with-cpp-symbols        ;; outer handler
;;     (lambda ()
;;       (with-cpp-macros     ;; inner handler
;;         (lambda ()
;;           (define-macro! 'FOO ...)))))
;;
;; When define-macro! performs cpp-define effect:
;; 1. Symbol handler intercepts it first (outer)
;; 2. Calls loop to pass it to inner handler
;; 3. Macro handler receives and processes it
;; 4. Both handlers maintain consistent state

(library (c-tools effects core)
  (export effect make-effect effect? effect-tag effect-data
          perform
          with-handler)
  (import (rnrs base)
          (rnrs records syntactic)
          (c-tools effects control))

  ;; effect : tag data => effect
  ;;   Represents a request for a capability.
  (define-record-type (effect make-effect effect?)
    (fields tag data))

  ;; perform : effect => value
  ;;   effects: depends on effect tag
  ;;   Perform an effect using shift - captures continuation up to nearest handler.
  ;;   Returns effect+continuation pair that bubbles up to the handler.
  (define (perform eff)
    (shift k (cons eff k)))

  ;; with-handler : symbol procedure body ... => values
  ;;   effects: depends on body
  ;;   Handle effects of a specific tag using reset as delimiter.
  ;;   Handler receives: (data continuation loop-procedure).
  ;;   Preserves multiple return values through handler boundaries.
  (define-syntax with-handler
    (syntax-rules ()
      [(_ tag handler-fn body ...)
       (let loop ([thunk (lambda () body ...)])
         (call-with-values
           (lambda () (reset (thunk)))
           (lambda results
             (cond
               ;; Single result that's an effect+continuation pair
               [(and (= (length results) 1)
                     (pair? (car results))
                     (effect? (car (car results)))
                     (procedure? (cdr (car results))))
                (let ([eff (car (car results))]
                      [k (cdr (car results))])
                  (if (eq? (effect-tag eff) tag)
                      ;; Matched - invoke handler with data, continuation, and loop
                      (handler-fn (effect-data eff) k loop)
                      ;; Unhandled - propagate to outer handler using shift
                      (shift k2
                        (cons eff
                              (lambda (v)
                                (k2 (loop (lambda () (k v)))))))))]

               ;; Normal values - return all values
               [else (apply values results)]))))]))
)




