;; SPDX-License-Identifier: WTFPL
;; C Preprocessor Diagnostic Collection Handler
;; Collects warnings and errors during preprocessing

(library (c-tools effects cpp diagnostics)
  (export with-cpp-diagnostics)
  (import (rnrs base)
          (c-tools effects core)
          (c-tools effects registry))

  ;; Diagnostic collection handler
  ;; Collects warnings and errors, appending them to the result values
  ;; Follows the citations/footnotes pattern from other handlers
  (define (with-cpp-diagnostics thunk)
    (let ([warnings '()]
          [errors '()])

      (call-with-values
        (lambda ()
          (with-handler 'cpp-warning
            (lambda (data k loop)
              (set! warnings (cons data warnings))
              (k #f))

            (with-handler 'cpp-error
              (lambda (data k loop)
                (set! errors (cons data errors))
                (k #f))

              (thunk))))

        (lambda results
          ;; Append diagnostics to results
          ;; Returns: original-results + (list warnings) + (list errors)
          (apply values
            (append results
                    (list (reverse warnings))
                    (list (reverse errors))))))))

  ;;=========================================================================
  ;; Registration

  (register-effect! 'cpp-diagnostics
    (lambda (spec thunk)
      (with-cpp-diagnostics thunk)))
)
