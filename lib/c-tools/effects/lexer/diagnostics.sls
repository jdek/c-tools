;; Lexer Diagnostic Collection Handler
;; Collects warnings and errors during lexing without halting

(library (c-tools effects lexer diagnostics)
  (export with-lexer-diagnostics
          make-lexer-diagnostic lexer-diagnostic?
          lexer-diagnostic-severity lexer-diagnostic-message lexer-diagnostic-location)

  (import (rnrs base)
          (rnrs records syntactic)
          (c-tools effects core))

  ;; lexer-diagnostic : severity message location => diagnostic
  ;;   Represents a diagnostic message from the lexer
  ;;   severity: 'warning or 'error
  ;;   message: string describing the issue
  ;;   location: source location where issue occurred
  (define-record-type lexer-diagnostic
    (fields severity message location))

  ;; with-lexer-diagnostics : (=> values) => values
  ;;   effects: handles lexer-warning and lexer-error effects
  ;;   Collects diagnostics during lexing, appending them to result values
  ;;   Returns: (values original-results... diagnostics-list)
  (define (with-lexer-diagnostics thunk)
    (let ([diagnostics '()])
      (call-with-values
        (lambda ()
          (with-handler 'lexer-warning
            (lambda (data k loop)
              ;; data: (message . location)
              (let ([diag (make-lexer-diagnostic 'warning (car data) (cdr data))])
                (set! diagnostics (cons diag diagnostics)))
              (k #f))

            (with-handler 'lexer-error
              (lambda (data k loop)
                ;; data: (message . location)
                (let ([diag (make-lexer-diagnostic 'error (car data) (cdr data))])
                  (set! diagnostics (cons diag diagnostics)))
                (k #f))

              (thunk))))

        (lambda results
          ;; Append diagnostics list as final result value
          (apply values
            (append results
                    (list (reverse diagnostics)))))))))
