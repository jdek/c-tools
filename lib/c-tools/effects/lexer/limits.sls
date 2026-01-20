;; SPDX-License-Identifier: WTFPL
;; Lexer Security Limit Enforcement via Effects
;; Integrates with core/limits.sls for DoS prevention

(library (c-tools effects lexer limits)
  (export with-lexer-limits)

  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (c-tools core conditions)
          (c-tools core limits)
          (c-tools core tokens)
          (c-tools effects core)
          (only (chezscheme) format))

  ;; with-lexer-limits : (=> values) => values
  ;;   effects: handles check-limit effect
  ;;   Enforces security limits during lexing
  ;;   Raises cpp-limit-exceeded condition when limit is exceeded
  (define (with-lexer-limits thunk)
    (with-handler 'check-limit
      (lambda (data k loop)
        ;; data: (limit-name current-value max-value location)
        (let ([limit-name (car data)]
              [current-value (cadr data)]
              [max-value (caddr data)]
              [location (cadddr data)])
          (when (> current-value max-value)
            ;; Raise exception immediately (security limit)
            (raise
              (condition
                (make-cpp-limit-exceeded limit-name current-value)
                (make-message-condition
                  (format "Limit exceeded: ~a (~a > ~a) at ~a"
                          limit-name current-value max-value
                          (if location
                              (format "~a:~a:~a"
                                      (location-file location)
                                      (location-line location)
                                      (location-column location))
                              "unknown location"))))))
          ;; Limit OK, continue
          (k #f)))

      (thunk))))
