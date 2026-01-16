;; C Preprocessor and Parser Condition Hierarchy
;; Follows R6RS exception patterns for consistent error reporting

(library (c-tools core conditions)
  (export &cpp-error make-cpp-error cpp-error?
          &cpp-parse-error make-cpp-parse-error cpp-parse-error?
          cpp-parse-error-location
          &cpp-include-error make-cpp-include-error cpp-include-error?
          cpp-include-error-path
          &cpp-macro-error make-cpp-macro-error cpp-macro-error?
          cpp-macro-error-name
          &cpp-limit-exceeded make-cpp-limit-exceeded cpp-limit-exceeded?
          cpp-limit-exceeded-name
          cpp-limit-exceeded-value
          &cpp-effect-error make-cpp-effect-error cpp-effect-error?
          cpp-effect-error-effect-name
          raise-cpp-error)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs exceptions))

  ;; Base condition type for all C preprocessor/parser errors
  (define-condition-type &cpp-error &error
    make-cpp-error cpp-error?)

  ;; Parse errors - syntax errors in C code
  (define-condition-type &cpp-parse-error &cpp-error
    make-cpp-parse-error cpp-parse-error?
    (location cpp-parse-error-location))  ;; Source location (file:line:col)

  ;; Include errors - file not found, circular includes
  (define-condition-type &cpp-include-error &cpp-error
    make-cpp-include-error cpp-include-error?
    (path cpp-include-error-path))  ;; Include path that failed

  ;; Macro errors - invalid macro definition, expansion failures
  (define-condition-type &cpp-macro-error &cpp-error
    make-cpp-macro-error cpp-macro-error?
    (macro-name cpp-macro-error-name))  ;; Macro name that caused error

  ;; Limit exceeded errors - security limits hit (DoS prevention)
  (define-condition-type &cpp-limit-exceeded &cpp-error
    make-cpp-limit-exceeded cpp-limit-exceeded?
    (limit-name cpp-limit-exceeded-name)      ;; Name of limit exceeded
    (limit-value cpp-limit-exceeded-value))   ;; Actual value that exceeded limit

  ;; Effect handler errors - unknown or unregistered effect handlers
  (define-condition-type &cpp-effect-error &cpp-error
    make-cpp-effect-error cpp-effect-error?
    (effect-name cpp-effect-error-effect-name))  ;; Effect name that was not found

  ;; Helper to raise C preprocessor errors with message and irritants
  (define (raise-cpp-error type-constructor message . irritants)
    ;;   Raise a C preprocessor error with message and optional irritants
    (raise
      (condition
        (type-constructor)
        (make-message-condition message)
        (if (null? irritants)
            (make-irritants-condition '())
            (make-irritants-condition irritants))))))
