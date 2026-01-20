;; SPDX-License-Identifier: WTFPL
;; C Preprocessor Include Directive Handler
;; Handles #include with cycle detection

(library (c-tools effects cpp includes)
  (export with-cpp-include)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs lists)
          (c-tools core conditions)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools effects cpp core)
          (c-tools effects files)
          (c-tools effects registry))

  ;; Include handler with cycle detection
  (define (with-cpp-include search-paths thunk)
    (let ([include-stack '()])

      (with-handler 'cpp-include
        (lambda (data k loop)
          (let ([path (car data)]
                [angle-brackets? (cdr data)]
                [paths (if (cdr data)  ;; angle-brackets?
                          search-paths
                          (cons "." search-paths))])

            ;; Try each search path
            (let try-path ([ps paths])
              (if (null? ps)
                  (k #f)
                  (let ([full-path (if (equal? (car ps) ".")
                                      path
                                      (string-append (car ps) "/" path))])

                    ;; Check for cycle
                    (if (member full-path include-stack)
                        (raise
                          (condition
                            (make-cpp-parse-error (make-location full-path 1 1))
                            (make-message-condition "Include cycle detected")
                            (make-irritants-condition include-stack)))

                        ;; Check if file exists
                        (if (file-exists! full-path)
                            (begin
                              (set! include-stack (cons full-path include-stack))
                              (let ([content (read-file! full-path)])
                                (set! include-stack (cdr include-stack))
                                (k (cons full-path content))))
                            (try-path (cdr ps)))))))))

        (thunk))))

  ;;=========================================================================
  ;; Registration

  (register-effect! 'cpp-include
    (lambda (spec thunk)
      (with-cpp-include (cadr spec) thunk)))
)
