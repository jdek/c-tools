;; SPDX-License-Identifier: WTFPL
;; C Preprocessor Security Limits
;; DoS prevention: limits on file sizes, nesting depth, expansion, etc.
;; Follows pattern from lib/format/javascript.sls

(library (c-tools core limits)
  (export ;; Document-level limits
          max-header-size
          max-token-length
          max-tokens-per-file

          ;; Nesting limits (DoS prevention)
          max-include-depth
          max-macro-expansion-depth
          max-conditional-depth
          max-paren-depth

          ;; Expansion limits (bomb prevention)
          max-macro-expansion-size
          max-total-expansions

          ;; Limit box creation
          make-preprocessor-limits

          ;; Limit checking helpers
          check-limit!
          increment-limit!
          decrement-limit!)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs conditions)
          (rnrs exceptions)
          (only (chezscheme) box unbox set-box!)
          (c-tools core conditions))

  ;;; Document-level limits
  (define max-header-size (* 16 1024 1024))        ;; 16MB per header file
  (define max-token-length (* 1 1024 1024))        ;; 1MB per token (strings/identifiers)
  (define max-tokens-per-file 1000000)             ;; 1M tokens per file

  ;;; Nesting limits (prevent stack overflow)
  (define max-include-depth 100)                    ;; Include nesting depth
  (define max-macro-expansion-depth 200)            ;; Macro expansion recursion
  (define max-conditional-depth 100)                ;; #if/#ifdef nesting
  (define max-paren-depth 100)                      ;; Parenthesis/bracket nesting

  ;;; Expansion limits (prevent exponential blowup)
  (define max-macro-expansion-size (* 10 1024 1024)) ;; 10MB total expanded size
  (define max-total-expansions 10000)                 ;; Total macro invocations

  ;;; Create mutable limit counters (boxes pattern from javascript.sls)
  (define (make-preprocessor-limits)
    ;;   Create a fresh set of limit counters for a preprocessing session
    (list
      (cons 'bytes-read (box 0))
      (cons 'token-count (box 0))
      (cons 'include-depth (box 0))
      (cons 'expansion-depth (box 0))
      (cons 'conditional-depth (box 0))
      (cons 'paren-depth (box 0))
      (cons 'total-expansions (box 0))
      (cons 'expansion-size (box 0))))

  ;;; Limit checking helper
  (define (check-limit! limits limit-name max-value location)
    ;;   Check if a limit has been exceeded, raise error if so
    (let ([limit-box (cdr (assq limit-name limits))])
      (when (> (unbox limit-box) max-value)
        (raise
          (condition
            (make-cpp-limit-exceeded limit-name (unbox limit-box))
            (make-message-condition
              (string-append "Limit exceeded: "
                            (symbol->string limit-name)
                            " at "
                            (if location location "unknown location"))))))))

  ;;; Increment limit counter and check
  (define (increment-limit! limits limit-name max-value location)
    ;;   Increment a limit counter and check if exceeded
    (let ([limit-box (cdr (assq limit-name limits))])
      (set-box! limit-box (+ (unbox limit-box) 1))
      (check-limit! limits limit-name max-value location)))

  ;;; Decrement limit counter (for nesting depth unwinding)
  (define (decrement-limit! limits limit-name)
    ;;   Decrement a limit counter (for exiting nested contexts)
    (let ([limit-box (cdr (assq limit-name limits))])
      (set-box! limit-box (- (unbox limit-box) 1)))))
