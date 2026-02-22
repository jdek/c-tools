#!/usr/bin/env -S scheme --script
;;=======================================================================
;; run-all-tests.ss - Run all test suites
;;=======================================================================

(import (chezscheme))

(define (run-test-file file description)
  (printf "\n========================================\n")
  (printf "Running: ~a\n" description)
  (printf "========================================\n")
  (flush-output-port (current-output-port))
  (let ([result (system (string-append file " 2>/dev/null"))])
    (if (= result 0)
        (begin
          (printf "✓ ~a PASSED\n" description)
          #t)
        (begin
          (printf "✗ ~a FAILED (exit code ~a)\n" description result)
          #f))))

(printf "C-Tools Test Suite\n")
(printf "==================\n\n")

(let ([tests (list
               (list "./tests/properties/test-parser-properties.ss" "Parser Properties")
               (list "./tests/properties/test-type-mapping.ss" "Type Mapping")
               (list "./tests/properties/test-parser-random.ss" "Random Parser Tests")
               (list "./tests/fuzzing/fuzz-all.ss 100" "Comprehensive Fuzzing (59 campaigns)"))]
      [passed 0]
      [failed 0])

  (for-each
    (lambda (test)
      (if (run-test-file (car test) (cadr test))
          (set! passed (+ passed 1))
          (set! failed (+ failed 1))))
    tests)

  (printf "\n========================================\n")
  (printf "Test Summary\n")
  (printf "========================================\n")
  (printf "Total:  ~a\n" (+ passed failed))
  (printf "Passed: ~a\n" passed)
  (printf "Failed: ~a\n" failed)
  (printf "\n")

  (if (> failed 0)
      (exit 1)
      (exit 0)))
