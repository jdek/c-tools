;; Fuzzing Test Harness - Campaign runner for C parser fuzzing

(library (c-tools testing fuzzing harness)
  (export
    ;; Main API
    run-fuzzing-campaign      ;; campaign => campaign-result
    make-campaign
    campaign?

    ;; Campaign configuration
    campaign-name
    campaign-parser
    campaign-input-generator
    campaign-duration
    campaign-test-count

    ;; Campaign result
    campaign-result?
    campaign-result-name
    campaign-result-total-tests
    campaign-result-passed
    campaign-result-crashes
    campaign-result-failures
    campaign-result-duration
    campaign-result-tests-per-second

    ;; Reporting
    print-campaign-summary)

  (import (rnrs base)
          (rnrs control)
          (rnrs exceptions)
          (rnrs io simple)
          (rnrs lists)
          (rnrs records syntactic)
          (only (chezscheme) current-time time-second time-nanosecond
                             time-difference format random-seed))

  ;;=======================================================================
  ;; Campaign configuration

  (define-record-type campaign
    (fields
      (immutable name)               ;; String: campaign name
      (immutable parser)             ;; Procedure: parser to test (string => result)
      (immutable input-generator)    ;; Procedure: generates test inputs (seed => string)
      (immutable duration)           ;; Max duration (seconds) or #f
      (immutable test-count)))       ;; Number of tests to run

  ;;=======================================================================
  ;; Campaign result

  (define-record-type campaign-result
    (fields
      (immutable name)               ;; Campaign name
      (immutable total-tests)        ;; Total tests executed
      (immutable passed)             ;; Tests that passed (didn't crash)
      (immutable crashes)            ;; List of crash-causing inputs
      (immutable failures)           ;; List of failed inputs
      (immutable duration)           ;; Actual duration (seconds)
      (immutable tests-per-second))) ;; Throughput

  ;;=======================================================================
  ;; Main Campaign Execution

  ;; run-fuzzing-campaign : campaign => campaign-result
  ;;   Runs a fuzzing campaign and returns results.
  (define (run-fuzzing-campaign campaign)
    (display-campaign-header campaign)

    (let* ([start-time (current-time)]
           [parser (campaign-parser campaign)]
           [input-gen (campaign-input-generator campaign)]
           [test-count (campaign-test-count campaign)])

      (display "Starting fuzzing campaign...\n\n")

      ;; Run tests
      (let-values ([(passed crashes failures)
                    (run-tests parser input-gen test-count)])

        (let* ([end-time (current-time)]
               [elapsed (time-difference end-time start-time)]
               [duration-secs (+ (time-second elapsed)
                                (/ (time-nanosecond elapsed) 1000000000.0))]
               [total test-count]
               [throughput (if (> duration-secs 0.0)
                              (/ total duration-secs)
                              0.0)])

          (make-campaign-result
            (campaign-name campaign)
            total
            passed
            crashes
            failures
            duration-secs
            throughput)))))

  ;; run-tests : procedure procedure fixnum => (values fixnum list list)
  ;;   Runs test-count tests, returns (passed-count crashes failures).
  (define (run-tests parser input-gen test-count)
    (let loop ([n 0]
               [passed 0]
               [crashes '()]
               [failures '()])
      (if (>= n test-count)
          (values passed (reverse crashes) (reverse failures))
          (let* ([seed (+ n 1)]  ;; Start seeds from 1, not 0
                 [input (input-gen seed)]
                 [result (test-input parser input)])
            (case result
              [(pass)
               (when (zero? (mod n 100))
                 (display (format "  Progress: ~a/~a tests completed\r"
                                 n test-count)))
               (loop (+ n 1) (+ passed 1) crashes failures)]
              [(crash)
               (display (format "\n  CRASH found at test ~a\n" n))
               (loop (+ n 1) passed (cons input crashes) failures)]
              [(fail)
               (display (format "\n  FAIL found at test ~a\n" n))
               (loop (+ n 1) passed crashes (cons input failures))])))))

  ;; test-input : procedure string => symbol
  ;;   Tests a single input, returns 'pass, 'crash, or 'fail.
  (define (test-input parser input)
    (guard (ex
            [else 'crash])  ;; Any exception is a crash
      (let ([result (parser input)])
        (if result 'pass 'fail))))

  ;;=======================================================================
  ;; Reporting

  ;; display-campaign-header : campaign => void
  ;;   Displays campaign header.
  (define (display-campaign-header campaign)
    (display "\n")
    (display "================================================================\n")
    (display (format "  C-Tools Fuzzing Campaign: ~a\n" (campaign-name campaign)))
    (display "================================================================\n\n")
    (display (format "Tests: ~a\n\n" (campaign-test-count campaign))))

  ;; print-campaign-summary : campaign-result => void
  ;;   Prints campaign summary.
  (define (print-campaign-summary result)
    (display "\n")
    (display "================================================================\n")
    (display (format "  Campaign Summary: ~a\n" (campaign-result-name result)))
    (display "================================================================\n\n")

    (let ([total (campaign-result-total-tests result)]
          [passed (campaign-result-passed result)]
          [crashes (campaign-result-crashes result)]
          [failures (campaign-result-failures result)]
          [duration (campaign-result-duration result)]
          [throughput (campaign-result-tests-per-second result)])

      (display (format "Total Tests:     ~a\n" total))
      (display (format "Passed:          ~a (~a%)\n" passed
                       (if (> total 0)
                           (inexact (floor (* 100 (/ passed total))))
                           0)))
      (display (format "Crashes:         ~a\n" (length crashes)))
      (display (format "Failures:        ~a\n" (length failures)))
      (display (format "Duration:        ~a seconds\n"
                       (round-to-2-decimals duration)))
      (display (format "Throughput:      ~a tests/sec\n\n"
                       (round-to-1-decimal throughput)))

      ;; Show status
      (cond
        [(> (length crashes) 0)
         (display "STATUS: CRASHES FOUND\n")
         (display "\nCrash-causing inputs:\n")
         (for-each
           (lambda (input)
             (display (format "  ~s\n" (truncate-string input 200))))
           (take-up-to 5 crashes))
         (display "\n")]

        [(> (length failures) 0)
         (display "STATUS: FAILURES DETECTED\n")
         (display "\nFailed inputs:\n")
         (for-each
           (lambda (input)
             (display (format "  ~s\n" (truncate-string input 200))))
           (take-up-to 5 failures))
         (display "\n")]

        [else
         (display "STATUS: ALL TESTS PASSED\n")])

      (display "\n")))

  ;;=======================================================================
  ;; Helpers

  ;; round-to-2-decimals : real => real
  ;;   Rounds to 2 decimal places.
  (define (round-to-2-decimals x)
    (/ (round (* x 100)) 100))

  ;; round-to-1-decimal : real => real
  ;;   Rounds to 1 decimal place.
  (define (round-to-1-decimal x)
    (/ (round (* x 10)) 10))

  ;; take-up-to : fixnum list => list
  ;;   Takes up to n elements from list.
  (define (take-up-to n lst)
    (if (or (null? lst) (<= n 0))
        '()
        (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

  ;; truncate-string : string fixnum => string
  ;;   Truncates string to max length.
  (define (truncate-string str max-len)
    (if (> (string-length str) max-len)
        (string-append (substring str 0 max-len) "...")
        str))

) ;; end library
