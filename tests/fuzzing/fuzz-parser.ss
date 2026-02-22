#!/usr/bin/env -S scheme --script
;;=======================================================================
;; fuzz-parser.ss - Fuzzing campaign for C parser
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing fuzzing mutation)
        (c-tools testing fuzzing harness)
        (c-tools testing fuzzing corpus)
        (c-tools testing properties))

;;-----------------------------------------------------------------------
;; Fuzzing Strategies
;;-----------------------------------------------------------------------

;; Strategy 1: Mutate edge cases from corpus
(define (make-corpus-mutation-generator corpus mutations)
  (lambda (seed)
    (random-seed seed)
    (let* ([base-input (list-ref corpus (random (length corpus)))]
           [mutation (list-ref mutations (random (length mutations)))])
      (mutate-input base-input seed mutation))))

;; Strategy 2: Mutate valid C code
(define (make-valid-c-mutation-generator valid-corpus c-mutations)
  (lambda (seed)
    (random-seed seed)
    (let* ([base-input (list-ref valid-corpus (random (length valid-corpus)))]
           [mutation (list-ref c-mutations (random (length c-mutations)))])
      (mutate-input base-input seed mutation))))

;; Strategy 3: Pure random mutations on random base
(define (make-random-mutation-generator all-mutations)
  (lambda (seed)
    (random-seed seed)
    (let* ([base-inputs '("int x;" "struct S {};" "void f(void);"
                         "enum E { A };" "typedef int T;")]
           [base (list-ref base-inputs (random (length base-inputs)))]
           [n-mutations (+ 1 (random 5))])
      (apply-n-random-mutations base n-mutations all-mutations seed))))

(define (apply-n-random-mutations str n mutations seed)
  (if (<= n 0)
      str
      (let ([mutation (list-ref mutations (random (length mutations)))])
        (apply-n-random-mutations
          (mutate-input str seed mutation)
          (- n 1)
          mutations
          seed))))

;;-----------------------------------------------------------------------
;; Parser Wrapper
;;-----------------------------------------------------------------------

;; parse-c-wrapper : string => boolean
;;   Wraps the C parser for fuzzing (returns #t if no crash).
(define (parse-c-wrapper input)
  (guard (ex [else #f])
    (let ([result (parse-c-string input)])
      #t)))  ;; If we get here, no crash (result can be #f for parse failure)

;;-----------------------------------------------------------------------
;; Campaign Configurations
;;-----------------------------------------------------------------------

(define (get-test-count args)
  (if (and (pair? args) (string->number (car args)))
      (string->number (car args))
      1000))  ;; Default: 1000 tests

;;-----------------------------------------------------------------------
;; Helper
;;-----------------------------------------------------------------------

(define (take-mutations lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-mutations (cdr lst) (- n 1)))))

;;-----------------------------------------------------------------------
;; Run Campaigns
;;-----------------------------------------------------------------------

(printf "C Parser Fuzzing Test\n")
(printf "====================\n\n")

(let ([test-count (get-test-count (command-line-arguments))]
      [edge-cases (get-edge-case-corpus)]
      [basic-c (get-basic-c-corpus)])

  (printf "Test count: ~a\n" test-count)
  (printf "Edge case corpus: ~a cases\n" (length edge-cases))
  (printf "Basic C corpus: ~a cases\n\n" (length basic-c))

  ;; Campaign 1: Edge case mutations
  (printf "Campaign 1: Edge Case Mutations\n")
  (printf "--------------------------------\n")
  (let* ([all-muts (take-mutations all-mutations 20)]  ;; Use first 20 mutations
         [input-gen (make-corpus-mutation-generator edge-cases all-muts)]
         [campaign (make-campaign "Edge Case Mutations"
                                 parse-c-wrapper
                                 input-gen
                                 #f
                                 test-count)]
         [result (run-fuzzing-campaign campaign)])
    (print-campaign-summary result)
    (when (> (length (campaign-result-crashes result)) 0)
      (printf "FAIL: Crashes found in edge case fuzzing\n")
      (exit 1)))

  ;; Campaign 2: C-specific mutations
  (printf "\nCampaign 2: C-Specific Mutations\n")
  (printf "--------------------------------\n")
  (let* ([input-gen (make-valid-c-mutation-generator basic-c c-specific-mutations)]
         [campaign (make-campaign "C-Specific Mutations"
                                 parse-c-wrapper
                                 input-gen
                                 #f
                                 test-count)]
         [result (run-fuzzing-campaign campaign)])
    (print-campaign-summary result)
    (when (> (length (campaign-result-crashes result)) 0)
      (printf "FAIL: Crashes found in C-specific fuzzing\n")
      (exit 1)))

  ;; Campaign 3: Random heavy mutations
  (printf "\nCampaign 3: Random Heavy Mutations\n")
  (printf "-----------------------------------\n")
  (let* ([input-gen (make-random-mutation-generator all-mutations)]
         [campaign (make-campaign "Random Heavy Mutations"
                                 parse-c-wrapper
                                 input-gen
                                 #f
                                 test-count)]
         [result (run-fuzzing-campaign campaign)])
    (print-campaign-summary result)
    (when (> (length (campaign-result-crashes result)) 0)
      (printf "FAIL: Crashes found in random fuzzing\n")
      (exit 1))))

(printf "\nAll fuzzing campaigns completed successfully!\n")
