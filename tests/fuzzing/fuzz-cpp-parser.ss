#!/usr/bin/env -S scheme --script
;;=======================================================================
;; fuzz-cpp-parser.ss - Fuzzing campaign for C++ parser
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing fuzzing mutation)
        (c-tools testing fuzzing harness)
        (c-tools testing fuzzing corpus)
        (c-tools testing properties))

;;-----------------------------------------------------------------------
;; C++-Specific Mutations
;;-----------------------------------------------------------------------

;; insert-cpp-scope : string => string
;;   Inserts C++ scope resolution operator.
(define (insert-cpp-scope str)
  (if (= (string-length str) 0)
      str
      (let* ([pos (random (+ 1 (string-length str)))]
             [scope "::"])
        (string-append (substring str 0 pos)
                      scope
                      (substring str pos (string-length str))))))

;; insert-cpp-template : string => string
;;   Inserts template angle brackets.
(define (insert-cpp-template str)
  (if (= (string-length str) 0)
      str
      (let* ([pos (random (+ 1 (string-length str)))]
             [templates (list "<>" "<T>" "<typename T>" "<int N>"
                             "<class T, class U>" "<<" ">>")])
        (string-append (substring str 0 pos)
                      (list-ref templates (random (length templates)))
                      (substring str pos (string-length str))))))

;; insert-cpp-keyword : string => string
;;   Inserts C++ specific keywords.
(define (insert-cpp-keyword str)
  (if (= (string-length str) 0)
      str
      (let* ([pos (random (+ 1 (string-length str)))]
             [keywords (list "class" "namespace" "template" "typename"
                            "virtual" "override" "final" "public" "private"
                            "protected" "operator" "friend" "using" "auto"
                            "constexpr" "noexcept" "nullptr" "delete" "default")])
        (string-append (substring str 0 pos)
                      " "
                      (list-ref keywords (random (length keywords)))
                      " "
                      (substring str pos (string-length str))))))

(define cpp-specific-mutations
  (list
    insert-c-keywords
    insert-preprocessor-directive
    insert-unmatched-brace
    insert-pointer-stars
    insert-array-brackets
    insert-cpp-scope
    insert-cpp-template
    insert-cpp-keyword))

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

;; Strategy 2: Mutate valid C++ code
(define (make-valid-cpp-mutation-generator valid-corpus cpp-mutations)
  (lambda (seed)
    (random-seed seed)
    (let* ([base-input (list-ref valid-corpus (random (length valid-corpus)))]
           [mutation (list-ref cpp-mutations (random (length cpp-mutations)))])
      (mutate-input base-input seed mutation))))

;; Strategy 3: Pure random mutations on random base
(define (make-random-mutation-generator all-mutations)
  (lambda (seed)
    (random-seed seed)
    (let* ([base-inputs '("class C {};" "namespace N {}" "template<typename T> class X {};"
                         "enum class E { A };" "using T = int;")]
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

;; parse-cpp-wrapper : string => boolean
;;   Wraps the C++ parser for fuzzing (returns #t if no crash).
(define (parse-cpp-wrapper input)
  (guard (ex [else #f])
    (let ([result (parse-cpp-string input)])
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

(printf "C++ Parser Fuzzing Test\n")
(printf "=======================\n\n")

(let ([test-count (get-test-count (command-line-arguments))]
      [edge-cases (get-cpp-edge-case-corpus)]
      [basic-cpp (get-basic-cpp-corpus)])

  (printf "Test count: ~a\n" test-count)
  (printf "C++ edge case corpus: ~a cases\n" (length edge-cases))
  (printf "Basic C++ corpus: ~a cases\n\n" (length basic-cpp))

  ;; Campaign 1: Edge case mutations
  (printf "Campaign 1: C++ Edge Case Mutations\n")
  (printf "------------------------------------\n")
  (let* ([all-muts (take-mutations all-mutations 20)]  ;; Use first 20 mutations
         [input-gen (make-corpus-mutation-generator edge-cases all-muts)]
         [campaign (make-campaign "C++ Edge Case Mutations"
                                 parse-cpp-wrapper
                                 input-gen
                                 #f
                                 test-count)]
         [result (run-fuzzing-campaign campaign)])
    (print-campaign-summary result)
    (when (> (length (campaign-result-crashes result)) 0)
      (printf "FAIL: Crashes found in C++ edge case fuzzing\n")
      (exit 1)))

  ;; Campaign 2: C++-specific mutations
  (printf "\nCampaign 2: C++-Specific Mutations\n")
  (printf "-----------------------------------\n")
  (let* ([input-gen (make-valid-cpp-mutation-generator basic-cpp cpp-specific-mutations)]
         [campaign (make-campaign "C++-Specific Mutations"
                                 parse-cpp-wrapper
                                 input-gen
                                 #f
                                 test-count)]
         [result (run-fuzzing-campaign campaign)])
    (print-campaign-summary result)
    (when (> (length (campaign-result-crashes result)) 0)
      (printf "FAIL: Crashes found in C++-specific fuzzing\n")
      (exit 1)))

  ;; Campaign 3: Random heavy mutations
  (printf "\nCampaign 3: Random Heavy Mutations\n")
  (printf "-----------------------------------\n")
  (let* ([input-gen (make-random-mutation-generator all-mutations)]
         [campaign (make-campaign "Random Heavy Mutations"
                                 parse-cpp-wrapper
                                 input-gen
                                 #f
                                 test-count)]
         [result (run-fuzzing-campaign campaign)])
    (print-campaign-summary result)
    (when (> (length (campaign-result-crashes result)) 0)
      (printf "FAIL: Crashes found in random fuzzing\n")
      (exit 1))))

(printf "\nAll C++ fuzzing campaigns completed successfully!\n")
