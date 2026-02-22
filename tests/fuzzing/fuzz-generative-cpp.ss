#!/usr/bin/env -S scheme --script
;;=======================================================================
;; fuzz-generative-cpp.ss - Generative fuzzing for C++ parser
;; Uses generators to create VALID C++ code and finds logic bugs
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing generators)
        (c-tools testing properties)
        (c-tools testing fuzzing harness))

;;-----------------------------------------------------------------------
;; Property-Based Test Cases
;;-----------------------------------------------------------------------

;; Bug detector: Parser should accept all generated valid code
(define (check-parse-succeeds input)
  (let ([result (parse-cpp-string input)])
    (if result
        'pass
        'fail)))  ;; Generated code failed to parse = BUG

;; Bug detector: Parser should be deterministic
(define (check-determinism input)
  (let ([result1 (parse-cpp-string input)]
        [result2 (parse-cpp-string input)])
    (if (or (and (not result1) (not result2))
            (and result1 result2))  ;; Both succeed or both fail
        'pass
        'fail)))  ;; Non-deterministic = BUG

;;-----------------------------------------------------------------------
;; C++ Code Generators
;;-----------------------------------------------------------------------

;; Generator: Simple C++ declarations (same as C)
(define (make-simple-declaration-generator seed)
  (random-seed seed)
  (let ([gen (gen-map (lambda (decl) (string-append decl ";"))
                     gen-simple-declaration)])
    (generate-from gen seed)))

;; Generator: Class declarations
(define (make-class-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return (string-append "class " name " {};"))))])
    (generate-from gen seed)))

;; Generator: Class with constructor
(define (make-class-with-constructor-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return
                           (string-append "class " name " { public: "
                                         name "(); };"))))])
    (generate-from gen seed)))

;; Generator: Class with destructor
(define (make-class-with-destructor-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return
                           (string-append "class " name " { public: ~"
                                         name "(); };"))))])
    (generate-from gen seed)))

;; Generator: Class with member function
(define (make-class-with-member-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (cname)
                         (gen-bind gen-basic-type
                                   (lambda (ret)
                                     (gen-bind gen-c-identifier
                                               (lambda (fname)
                                                 (gen-return
                                                   (string-append "class " cname
                                                                 " { public: " ret " "
                                                                 fname "(); };"))))))))])
    (generate-from gen seed)))

;; Generator: Namespace declarations
(define (make-namespace-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return (string-append "namespace " name " {}"))))])
    (generate-from gen seed)))

;; Generator: Namespace with declaration
(define (make-namespace-with-decl-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (nname)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-bind gen-c-identifier
                                               (lambda (vname)
                                                 (gen-return
                                                   (string-append "namespace " nname
                                                                 " { " type " " vname
                                                                 "; }"))))))))])
    (generate-from gen seed)))

;; Generator: Struct declarations (C++ style)
(define (make-cpp-struct-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-struct-name
                       (lambda (name)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-return
                                       (string-append "struct " name " { "
                                                     type " field; };")))))) ])
    (generate-from gen seed)))

;; Helper: Generate from a generator with a seed
(define (generate-from gen seed)
  (generate gen (mod seed 100)))

;;-----------------------------------------------------------------------
;; Multi-Property Test Runner
;;-----------------------------------------------------------------------

;; test-with-properties : string list-of-procedures => (list symbol string list)
;;   Tests input against multiple properties, returns (status input failures)
(define (test-with-properties input properties)
  (let loop ([props properties]
             [failures '()])
    (if (null? props)
        (if (null? failures)
            (list 'pass input '())
            (list 'fail input failures))
        (let* ([prop (car props)]
               [result (prop input)])
          (if (eq? result 'pass)
              (loop (cdr props) failures)
              (loop (cdr props) (cons (car props) failures)))))))

;; run-generative-campaign : string procedure list-of-procedures fixnum => campaign-result
;;   Runs generative fuzzing with property checking
(define (run-generative-campaign name input-gen properties test-count)
  (printf "\n================================================================\n")
  (printf "  Generative Fuzzing: ~a\n" name)
  (printf "================================================================\n\n")
  (printf "Tests: ~a\n" test-count)
  (printf "Properties: ~a\n\n" (length properties))

  (let loop ([n 0]
             [passed 0]
             [bugs '()])
    (if (>= n test-count)
        ;; Done
        (begin
          (printf "\n================================================================\n")
          (printf "  Results: ~a\n" name)
          (printf "================================================================\n\n")
          (printf "Total Tests:     ~a\n" test-count)
          (printf "Passed:          ~a (~a%)\n" passed
                  (if (> test-count 0)
                      (inexact (floor (* 100 (/ passed test-count))))
                      0))
          (printf "Bugs Found:      ~a\n" (length bugs))

          (when (> (length bugs) 0)
            (printf "\nBUGS DETECTED:\n")
            (for-each
              (lambda (bug)
                (printf "\nInput: ~s\n" (car bug))
                (printf "Failed properties: ~a\n" (length (cadr bug))))
              (take-up-to 5 bugs)))

          (printf "\n")
          bugs)
        ;; Run test
        (let* ([seed (+ n 1)]
               [input (input-gen seed)]
               [result (test-with-properties input properties)])
          (when (zero? (mod n 100))
            (printf "  Progress: ~a/~a tests completed\r" n test-count))

          (case (car result)
            [(pass)
             (loop (+ n 1) (+ passed 1) bugs)]
            [(fail)
             (printf "\n  BUG found at test ~a: ~s\n" n input)
             (loop (+ n 1) passed (cons (list input (caddr result)) bugs))])))))

(define (take-up-to n lst)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

;;-----------------------------------------------------------------------
;; Run Campaigns
;;-----------------------------------------------------------------------

(printf "C++ Parser Generative Fuzzing\n")
(printf "==============================\n\n")

(let ([test-count (if (and (pair? (command-line-arguments))
                          (string->number (car (command-line-arguments))))
                     (string->number (car (command-line-arguments)))
                     500)]  ;; Default: 500 tests
      [all-properties (list check-parse-succeeds
                           check-determinism)])

  (printf "Test count: ~a per campaign\n" test-count)
  (printf "Total properties checked: ~a\n\n" (length all-properties))

  (let ([total-bugs 0])

    ;; Campaign 1: Simple declarations
    (let ([bugs (run-generative-campaign
                  "Simple Declarations"
                  make-simple-declaration-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 2: Class declarations
    (let ([bugs (run-generative-campaign
                  "Class Declarations"
                  make-class-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 3: Classes with constructors
    (let ([bugs (run-generative-campaign
                  "Classes with Constructors"
                  make-class-with-constructor-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 4: Classes with destructors
    (let ([bugs (run-generative-campaign
                  "Classes with Destructors"
                  make-class-with-destructor-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 5: Classes with member functions
    (let ([bugs (run-generative-campaign
                  "Classes with Member Functions"
                  make-class-with-member-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 6: Namespace declarations
    (let ([bugs (run-generative-campaign
                  "Namespace Declarations"
                  make-namespace-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 7: Namespaces with declarations
    (let ([bugs (run-generative-campaign
                  "Namespaces with Declarations"
                  make-namespace-with-decl-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 8: C++ structs
    (let ([bugs (run-generative-campaign
                  "C++ Struct Declarations"
                  make-cpp-struct-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (printf "\n========================================\n")
    (printf "Total bugs found: ~a\n" total-bugs)
    (printf "========================================\n\n")

    (if (> total-bugs 0)
        (exit 1)
        (begin
          (printf "All generative fuzzing campaigns passed!\n")
          (exit 0)))))
