#!/usr/bin/env -S scheme --script
;;=======================================================================
;; fuzz-preprocessor.ss - Generative fuzzing for C preprocessor
;; Tests macro expansion, conditional compilation, and directive handling
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing generators)
        (c-tools testing properties)
        (c-tools testing fuzzing harness))

;;-----------------------------------------------------------------------
;; Property-Based Test Cases
;;-----------------------------------------------------------------------

;; Bug detector: Preprocessor should handle all valid directives
(define (check-preprocess-succeeds input)
  (let ([result (parse-c-string input)])
    (if result
        'pass
        'fail)))

;; Bug detector: Preprocessing should be deterministic
(define (check-preprocess-deterministic input)
  (let ([result1 (parse-c-string input)]
        [result2 (parse-c-string input)])
    (if (or (and (not result1) (not result2))
            (and result1 result2))
        'pass
        'fail)))

;;-----------------------------------------------------------------------
;; Preprocessor Directive Generators
;;-----------------------------------------------------------------------

;; Generator: Simple #define directive
(define (make-define-object-macro-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-bind gen-int-literal
                                   (lambda (value)
                                     (gen-return
                                       (string-append "#define " name " " value
                                                     "\nint x = " name ";"))))))])
    (generate-from gen seed)))

;; Generator: #define function-like macro
(define (make-define-function-macro-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-bind gen-c-identifier
                                   (lambda (param)
                                     (gen-return
                                       (string-append "#define " name "(" param ") (" param " + 1)"
                                                     "\nint x = " name "(42);"))))))])
    (generate-from gen seed)))

;; Generator: #ifdef / #endif
(define (make-ifdef-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-bind gen-c-identifier
                                               (lambda (var)
                                                 (gen-return
                                                   (string-append "#define " name " 1\n"
                                                                 "#ifdef " name "\n"
                                                                 type " " var ";\n"
                                                                 "#endif"))))))))])
    (generate-from gen seed)))

;; Generator: #ifndef / #define / #endif (header guard pattern)
(define (make-header-guard-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (guard)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-bind gen-c-identifier
                                               (lambda (var)
                                                 (gen-return
                                                   (string-append "#ifndef " guard "\n"
                                                                 "#define " guard "\n"
                                                                 type " " var ";\n"
                                                                 "#endif"))))))))])
    (generate-from gen seed)))

;; Generator: #if / #else / #endif
(define (make-if-else-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-int-literal
                       (lambda (val)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-bind gen-c-identifier
                                               (lambda (var)
                                                 (gen-return
                                                   (string-append "#if " val "\n"
                                                                 type " " var "1;\n"
                                                                 "#else\n"
                                                                 type " " var "2;\n"
                                                                 "#endif"))))))))])
    (generate-from gen seed)))

;; Generator: Nested conditionals
(define (make-nested-conditional-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (outer)
                         (gen-bind gen-macro-name
                                   (lambda (inner)
                                     (gen-return
                                       (string-append "#define " outer " 1\n"
                                                     "#define " inner " 1\n"
                                                     "#ifdef " outer "\n"
                                                     "#ifdef " inner "\n"
                                                     "int x;\n"
                                                     "#endif\n"
                                                     "#endif"))))))])
    (generate-from gen seed)))

;; Generator: Multiple defines with usage
(define (make-multiple-defines-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name1)
                         (gen-bind gen-macro-name
                                   (lambda (name2)
                                     (gen-bind gen-int-literal
                                               (lambda (val1)
                                                 (gen-bind gen-int-literal
                                                           (lambda (val2)
                                                             (gen-return
                                                               (string-append "#define " name1 " " val1 "\n"
                                                                             "#define " name2 " " val2 "\n"
                                                                             "int x = " name1 " + " name2 ";"))))))))))])
    (generate-from gen seed)))

;; Helper: Generate from a generator with a seed
(define (generate-from gen seed)
  (generate gen (mod seed 100)))

;;-----------------------------------------------------------------------
;; Multi-Property Test Runner
;;-----------------------------------------------------------------------

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

(printf "C Preprocessor Generative Fuzzing\n")
(printf "==================================\n\n")

(let ([test-count (if (and (pair? (command-line-arguments))
                          (string->number (car (command-line-arguments))))
                     (string->number (car (command-line-arguments)))
                     500)]
      [all-properties (list check-preprocess-succeeds
                           check-preprocess-deterministic)])

  (printf "Test count: ~a per campaign\n" test-count)
  (printf "Total properties checked: ~a\n\n" (length all-properties))

  (let ([total-bugs 0])

    ;; Campaign 1: Object-like macros
    (let ([bugs (run-generative-campaign
                  "Object-like Macros"
                  make-define-object-macro-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 2: Function-like macros
    (let ([bugs (run-generative-campaign
                  "Function-like Macros"
                  make-define-function-macro-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 3: #ifdef conditionals
    (let ([bugs (run-generative-campaign
                  "#ifdef Conditionals"
                  make-ifdef-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 4: Header guards
    (let ([bugs (run-generative-campaign
                  "Header Guards (#ifndef)"
                  make-header-guard-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 5: #if/#else conditionals
    (let ([bugs (run-generative-campaign
                  "#if/#else Conditionals"
                  make-if-else-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 6: Nested conditionals
    (let ([bugs (run-generative-campaign
                  "Nested Conditionals"
                  make-nested-conditional-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    ;; Campaign 7: Multiple defines
    (let ([bugs (run-generative-campaign
                  "Multiple Macro Definitions"
                  make-multiple-defines-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (printf "\n========================================\n")
    (printf "Total bugs found: ~a\n" total-bugs)
    (printf "========================================\n\n")

    (if (> total-bugs 0)
        (exit 1)
        (begin
          (printf "All preprocessor fuzzing campaigns passed!\n")
          (exit 0)))))
