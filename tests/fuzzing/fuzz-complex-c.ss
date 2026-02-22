#!/usr/bin/env -S scheme --script
;;=======================================================================
;; fuzz-complex-c.ss - Generative fuzzing for complex C code
;; Tests parser on realistic, complex valid C code
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing generators)
        (c-tools testing properties)
        (c-tools testing fuzzing harness))

;;-----------------------------------------------------------------------
;; Property-Based Test Cases
;;-----------------------------------------------------------------------

(define (check-parse-succeeds input)
  (let ([result (parse-c-string input)])
    (if result 'pass 'fail)))

(define (check-determinism input)
  (if (prop-parser-deterministic input) 'pass 'fail))

(define (check-ffi-succeeds input)
  (guard (ex [else 'fail])
    (let ([ffi (generate-ffi-from-string input)])
      (if ffi 'pass 'fail))))

;;-----------------------------------------------------------------------
;; Complex Code Generators
;;-----------------------------------------------------------------------

;; Generator: Multiple declarations in one file
(define (make-multi-declaration-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [vars (list "x" "y" "z")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " " (list-ref vars idx) "1;\n"
                    (list-ref types (mod (+ idx 1) (length types))) " "
                    (list-ref vars (mod (+ idx 1) (length vars))) "2;\n"
                    (list-ref types (mod (+ idx 2) (length types))) " "
                    (list-ref vars (mod (+ idx 2) (length vars))) "3;"))))

;; Generator: Nested structs
(define (make-nested-struct-generator seed)
  (random-seed seed)
  (let ([structs (list "Inner" "Nested" "Sub")]
        [types (list "int" "char" "float")])
    (let ([idx (mod seed (length structs))])
      (string-append "struct " (list-ref structs idx) " { "
                    (list-ref types idx) " field; };\n"
                    "struct Outer { struct " (list-ref structs idx) " nested; };"))))

;; Generator: Pointer to pointer
(define (make-pointer-to-pointer-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float" "void")]
        [vars (list "ptr" "p" "pointer")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " **"
                    (list-ref vars (mod idx (length vars))) ";"))))

;; Generator: Array of pointers
(define (make-array-of-pointers-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [vars (list "arr" "ptrs" "array")]
        [sizes (list "5" "10" "20")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " *"
                    (list-ref vars (mod idx (length vars))) "["
                    (list-ref sizes (mod idx (length sizes))) "];"))))

;; Generator: Pointer to array
(define (make-pointer-to-array-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [vars (list "ptr" "p" "arr")]
        [sizes (list "10" "20" "5")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " (*"
                    (list-ref vars (mod idx (length vars))) ")["
                    (list-ref sizes (mod idx (length sizes))) "];"))))

;; Generator: Function pointer
(define (make-function-pointer-generator seed)
  (random-seed seed)
  (let ([rets (list "int" "void" "char")]
        [names (list "fn" "func" "callback")]
        [params (list "int" "void" "char")])
    (let ([idx (mod seed (length rets))])
      (string-append (list-ref rets idx) " (*"
                    (list-ref names (mod idx (length names))) ")("
                    (list-ref params (mod idx (length params))) ");"))))

;; Generator: Struct with array field
(define (make-struct-with-array-generator seed)
  (random-seed seed)
  (let ([names (list "Data" "Buffer" "Array")]
        [types (list "int" "char" "float")]
        [fields (list "data" "buffer" "items")]
        [sizes (list "10" "20" "5")])
    (let ([idx (mod seed (length names))])
      (string-append "struct " (list-ref names idx) " { "
                    (list-ref types (mod idx (length types))) " "
                    (list-ref fields (mod idx (length fields))) "["
                    (list-ref sizes (mod idx (length sizes))) "]; };"))))

;; Generator: Typedef of function pointer
(define (make-typedef-function-pointer-generator seed)
  (random-seed seed)
  (let ([names (list "FnPtr" "Callback" "Handler")]
        [rets (list "int" "void" "char")]
        [params (list "int" "void" "char")])
    (let ([idx (mod seed (length names))])
      (string-append "typedef " (list-ref rets (mod idx (length rets))) " (*"
                    (list-ref names idx) ")("
                    (list-ref params (mod idx (length params))) ");"))))

;; Generator: Large file with many declarations
(define (make-large-file-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-basic-type
                       (lambda (t1)
                         (gen-bind gen-c-identifier
                                   (lambda (v1)
                                     (gen-bind gen-basic-type
                                               (lambda (t2)
                                                 (gen-bind gen-c-identifier
                                                           (lambda (v2)
                                                             (gen-bind gen-struct-name
                                                                       (lambda (sname)
                                                                         (gen-bind gen-c-identifier
                                                                                   (lambda (field1)
                                                                                     (gen-bind gen-c-identifier
                                                                                               (lambda (field2)
                                                                                                 (gen-bind gen-enum-name
                                                                                                           (lambda (ename)
                                                                                                             (gen-return
                                                                                                               (string-append t1 " " v1 ";\n"
                                                                                                                             t2 " " v2 ";\n"
                                                                                                                             "struct " sname " { " t1 " " field1 "; " t2 " " field2 "; };\n"
                                                                                                                             "enum " ename " { A, B, C };"))))))))))))))))))])
    (generate-from gen seed)))

;; Generator: Struct with multiple fields of different types
(define (make-complex-struct-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-struct-name
                       (lambda (name)
                         (gen-bind gen-basic-type
                                   (lambda (type1)
                                     (gen-bind gen-basic-type
                                               (lambda (type2)
                                                 (gen-bind gen-basic-type
                                                           (lambda (type3)
                                                             (gen-bind gen-int-literal
                                                                       (lambda (size)
                                                                         (gen-return
                                                                           (string-append "struct " name " { "
                                                                                         type1 " field1; "
                                                                                         type2 " *field2; "
                                                                                         type3 " field3[" size "]; };"))))))))))))])
    (generate-from gen seed)))

;; Helper
(define (generate-from gen seed)
  (generate gen (mod seed 100)))

;;-----------------------------------------------------------------------
;; Test Runner
;;-----------------------------------------------------------------------

(define (test-with-properties input properties)
  (let loop ([props properties] [failures '()])
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

  (let loop ([n 0] [passed 0] [bugs '()])
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
            [(pass) (loop (+ n 1) (+ passed 1) bugs)]
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

(printf "Complex C Code Generative Fuzzing\n")
(printf "==================================\n\n")

(let ([test-count (if (and (pair? (command-line-arguments))
                          (string->number (car (command-line-arguments))))
                     (string->number (car (command-line-arguments)))
                     500)]
      [all-properties (list check-parse-succeeds
                           check-determinism
                           check-ffi-succeeds)])

  (printf "Test count: ~a per campaign\n" test-count)
  (printf "Total properties checked: ~a\n\n" (length all-properties))

  (let ([total-bugs 0])

    (let ([bugs (run-generative-campaign
                  "Multiple Declarations"
                  make-multi-declaration-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Nested Structs"
                  make-nested-struct-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Pointer to Pointer"
                  make-pointer-to-pointer-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Array of Pointers"
                  make-array-of-pointers-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Pointer to Array"
                  make-pointer-to-array-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Function Pointers"
                  make-function-pointer-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Struct with Array Field"
                  make-struct-with-array-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Typedef Function Pointer"
                  make-typedef-function-pointer-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Complex Struct (Multiple Field Types)"
                  make-complex-struct-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (let ([bugs (run-generative-campaign
                  "Large Files (Many Declarations)"
                  make-large-file-generator
                  all-properties
                  test-count)])
      (set! total-bugs (+ total-bugs (length bugs))))

    (printf "\n========================================\n")
    (printf "Total bugs found: ~a\n" total-bugs)
    (printf "========================================\n\n")

    (if (> total-bugs 0)
        (exit 1)
        (begin
          (printf "All complex code fuzzing campaigns passed!\n")
          (exit 0)))))
