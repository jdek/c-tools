#!/usr/bin/env -S scheme --script
;;=======================================================================
;; test-parser-properties.ss - Property tests for C parser
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing generators)
        (c-tools utility))

;;-----------------------------------------------------------------------
;; Properties
;;-----------------------------------------------------------------------

;; Property: Generated identifiers should not start with digits
(define prop-identifier-no-leading-digit
  (lambda (id)
    (let ([first-char (string-ref id 0)])
      (not (char-numeric? first-char)))))

;; Property: Generated basic types should be non-empty
(define prop-basic-type-non-empty
  (lambda (type)
    (> (string-length type) 0)))

;; Property: Pointer types should end with '*'
(define prop-pointer-type-has-star
  (lambda (ptr-type)
    (string-suffix? ptr-type "*")))

;; Property: Array types should contain brackets
(define prop-array-type-has-brackets
  (lambda (arr-type)
    (and (string-contains? arr-type "[")
         (string-contains? arr-type "]"))))

;; Property: String literals should be quoted
(define prop-string-literal-quoted
  (lambda (str-lit)
    (and (string-prefix? str-lit "\"")
         (string-suffix? str-lit "\""))))

;; Property: Hex literals should start with 0x
(define prop-hex-literal-prefix
  (lambda (hex-lit)
    (string-prefix? hex-lit "0x")))

;; Property: Macro names should be uppercase
(define prop-macro-name-uppercase
  (lambda (macro-name)
    (string=? macro-name
              (list->string (map char-upcase (string->list macro-name))))))

;; Property: Function signatures should contain parentheses
(define prop-function-signature-has-parens
  (lambda (sig)
    (and (string-contains? sig "(")
         (string-contains? sig ")"))))

;; Property: Simple declarations should have at least one space (type + name)
(define prop-simple-declaration-has-space
  (lambda (decl)
    (string-contains? decl " ")))

;;-----------------------------------------------------------------------
;; Run Tests
;;-----------------------------------------------------------------------

(printf "Running C parser property tests...\n\n")

(let ([cfg (make-config 50)])
  (display "Checking: C identifiers do not start with digits\n")
  (let ([result (check-property prop-identifier-no-leading-digit
                               gen-c-identifier
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Basic types are non-empty\n")
  (let ([result (check-property prop-basic-type-non-empty
                               gen-basic-type
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline))))

  (display "Checking: Pointer types end with *\n")
  (let ([result (check-property prop-pointer-type-has-star
                               (gen-pointer-type gen-basic-type)
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Array types contain brackets\n")
  (let ([result (check-property prop-array-type-has-brackets
                               (gen-array-type gen-basic-type)
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: String literals are quoted\n")
  (let ([result (check-property prop-string-literal-quoted
                               gen-string-literal
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Hex literals start with 0x\n")
  (let ([result (check-property prop-hex-literal-prefix
                               gen-hex-literal
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Macro names are uppercase\n")
  (let ([result (check-property prop-macro-name-uppercase
                               gen-macro-name
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Function signatures contain parentheses\n")
  (let ([result (check-property prop-function-signature-has-parens
                               gen-function-signature
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Simple declarations have spaces\n")
  (let ([result (check-property prop-simple-declaration-has-space
                               gen-simple-declaration
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline)))))

(printf "\nAll parser property tests completed!\n")
