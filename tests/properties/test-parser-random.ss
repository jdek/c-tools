#!/usr/bin/env -S scheme --script
;;=======================================================================
;; test-parser-random.ss - Randomized property tests for parser/FFI
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing generators)
        (c-tools testing properties)
        (c-tools utility))

;;-----------------------------------------------------------------------
;; Random Code Generators
;;-----------------------------------------------------------------------

;; gen-simple-c-declaration : => generator
;;   Generates random simple C declarations that should parse successfully.
(define gen-simple-c-declaration
  (gen-one-of
    ;; Variable declarations
    (gen-map (lambda (decl) (string-append decl ";"))
             gen-simple-declaration)
    ;; Pointer declarations
    (gen-map (lambda (type)
               (gen-bind gen-c-identifier
                         (lambda (name)
                           (gen-return (string-append type " " name ";")))))
             (gen-pointer-type gen-basic-type))
    ;; Array declarations
    (gen-bind gen-basic-type
              (lambda (base)
                (gen-bind gen-c-identifier
                          (lambda (name)
                            (gen-bind (gen-choose 1 50)
                                      (lambda (size)
                                        (gen-return
                                          (string-append base " " name "["
                                                        (number->string size) "];"))))))))))

;; gen-typedef-declaration : => generator
;;   Generates random typedef declarations.
(define gen-typedef-declaration
  (gen-bind gen-basic-type
            (lambda (base)
              (gen-bind gen-c-identifier
                        (lambda (name)
                          (gen-return (string-append "typedef " base " " name ";")))))))

;; gen-enum-declaration : => generator
;;   Generates random enum declarations.
(define gen-enum-declaration
  (gen-bind gen-enum-name
            (lambda (name)
              (gen-bind (gen-list (gen-choose 1 10))
                        (lambda (values)
                          (gen-return
                            (string-append "enum " name " { "
                                          (string-join
                                            (map (lambda (v)
                                                   (string-append "VAL_" (number->string v)))
                                                 values)
                                            ", ")
                                          " };")))))))

;;-----------------------------------------------------------------------
;; Properties Using Generated Code
;;-----------------------------------------------------------------------

;; Property: Parser is deterministic on generated valid code
(define prop-random-deterministic
  (lambda (code)
    (prop-parser-deterministic code)))

;; Property: FFI generation doesn't crash on generated code
(define prop-random-ffi-no-crash
  (lambda (code)
    (prop-ffi-no-crash code)))

;;-----------------------------------------------------------------------
;; Run Tests
;;-----------------------------------------------------------------------

(printf "Running randomized parser property tests...\\n\\n")

(let ([cfg (make-config 100)])  ;; 100 random tests

  (display "Checking: Parser deterministic on random simple declarations\\n")
  (let ([result (check-property prop-random-deterministic
                               gen-simple-c-declaration
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: FFI generation doesn't crash on random declarations\\n")
  (let ([result (check-property prop-random-ffi-no-crash
                               gen-simple-c-declaration
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Parser deterministic on random typedefs\\n")
  (let ([result (check-property prop-random-deterministic
                               gen-typedef-declaration
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: FFI generation doesn't crash on random typedefs\\n")
  (let ([result (check-property prop-random-ffi-no-crash
                               gen-typedef-declaration
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: Parser deterministic on random enums\\n")
  (let ([result (check-property prop-random-deterministic
                               gen-enum-declaration
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline))))

  (display "Checking: FFI generation doesn't crash on random enums\\n")
  (let ([result (check-property prop-random-ffi-no-crash
                               gen-enum-declaration
                               cfg)])
    (if (test-result-pass? result)
        (display "  PASS\\n")
        (begin
          (display "  FAIL: ")
          (display (test-result-reason result))
          (newline)
          (display "  Counter-example: ")
          (display (test-result-counter-example result))
          (newline)))))

(printf "\\nAll randomized parser tests completed!\\n")
