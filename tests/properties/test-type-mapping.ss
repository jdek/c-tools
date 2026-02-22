#!/usr/bin/env -S scheme --script
;;=======================================================================
;; test-type-mapping.ss - Property tests for C-to-FFI type mappings
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing properties))

;;-----------------------------------------------------------------------
;; Specific Test Cases
;;-----------------------------------------------------------------------

;; Test bool maps to boolean
(define test-bool-type
  "bool flag;")

(define test-bool-in-struct
  "struct Foo { bool enabled; };")

(define test-bool-in-function
  "bool is_valid(int x);")

;; Test arrays in structs are inline
(define test-array-in-struct
  "struct Bar { int values[10]; };")

(define test-array-with-macro
  "#define SIZE 24\nstruct Baz { int data[SIZE]; };")

;; Test pointer types
(define test-pointer-simple
  "int* ptr;")

(define test-pointer-in-struct
  "struct Qux { char* name; };")

;; Test static functions excluded
(define test-static-function
  "static inline int helper(void) { return 42; }")

;;-----------------------------------------------------------------------
;; Run Tests
;;-----------------------------------------------------------------------

(printf "Running C type mapping property tests...\n\n")

(define (run-test name code property)
  (printf "Testing: ~a\n" name)
  (if (property code)
      (printf "  PASS\n")
      (printf "  FAIL\n")))

(run-test "bool type maps to boolean (variable)"
          test-bool-type
          prop-bool-maps-to-boolean)

(run-test "bool type maps to boolean (struct field)"
          test-bool-in-struct
          prop-bool-maps-to-boolean)

(run-test "bool type maps to boolean (function return)"
          test-bool-in-function
          prop-bool-maps-to-boolean)

(run-test "arrays in structs are inline"
          test-array-in-struct
          prop-arrays-in-structs-are-inline)

(run-test "arrays with macro size in structs are inline"
          test-array-with-macro
          prop-arrays-in-structs-are-inline)

(run-test "pointer types preserved (simple)"
          test-pointer-simple
          prop-pointer-types-preserved)

(run-test "pointer types preserved (in struct)"
          test-pointer-in-struct
          prop-pointer-types-preserved)

(run-test "static functions excluded from FFI"
          test-static-function
          prop-static-functions-excluded)

(printf "\n")

;; Parser robustness tests
(printf "Parser robustness tests...\n")

(run-test "parser deterministic on valid input"
          test-array-in-struct
          prop-parser-deterministic)

(run-test "parser doesn't crash on garbage input"
          "@@@ invalid C code @#$%"
          prop-parser-no-crash)

(run-test "parser whitespace insensitive"
          "int  x  ;  "
          prop-parser-preserves-whitespace-insensitivity)

(run-test "FFI generation doesn't crash on garbage"
          "not valid C at all!!!"
          prop-ffi-no-crash)

(printf "\nAll type mapping tests completed!\n")
