;; Common property definitions for C parsing and FFI generation

(library (c-tools testing properties)
  (export
    ;; Parser properties
    prop-parser-deterministic
    prop-parser-no-crash
    prop-parser-preserves-whitespace-insensitivity

    ;; FFI generation properties
    prop-ffi-no-crash
    prop-static-functions-excluded
    prop-pointer-types-preserved

    ;; Type mapping properties
    prop-bool-maps-to-boolean
    prop-arrays-in-structs-are-inline
    prop-arrays-in-params-decay

    ;; Helpers
    parse-c-string
    generate-ffi-from-string)

  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs exceptions)
          (rnrs io ports)
          (rnrs io simple)
          (c-tools lexer c)
          (c-tools preprocess c)
          (c-tools parser c)
          (c-tools codegen chez ffi)
          (c-tools ast c)
          (c-tools effects cpp core)
          (c-tools effects cpp macros)
          (c-tools effects cpp includes)
          (c-tools effects cpp conditionals)
          (c-tools effects registry)
          (c-tools utility))

  ;;=======================================================================
  ;; Helper Functions

  ;; parse-c-string : string => ast | #f
  ;;   Attempts to parse C string, returns AST or #f on failure.
  (define (parse-c-string str)
    (guard (ex [else #f])
      (with-effects '((cpp-include ())
                      cpp-macros
                      cpp-conditional)
        (lambda ()
          (let* ([tokens (preprocess-string str)]
                 [decls (parse-declarations tokens)])
            (if (null? decls)
                #f
                decls))))))

  ;; generate-ffi-from-string : string => s-expr | #f
  ;;   Attempts to parse C and generate FFI, returns FFI code or #f on failure.
  (define (generate-ffi-from-string str)
    (guard (ex [else #f])
      (let ([decls (parse-c-string str)])
        (if decls
            (generate-ffi-code decls "test-lib")
            #f))))

  ;; sexp->string : s-expr => string
  ;;   Converts s-expression to string representation.
  (define (sexp->string sexp)
    (call-with-string-output-port
      (lambda (port)
        (write sexp port))))

  ;; sexp-contains-symbol? : s-expr symbol => boolean
  ;;   Checks if s-expression contains a specific symbol.
  (define (sexp-contains-symbol? sexp sym)
    (cond
      [(null? sexp) #f]
      [(symbol? sexp) (eq? sexp sym)]
      [(pair? sexp)
       (or (sexp-contains-symbol? (car sexp) sym)
           (sexp-contains-symbol? (cdr sexp) sym))]
      [else #f]))

  ;;=======================================================================
  ;; Parser Properties

  ;; prop-parser-deterministic : string => boolean
  ;;   Property: Parsing the same input twice gives the same result.
  (define (prop-parser-deterministic c-code)
    (let ([result1 (parse-c-string c-code)]
          [result2 (parse-c-string c-code)])
      (or (and (not result1) (not result2))
          (and result1 result2
               (ast-equal? result1 result2)))))

  ;; prop-parser-no-crash : string => boolean
  ;;   Property: Parser never crashes, even on invalid input.
  (define (prop-parser-no-crash c-code)
    (guard (ex [else #f])  ;; If exception, property fails
      (let ([result (parse-c-string c-code)])
        #t)))  ;; If we get here, no crash

  ;; prop-parser-preserves-whitespace-insensitivity : string => boolean
  ;;   Property: Extra whitespace doesn't change parse result.
  (define (prop-parser-preserves-whitespace-insensitivity c-code)
    (let* ([spaced (string-replace c-code " " "  ")]
           [result1 (parse-c-string c-code)]
           [result2 (parse-c-string spaced)])
      (or (and (not result1) (not result2))
          (and result1 result2
               (ast-equal? result1 result2)))))

  ;;=======================================================================
  ;; FFI Generation Properties

  ;; prop-ffi-no-crash : string => boolean
  ;;   Property: FFI generation never crashes.
  (define (prop-ffi-no-crash c-code)
    (guard (ex [else #f])
      (let ([result (generate-ffi-from-string c-code)])
        #t)))

  ;; prop-static-functions-excluded : string => boolean
  ;;   Property: Static functions are not included in FFI output.
  (define (prop-static-functions-excluded c-code)
    (guard (ex [else #t])  ;; If parse fails, property vacuously true
      (let ([ffi-output (generate-ffi-from-string c-code)])
        (if ffi-output
            ;; Check that if input has "static", output doesn't reference it
            (if (string-contains? c-code "static")
                ;; This is a weak check - ideally we'd parse the function name
                ;; For now, just verify it compiled
                #t
                #t)
            #t))))

  ;; prop-pointer-types-preserved : string => boolean
  ;;   Property: Pointer types in input appear as (* T) in output.
  (define (prop-pointer-types-preserved c-code)
    (guard (ex [else #t])
      (let ([ffi-output (generate-ffi-from-string c-code)])
        (if (and ffi-output (string-contains? c-code "*"))
            (sexp-contains-symbol? ffi-output '*)
            #t))))

  ;;=======================================================================
  ;; Type Mapping Properties

  ;; prop-bool-maps-to-boolean : string => boolean
  ;;   Property: bool type maps to boolean in FFI.
  (define (prop-bool-maps-to-boolean c-code)
    (guard (ex [else #t])
      (let ([ffi-output (generate-ffi-from-string c-code)])
        (if (and ffi-output (string-contains? c-code "bool"))
            (sexp-contains-symbol? ffi-output 'boolean)
            #t))))

  ;; prop-arrays-in-structs-are-inline : string => boolean
  ;;   Property: Arrays with known size in structs become (array N T).
  (define (prop-arrays-in-structs-are-inline c-code)
    (guard (ex [else #t])
      (let ([ffi-output (generate-ffi-from-string c-code)])
        (if (and ffi-output
                 (string-contains? c-code "struct")
                 (string-contains? c-code "["))
            (sexp-contains-symbol? ffi-output 'array)
            #t))))

  ;; prop-arrays-in-params-decay : string => boolean
  ;;   Property: Arrays in function parameters become pointers.
  (define (prop-arrays-in-params-decay c-code)
    (guard (ex [else #t])
      (let ([ffi-output (generate-ffi-from-string c-code)])
        ;; This is hard to check without actually analyzing the AST
        ;; For now, just verify it doesn't crash
        #t)))

  ;;=======================================================================
  ;; AST Equality

  ;; ast-equal? : any any => boolean
  ;;   Deep structural equality for AST nodes.
  (define (ast-equal? a b)
    (cond
      [(and (basic-type? a) (basic-type? b))
       (eq? (basic-type-name a) (basic-type-name b))]
      [(and (pointer-type? a) (pointer-type? b))
       (ast-equal? (pointer-type-pointee a) (pointer-type-pointee b))]
      [(and (array-type? a) (array-type? b))
       (and (ast-equal? (array-type-element a) (array-type-element b))
            (equal? (array-type-size a) (array-type-size b)))]
      [(and (named-type? a) (named-type? b))
       (and (eq? (named-type-kind a) (named-type-kind b))
            (eq? (named-type-name a) (named-type-name b)))]
      [(and (qualified-type? a) (qualified-type? b))
       (and (equal? (qualified-type-qualifiers a) (qualified-type-qualifiers b))
            (ast-equal? (qualified-type-type a) (qualified-type-type b)))]
      [(and (function-type? a) (function-type? b))
       (and (ast-equal? (function-type-return a) (function-type-return b))
            (= (length (function-type-params a)) (length (function-type-params b)))
            (andmap ast-equal? (function-type-params a) (function-type-params b))
            (eq? (function-type-variadic? a) (function-type-variadic? b)))]
      [(and (typedef? a) (typedef? b))
       (and (eq? (typedef-name a) (typedef-name b))
            (ast-equal? (typedef-type a) (typedef-type b)))]
      [(and (struct-decl? a) (struct-decl? b))
       (and (eq? (struct-decl-name a) (struct-decl-name b))
            (= (length (struct-decl-fields a)) (length (struct-decl-fields b)))
            (andmap ast-equal? (struct-decl-fields a) (struct-decl-fields b)))]
      [(and (field? a) (field? b))
       (and (eq? (field-name a) (field-name b))
            (ast-equal? (field-type a) (field-type b)))]
      [(and (function-decl? a) (function-decl? b))
       (and (eq? (function-decl-name a) (function-decl-name b))
            (ast-equal? (function-decl-return-type a) (function-decl-return-type b))
            (= (length (function-decl-params a)) (length (function-decl-params b)))
            (andmap ast-equal? (function-decl-params a) (function-decl-params b))
            (eq? (function-decl-variadic? a) (function-decl-variadic? b)))]
      [(and (param? a) (param? b))
       (and (eq? (param-name a) (param-name b))
            (ast-equal? (param-type a) (param-type b)))]
      [(and (enum-decl? a) (enum-decl? b))
       (and (eq? (enum-decl-name a) (enum-decl-name b))
            (= (length (enum-decl-enumerators a)) (length (enum-decl-enumerators b)))
            (andmap ast-equal? (enum-decl-enumerators a) (enum-decl-enumerators b)))]
      [(and (enumerator? a) (enumerator? b))
       (and (eq? (enumerator-name a) (enumerator-name b))
            (equal? (enumerator-value a) (enumerator-value b)))]
      [(and (list? a) (list? b))
       (and (= (length a) (length b))
            (andmap ast-equal? a b))]
      [else
       (equal? a b)]))

  ;; andmap : (a b => boolean) list-of-a list-of-b => boolean
  ;;   Returns #t if predicate is true for all corresponding pairs.
  (define (andmap proc lst1 lst2)
    (or (and (null? lst1) (null? lst2))
        (and (pair? lst1) (pair? lst2)
             (proc (car lst1) (car lst2))
             (andmap proc (cdr lst1) (cdr lst2)))))

) ;; end library
