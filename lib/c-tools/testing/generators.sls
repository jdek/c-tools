;; C-specific generators for property-based testing

(library (c-tools testing generators)
  (export
    ;; Type generators
    gen-c-identifier
    gen-c-type-name
    gen-basic-type
    gen-pointer-type
    gen-array-type
    gen-struct-name
    gen-enum-name

    ;; Value generators
    gen-int-literal
    gen-hex-literal
    gen-string-literal
    gen-char-literal

    ;; Declaration generators
    gen-simple-declaration
    gen-function-signature
    gen-struct-field

    ;; Preprocessor generators
    gen-macro-name
    gen-include-directive)

  (import (rnrs base)
          (rnrs lists)
          (rnrs unicode)
          (c-tools testing quickcheck)
          (c-tools utility))

  ;;=======================================================================
  ;; Helper Functions

  (define (gen-choose-char str)
    (gen-map (lambda (idx)
               (string-ref str idx))
             (gen-choose 0 (- (string-length str) 1))))

  (define (gen-list-fixed n gen)
    (make-generator
      (lambda (size)
        (let loop ([i 0] [acc '()])
          (if (>= i n)
              (reverse acc)
              (loop (+ i 1) (cons (generate gen size) acc)))))))

  (define (first-alpha-char)
    (generate (gen-choose-char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") 10))

  ;;=======================================================================
  ;; Identifier Generators

  (define gen-identifier-char
    (gen-frequency
      (list 50 (gen-choose-char "abcdefghijklmnopqrstuvwxyz"))
      (list 30 (gen-choose-char "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
      (list 15 (gen-choose-char "0123456789"))
      (list 5 (gen-return #\_))))

  ;; gen-c-identifier : => generator
  ;;   Generates valid C identifiers (alphanumeric + underscore, no leading digit).
  (define gen-c-identifier
    (gen-bind gen-boolean
              (lambda (start-with-underscore?)
                (gen-bind (gen-choose 1 15)
                          (lambda (len)
                            (gen-bind (gen-list-fixed len gen-identifier-char)
                                      (lambda (chars)
                                        (gen-return
                                          (if start-with-underscore?
                                              (string-append "_" (list->string chars))
                                              (list->string (cons (first-alpha-char) (cdr chars))))))))))))

  ;;=======================================================================
  ;; Type Name Generators

  ;; gen-basic-type : => generator
  ;;   Generates basic C type names.
  (define gen-basic-type
    (gen-one-of
      (gen-return "int")
      (gen-return "char")
      (gen-return "short")
      (gen-return "long")
      (gen-return "float")
      (gen-return "double")
      (gen-return "void")
      (gen-return "unsigned int")
      (gen-return "unsigned char")
      (gen-return "unsigned short")
      (gen-return "unsigned long")
      (gen-return "signed char")
      (gen-return "long long")
      (gen-return "unsigned long long")))

  ;; gen-struct-name : => generator
  ;;   Generates struct names (often capitalized or with specific patterns).
  (define gen-struct-name
    (gen-map (lambda (name)
               (string-append (string-upcase (substring name 0 1))
                             (substring name 1 (string-length name))))
             gen-c-identifier))

  ;; gen-enum-name : => generator
  ;;   Generates enum names.
  (define gen-enum-name gen-struct-name)

  ;; gen-c-type-name : => generator
  ;;   Generates common C type names.
  (define gen-c-type-name
    (gen-one-of
      gen-basic-type
      (gen-map (lambda (name) (string-append "struct " name)) gen-struct-name)
      (gen-map (lambda (name) (string-append "enum " name)) gen-enum-name)))

  ;;=======================================================================
  ;; Pointer and Array Type Generators

  ;; gen-pointer-type : generator => generator
  ;;   Wraps a type generator to produce pointer types.
  (define (gen-pointer-type base-gen)
    (gen-map (lambda (base)
               (string-append base "*"))
             base-gen))

  ;; gen-array-type : generator => generator
  ;;   Wraps a type generator to produce array types with sizes.
  (define (gen-array-type base-gen)
    (gen-bind base-gen
              (lambda (base)
                (gen-bind (gen-choose 1 100)
                          (lambda (size)
                            (gen-return (string-append base "[" (number->string size) "]")))))))

  ;;=======================================================================
  ;; Literal Value Generators

  ;; gen-int-literal : => generator
  ;;   Generates integer literal strings.
  (define gen-int-literal
    (gen-map number->string gen-integer))

  ;; gen-hex-literal : => generator
  ;;   Generates hexadecimal literal strings.
  (define gen-hex-literal
    (gen-map (lambda (n)
               (string-append "0x" (number->string (abs n) 16)))
             gen-natural))

  ;; gen-string-literal : => generator
  ;;   Generates C string literals (quoted, with basic escaping).
  (define gen-string-literal
    (gen-map (lambda (str)
               (string-append "\"" (escape-c-string str) "\""))
             gen-string))

  (define (escape-c-string str)
    (let loop ([i 0] [acc '()])
      (if (>= i (string-length str))
          (list->string (reverse acc))
          (let ([c (string-ref str i)])
            (case c
              [(#\newline) (loop (+ i 1) (cons #\n (cons #\\ acc)))]
              [(#\return) (loop (+ i 1) (cons #\r (cons #\\ acc)))]
              [(#\tab) (loop (+ i 1) (cons #\t (cons #\\ acc)))]
              [(#\") (loop (+ i 1) (cons #\" (cons #\\ acc)))]
              [(#\\) (loop (+ i 1) (cons #\\ (cons #\\ acc)))]
              [else (loop (+ i 1) (cons c acc))])))))

  ;; gen-char-literal : => generator
  ;;   Generates C character literals.
  (define gen-char-literal
    (gen-map (lambda (c)
               (define sq (integer->char 39))  ;; single quote
               (cond
                 [(char=? c #\newline) (string sq #\\ #\n sq)]
                 [(char=? c #\return) (string sq #\\ #\r sq)]
                 [(char=? c #\tab) (string sq #\\ #\t sq)]
                 [(char=? c sq) (string sq #\\ sq sq)]
                 [(char=? c #\\) (string sq #\\ #\\ sq)]
                 [else (string sq c sq)]))
             gen-char))

  ;;=======================================================================
  ;; Declaration Generators

  ;; gen-simple-declaration : => generator
  ;;   Generates simple variable declarations (type + identifier).
  (define gen-simple-declaration
    (gen-bind gen-basic-type
              (lambda (type)
                (gen-bind gen-c-identifier
                          (lambda (name)
                            (gen-return (string-append type " " name)))))))

  ;; gen-function-signature : => generator
  ;;   Generates function signatures.
  (define gen-function-signature
    (gen-bind gen-basic-type
              (lambda (return-type)
                (gen-bind gen-c-identifier
                          (lambda (func-name)
                            (gen-bind (gen-list gen-simple-declaration)
                                      (lambda (params)
                                        (gen-return
                                          (string-append return-type " "
                                                        func-name "("
                                                        (if (null? params)
                                                            "void"
                                                            (string-join params ", "))
                                                        ")")))))))))

  ;; gen-struct-field : => generator
  ;;   Generates struct field declarations.
  (define gen-struct-field
    (gen-bind gen-basic-type
              (lambda (type)
                (gen-bind gen-c-identifier
                          (lambda (name)
                            (gen-return (string-append type " " name ";")))))))

  ;;=======================================================================
  ;; Preprocessor Generators

  ;; gen-macro-name : => generator
  ;;   Generates macro names (typically uppercase).
  (define gen-macro-name
    (gen-map (lambda (name)
               (list->string
                 (map char-upcase (string->list name))))
             gen-c-identifier))

  ;; gen-include-directive : => generator
  ;;   Generates #include directives.
  (define gen-include-directive
    (gen-bind gen-boolean
              (lambda (is-system?)
                (gen-bind gen-c-identifier
                          (lambda (name)
                            (gen-return
                              (if is-system?
                                  (string-append "#include <" name ".h>")
                                  (string-append "#include \"" name ".h\""))))))))

) ;; end library
