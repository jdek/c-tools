;; SPDX-License-Identifier: WTFPL
;; C Parser - Recursive Descent Parser for C Declarations
;; Parses preprocessed tokens into AST nodes
;; Uses closure pattern like C++ parser

(library (c-tools parser c)
  (export parse-declarations
          parse-declaration)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (rnrs lists)
          (c-tools ast c)
          (c-tools core conditions)
          (c-tools core tokens)
          (only (chezscheme) format))

  ;; Parser entry point
  (define (parse-declarations tokens)
    (parse-tokens tokens))

  ;; Internal parser using closure pattern
  (define (parse-tokens tokens)
    ;; Mutable state
    (define pos 0)
    (define token-list (list->vector tokens))
    (define token-count (vector-length token-list))

    ;; Token operations
    (define (peek)
      (if (>= pos token-count)
          #f
          (vector-ref token-list pos)))

    (define (peek-ahead n)
      (let ([idx (+ pos n)])
        (if (>= idx token-count)
            #f
            (vector-ref token-list idx))))

    (define (advance!)
      (let ([tok (peek)])
        (when tok
          (set! pos (+ pos 1)))
        tok))

    (define (at-eof?)
      (let ([tok (peek)])
        (or (not tok) (eof-token? tok))))

    ;; Token predicates
    (define (is-keyword? tok kw)
      (and tok (keyword-token? tok) (eq? (token-value tok) kw)))

    (define (is-punct? tok p)
      (and tok (punctuator? tok) (equal? (token-value tok) p)))

    (define (is-identifier? tok)
      (and tok (identifier-token? tok)))

    ;; Error handling
    (define (parse-error msg)
      (let ([tok (peek)])
        (raise
          (condition
            (make-cpp-parse-error (if tok (token-location tok) #f))
            (make-message-condition msg)
            (make-irritants-condition (if tok (list tok) '()))))))

    ;; Expect helpers
    (define (expect-keyword kw)
      (let ([tok (peek)])
        (if (is-keyword? tok kw)
            (advance!)
            (parse-error (format "Expected keyword '~a'" kw)))))

    (define (expect-identifier)
      (let ([tok (peek)])
        (if (is-identifier? tok)
            (advance!)
            (parse-error "Expected identifier"))))

    (define (expect-punct p)
      (let ([tok (peek)])
        (if (is-punct? tok p)
            (advance!)
            (parse-error (format "Expected '~a'" p)))))

    ;;=======================================================================
    ;; Type Parsing

    ;; Parse type specifier (int, struct foo, etc.)
    (define (parse-type-specifier)
      (let ([tok (peek)])
        (cond
          [(not tok)
           (parse-error "Expected type specifier")]

          ;; Qualifiers (const, volatile)
          [(or (is-keyword? tok 'const) (is-keyword? tok 'volatile))
           (let ([qual (token-value (advance!))])
             (let ([inner (parse-type-specifier)])
               (make-qualified-type (list qual) inner)))]

          ;; Basic types
          [(and (keyword-token? tok)
                (memq (token-value tok)
                      '(void char short int long float double
                        signed unsigned)))
           (parse-basic-type)]

          ;; struct/union
          [(is-keyword? tok 'struct)
           (advance!)
           (parse-struct-or-union-type 'struct)]

          [(is-keyword? tok 'union)
           (advance!)
           (parse-struct-or-union-type 'union)]

          ;; enum
          [(is-keyword? tok 'enum)
           (advance!)
           (parse-enum-type)]

          ;; Identifier (typedef name)
          [(is-identifier? tok)
           (let ([name (token-value (advance!))])
             (make-named-type 'typedef name))]

          [else
           (parse-error "Expected type specifier")])))

    ;; Parse basic type (handles multi-word: unsigned long long, etc.)
    (define (parse-basic-type)
      (let loop ([keywords '()])
        (let ([tok (peek)])
          (if (and (keyword-token? tok)
                   (memq (token-value tok)
                         '(char short int long float double
                           signed unsigned void)))
              (begin
                (advance!)
                (loop (cons (token-value tok) keywords)))
              (make-basic-type (normalize-basic-type (reverse keywords)))))))

    ;; Normalize multi-word basic type
    (define (normalize-basic-type keywords)
      (cond
        [(null? keywords) 'int]
        [(equal? keywords '(void)) 'void]
        [(equal? keywords '(char)) 'char]
        [(memq 'short keywords) 'short]
        [(memq 'float keywords) 'float]
        [(memq 'double keywords)
         (if (memq 'long keywords) 'long-double 'double)]
        [(memq 'long keywords)
         (let ([long-count (length (filter (lambda (k) (eq? k 'long)) keywords))])
           (if (>= long-count 2)
               (if (memq 'unsigned keywords) 'unsigned-long-long 'long-long)
               (if (memq 'unsigned keywords) 'unsigned-long 'long)))]
        [(memq 'unsigned keywords) 'unsigned]
        [(memq 'signed keywords) 'int]
        [else 'int]))

    ;; Parse struct/union type
    (define (parse-struct-or-union-type kind)
      (if (is-identifier? (peek))
          (let ([name (token-value (advance!))])
            (make-named-type kind name))
          (parse-error (format "Expected ~a name" kind))))

    ;; Parse enum type
    (define (parse-enum-type)
      (if (is-identifier? (peek))
          (let ([name (token-value (advance!))])
            (make-named-type 'enum name))
          (parse-error "Expected enum name")))

    ;; Parse type with qualifiers and pointers
    (define (parse-type)
      (let ([base (parse-type-specifier)])
        (parse-type-modifiers base)))

    ;; Parse type modifiers (*, const, volatile)
    (define (parse-type-modifiers base)
      (let ([tok (peek)])
        (cond
          ;; Pointer
          [(is-punct? tok "*")
           (advance!)
           (let ([quals (parse-cv-qualifiers)])
             (let ([ptr (make-pointer-type base)])
               (parse-type-modifiers
                 (if (null? quals) ptr (make-qualified-type quals ptr)))))]

          ;; const/volatile after type
          [(or (is-keyword? tok 'const) (is-keyword? tok 'volatile))
           (let ([quals (parse-cv-qualifiers)])
             (make-qualified-type quals base))]

          ;; Done
          [else base])))

    ;; Parse const/volatile qualifiers
    (define (parse-cv-qualifiers)
      (let loop ([quals '()])
        (let ([tok (peek)])
          (cond
            [(is-keyword? tok 'const)
             (advance!)
             (loop (cons 'const quals))]
            [(is-keyword? tok 'volatile)
             (advance!)
             (loop (cons 'volatile quals))]
            [else (reverse quals)]))))

    ;;=======================================================================
    ;; Declarator Parsing

    ;; Parse declarator (name with type modifications)
    (define (parse-declarator base-type)
      ;; Handle pointer prefix
      (let ([type (parse-pointer-chain base-type)])
        (let ([tok (peek)])
          (cond
            ;; Parenthesized declarator (function pointer or array of pointers)
            [(is-punct? tok "(")
             (parse-paren-declarator type)]

            ;; Direct declarator (identifier)
            [(is-identifier? tok)
             (let ([name (token-value (advance!))])
               (parse-declarator-suffix name type))]

            ;; Abstract declarator (no name)
            [else
             (values #f type)]))))

    ;; Parse pointer chain (***)
    (define (parse-pointer-chain base-type)
      (if (is-punct? (peek) "*")
          (begin
            (advance!)
            (let ([quals (parse-cv-qualifiers)])
              (let ([ptr (make-pointer-type base-type)])
                (parse-pointer-chain
                  (if (null? quals) ptr (make-qualified-type quals ptr))))))
          base-type))

    ;; Parse parenthesized declarator
    (define (parse-paren-declarator outer-type)
      (advance!)  ;; consume (
      (let ([tok (peek)])
        (cond
          ;; Function pointer: (*name)
          [(is-punct? tok "*")
           (advance!)
           (if (is-identifier? (peek))
               (let ([name (token-value (advance!))])
                 (expect-punct ")")
                 ;; Now check for function suffix
                 (if (is-punct? (peek) "(")
                     ;; Function pointer
                     (begin
                       (advance!)
                       (let-values ([(params variadic?) (parse-parameter-list)])
                         (expect-punct ")")
                         (let ([fn-type (make-function-type outer-type params variadic?)])
                           (values name (make-pointer-type fn-type)))))
                     ;; Just pointer
                     (values name (make-pointer-type outer-type))))
               (parse-error "Expected identifier after * in parenthesized declarator"))]

          ;; Just grouping: (name)
          [(is-identifier? tok)
           (let ([name (token-value (advance!))])
             (expect-punct ")")
             (parse-declarator-suffix name outer-type))]

          [else
           (parse-error "Expected identifier or * in parenthesized declarator")])))

    ;; Parse declarator suffix (arrays and functions)
    (define (parse-declarator-suffix name type)
      (let ([tok (peek)])
        (cond
          ;; Array suffix [size]
          [(is-punct? tok "[")
           (advance!)
           (skip-to-bracket)
           (expect-punct "]")
           (parse-declarator-suffix name (make-array-type type #f))]

          ;; Function suffix (params)
          [(is-punct? tok "(")
           (advance!)
           (let-values ([(params variadic?) (parse-parameter-list)])
             (expect-punct ")")
             (values name (make-function-type type params variadic?)))]

          ;; Done
          [else
           (values name type)])))

    ;; Skip to closing bracket (array size expressions)
    (define (skip-to-bracket)
      (let loop ([depth 1])
        (let ([tok (advance!)])
          (cond
            [(not tok) (parse-error "Unexpected end of input")]
            [(is-punct? tok "[") (loop (+ depth 1))]
            [(is-punct? tok "]")
             (when (> depth 1)
               (loop (- depth 1)))]
            [else (loop depth)]))))

    ;;=======================================================================
    ;; Declaration Parsing

    ;; Parse declaration
    (define (parse-declaration)
      (let ([tok (peek)])
        (cond
          [(not tok) #f]
          [(eof-token? tok) #f]

          ;; typedef
          [(is-keyword? tok 'typedef)
           (parse-typedef)]

          ;; struct/union definition
          [(is-keyword? tok 'struct)
           (if (is-struct-definition?)
               (parse-struct-definition)
               (parse-function-or-variable))]

          [(is-keyword? tok 'union)
           (if (is-union-definition?)
               (parse-union-definition)
               (parse-function-or-variable))]

          ;; enum definition
          [(is-keyword? tok 'enum)
           (if (is-enum-definition?)
               (parse-enum-definition)
               (parse-function-or-variable))]

          ;; Function or variable
          [else
           (parse-function-or-variable)])))

    ;; Check if struct definition
    (define (is-struct-definition?)
      (let ([tok1 (peek-ahead 1)])
        (and (is-identifier? tok1)
             (let ([tok2 (peek-ahead 2)])
               (is-punct? tok2 "{")))))

    ;; Check if union definition
    (define (is-union-definition?)
      (let ([tok1 (peek-ahead 1)])
        (and (is-identifier? tok1)
             (let ([tok2 (peek-ahead 2)])
               (is-punct? tok2 "{")))))

    ;; Check if enum definition
    (define (is-enum-definition?)
      (let ([tok1 (peek-ahead 1)])
        (and (is-identifier? tok1)
             (let ([tok2 (peek-ahead 2)])
               (is-punct? tok2 "{")))))

    ;; Parse typedef
    (define (parse-typedef)
      (expect-keyword 'typedef)
      (let ([type (parse-type)])
        (let-values ([(name decl-type) (parse-declarator type)])
          (expect-punct ";")
          (make-typedef name decl-type))))

    ;; Parse struct definition
    (define (parse-struct-definition)
      (expect-keyword 'struct)
      (let ([name (token-value (expect-identifier))])
        (expect-punct "{")
        (let ([fields (parse-field-list)])
          (expect-punct "}")
          (expect-punct ";")
          (make-struct-decl name fields))))

    ;; Parse union definition
    (define (parse-union-definition)
      (expect-keyword 'union)
      (let ([name (token-value (expect-identifier))])
        (expect-punct "{")
        (let ([fields (parse-field-list)])
          (expect-punct "}")
          (expect-punct ";")
          (make-union-decl name fields))))

    ;; Parse field list
    (define (parse-field-list)
      (let loop ([fields '()])
        (if (is-punct? (peek) "}")
            (reverse fields)
            (loop (cons (parse-field) fields)))))

    ;; Parse single field
    (define (parse-field)
      (let ([type (parse-type)])
        (let-values ([(name field-type) (parse-declarator type)])
          (expect-punct ";")
          (make-field name field-type))))

    ;; Parse enum definition
    (define (parse-enum-definition)
      (expect-keyword 'enum)
      (let ([name (token-value (expect-identifier))])
        (expect-punct "{")
        (let ([enumerators (parse-enumerator-list 0)])
          (expect-punct "}")
          (expect-punct ";")
          (make-enum-decl name enumerators))))

    ;; Parse enumerator list
    (define (parse-enumerator-list current-value)
      (if (is-punct? (peek) "}")
          '()
          (let loop ([enums '()] [value current-value])
            (let ([enum (parse-enumerator value)])
              (if (is-punct? (peek) ",")
                  (begin
                    (advance!)
                    (if (is-punct? (peek) "}")
                        (reverse (cons enum enums))
                        (loop (cons enum enums) (+ (enumerator-value enum) 1))))
                  (reverse (cons enum enums)))))))

    ;; Parse single enumerator
    (define (parse-enumerator current-value)
      (let ([name (token-value (expect-identifier))])
        (let ([val (if (is-punct? (peek) "=")
                       (begin
                         (advance!)
                         (let ([tok (peek)])
                           (if (number-token? tok)
                               (begin
                                 (advance!)
                                 (string->number (token-value tok)))
                               current-value)))
                       current-value)])
          (make-enumerator name val))))

    ;; Parse function or variable declaration
    (define (parse-function-or-variable)
      (let ([type (parse-type)])
        (let-values ([(name decl-type) (parse-declarator type)])
          (expect-punct ";")
          (if (function-type? decl-type)
              (make-function-decl name
                                  (function-type-return decl-type)
                                  (function-type-params decl-type)
                                  (function-type-variadic? decl-type))
              (make-field name decl-type)))))

    ;;=======================================================================
    ;; Parameter Parsing

    ;; Parse parameter list
    (define (parse-parameter-list)
      (if (is-punct? (peek) ")")
          (values '() #f)
          (let loop ([params '()])
            (let ([tok (peek)])
              (cond
                ;; Variadic
                [(is-punct? tok "...")
                 (advance!)
                 (values (reverse params) #t)]

                ;; void (no parameters)
                [(and (null? params)
                      (is-keyword? tok 'void)
                      (is-punct? (peek-ahead 1) ")"))
                 (advance!)
                 (values '() #f)]

                ;; Regular parameter
                [else
                 (let ([param (parse-parameter)])
                   (if (is-punct? (peek) ",")
                       (begin (advance!) (loop (cons param params)))
                       (values (reverse (cons param params)) #f)))])))))

    ;; Parse single parameter
    (define (parse-parameter)
      (let ([type (parse-type)])
        (let-values ([(name param-type) (parse-declarator type)])
          (make-param name param-type))))

    ;;=======================================================================
    ;; Main Parse Loop

    (let loop ([decls '()])
      (if (at-eof?)
          (reverse decls)
          (let ([decl (parse-declaration)])
            (if decl
                (loop (cons decl decls))
                (loop decls))))))

  ;; Single declaration entry point
  (define (parse-declaration tokens)
    (let ([decls (parse-declarations tokens)])
      (if (null? decls)
          #f
          (car decls)))))
