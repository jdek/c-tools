;; C Parser - Recursive Descent Parser for C Declarations
;; Parses preprocessed tokens into AST nodes
;; Supports function pointers, arrays, and complex declarators

(library (c-tools parser c)
  (export parse-declarations
          parse-declaration)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (rnrs lists)
          (rnrs records syntactic)
          (c-tools ast c)
          (c-tools core conditions)
          (c-tools core tokens)
          (only (chezscheme) format))

  ;;; Parser State

  ;; Parser state: tokens and current position
  (define-record-type (parser-state create-parser-state parser-state?)
    (fields tokens     ;; list of tokens
            index))    ;; current position

  ;; Helper to create initial parser state
  (define (make-parser-state tokens)
    (create-parser-state tokens 0))

  ;;; Token Stream Operations

  ;; Peek at current token
  (define (peek-token state)
    (let ([tokens (parser-state-tokens state)]
          [index (parser-state-index state)])
      (if (>= index (length tokens))
          #f
          (list-ref tokens index))))

  ;; Peek ahead n tokens
  (define (peek-token-ahead state n)
    (let ([tokens (parser-state-tokens state)]
          [index (+ (parser-state-index state) n)])
      (if (>= index (length tokens))
          #f
          (list-ref tokens index))))

  ;; Consume current token and advance
  (define (consume-token state)
    (create-parser-state
     (parser-state-tokens state)
     (+ (parser-state-index state) 1)))

  ;; Check if current token matches predicate
  (define (match-token? state pred)
    (let ([tok (peek-token state)])
      (and tok (pred tok))))

  ;; Expect a specific token type
  (define (expect-token state pred error-msg)
    (let ([tok (peek-token state)])
      (if (and tok (pred tok))
          (values tok (consume-token state))
          (parse-error tok error-msg))))

  ;; Expect a keyword
  (define (expect-keyword state kw)
    (expect-token state
                  (lambda (t) (and (keyword-token? t) (eq? (token-value t) kw)))
                  (format "Expected keyword '~a'" kw)))

  ;; Expect an identifier
  (define (expect-identifier state)
    (expect-token state
                  identifier-token?
                  "Expected identifier"))

  ;; Expect a punctuator
  (define (expect-punct state punct)
    (expect-token state
                  (lambda (t) (and (punctuator? t) (equal? (token-value t) punct)))
                  (format "Expected '~a'" punct)))

  ;; Check if at end of input
  (define (at-eof? state)
    (let ([tok (peek-token state)])
      (or (not tok) (eof-token? tok))))

  ;; Raise parse error
  (define (parse-error tok msg)
    (raise
     (condition
      (make-cpp-parse-error (if tok (token-location tok) #f))
      (make-message-condition msg)
      (make-irritants-condition (if tok (list tok) '())))))

  ;; Helper predicates
  (define (is-punct? tok p)
    (and tok (punctuator? tok) (equal? (token-value tok) p)))

  (define (is-keyword? tok kw)
    (and tok (keyword-token? tok) (eq? (token-value tok) kw)))

  (define (is-identifier? tok)
    (and tok (identifier-token? tok)))

  ;;; Type Parsing

  ;; Parse type specifier (int, char, struct foo, etc.)
  (define (parse-type-specifier state)
    (let ([tok (peek-token state)])
      (cond
        [(not tok) (parse-error tok "Expected type specifier")]

        ;; Basic types
        [(and (keyword-token? tok)
              (memq (token-value tok) '(void char short int long float double signed unsigned)))
         (let ([state (consume-token state)])
           ;; Handle multi-word types (unsigned int, long long, etc.)
           (parse-type-specifier-tail state (list (token-value tok))))]

        ;; struct
        [(and (keyword-token? tok) (eq? (token-value tok) 'struct))
         (parse-struct-or-union-type state 'struct)]

        ;; union
        [(and (keyword-token? tok) (eq? (token-value tok) 'union))
         (parse-struct-or-union-type state 'union)]

        ;; enum
        [(and (keyword-token? tok) (eq? (token-value tok) 'enum))
         (parse-enum-type state)]

        ;; typedef name
        [(identifier-token? tok)
         (let ([name (token-value tok)]
               [state (consume-token state)])
           (values (make-named-type 'typedef name) state))]

        [else
         (parse-error tok "Expected type specifier")])))

  ;; Parse tail of multi-word type specifier
  (define (parse-type-specifier-tail state keywords)
    (let ([tok (peek-token state)])
      (if (and tok
               (keyword-token? tok)
               (memq (token-value tok) '(char short int long signed unsigned)))
          ;; Continue collecting keywords
          (parse-type-specifier-tail (consume-token state)
                                     (cons (token-value tok) keywords))
          ;; Done - normalize to a basic type
          (values (make-basic-type (normalize-basic-type (reverse keywords)))
                  state))))

  ;; Normalize multi-word basic type to canonical form
  (define (normalize-basic-type keywords)
    (cond
      [(equal? keywords '(void)) 'void]
      [(equal? keywords '(char)) 'char]
      [(memq 'short keywords) 'short]
      [(memq 'long keywords)
       (if (>= (length (filter (lambda (k) (eq? k 'long)) keywords)) 2)
           'long-long
           'long)]
      [(memq 'float keywords) 'float]
      [(memq 'double keywords) 'double]
      [(memq 'unsigned keywords) 'unsigned]
      [else 'int]))

  ;; Parse struct/union type
  (define (parse-struct-or-union-type state kind)
    (let* ([state (consume-token state)]  ;; consume 'struct' or 'union'
           [tok (peek-token state)])
      (cond
        ;; struct/union name
        [(identifier-token? tok)
         (let ([name (token-value tok)]
               [state (consume-token state)])
           (values (make-named-type kind name) state))]

        ;; Anonymous struct/union with body - not needed for declarations
        [else
         (parse-error tok (format "Expected ~a name" kind))])))

  ;; Parse enum type
  (define (parse-enum-type state)
    (let* ([state (consume-token state)]  ;; consume 'enum'
           [tok (peek-token state)])
      (cond
        ;; enum name
        [(identifier-token? tok)
         (let ([name (token-value tok)]
               [state (consume-token state)])
           (values (make-named-type 'enum name) state))]

        ;; Anonymous enum - not needed for declarations
        [else
         (parse-error tok "Expected enum name")])))

  ;; Parse type qualifiers (const, volatile)
  (define (parse-type-qualifiers state)
    (let loop ([state state] [quals '()])
      (let ([tok (peek-token state)])
        (if (and tok
                 (keyword-token? tok)
                 (memq (token-value tok) '(const volatile)))
            (loop (consume-token state) (cons (token-value tok) quals))
            (values (reverse quals) state)))))

  ;; Parse complete type (with pointers, qualifiers, etc.)
  (define (parse-type state)
    ;; First parse any leading qualifiers (const, volatile)
    (let-values ([(quals state) (parse-type-qualifiers state)])
      (let-values ([(base-type state) (parse-type-specifier state)])
        ;; Apply qualifiers to base type if any
        (let ([qualified-type (if (null? quals)
                                  base-type
                                  (make-qualified-type quals base-type))])
          (parse-declarator-type state qualified-type)))))

  ;; Parse declarator modifiers (*, [], ())
  ;; This handles simple pointer chains but not full declarator syntax
  (define (parse-declarator-type state base-type)
    (let ([tok (peek-token state)])
      (cond
        ;; Pointer
        [(is-punct? tok "*")
         (let ([state (consume-token state)])
           ;; Parse pointer qualifiers
           (let-values ([(quals state) (parse-type-qualifiers state)])
             (let ([ptr-type (if (null? quals)
                                 (make-pointer-type base-type)
                                 (make-qualified-type quals (make-pointer-type base-type)))])
               (parse-declarator-type state ptr-type))))]

        ;; Done with declarator
        [else
         (values base-type state)])))

  ;; Parse a full declarator (name + type modifiers)
  ;; Returns: (values name final-type state)
  ;; Handles: *name, name[], name[N], (*name)(params), (*name)[N]
  (define (parse-declarator state base-type)
    ;; First collect pointer prefixes
    (let-values ([(ptr-type state) (parse-pointer-chain state base-type)])
      (let ([tok (peek-token state)])
        (cond
          ;; Parenthesized declarator: (declarator) or (*name)
          [(is-punct? tok "(")
           (parse-paren-declarator state ptr-type)]

          ;; Direct declarator with identifier
          [(is-identifier? tok)
           (let ([name (token-value tok)]
                 [state (consume-token state)])
             ;; Parse suffix modifiers (arrays, function params)
             (parse-declarator-suffix state name ptr-type))]

          ;; Abstract declarator (no name)
          [else
           (parse-declarator-suffix state #f ptr-type)]))))

  ;; Parse chain of pointer modifiers: * const * volatile *
  (define (parse-pointer-chain state base-type)
    (let ([tok (peek-token state)])
      (if (is-punct? tok "*")
          (let ([state (consume-token state)])
            (let-values ([(quals state) (parse-type-qualifiers state)])
              (let ([ptr-type (make-pointer-type base-type)])
                (parse-pointer-chain state
                  (if (null? quals) ptr-type (make-qualified-type quals ptr-type))))))
          (values base-type state))))

  ;; Parse parenthesized declarator: (*name), (*name)(params), etc.
  (define (parse-paren-declarator state outer-type)
    (let ([state (consume-token state)])  ;; consume '('
      (let ([tok (peek-token state)])
        (cond
          ;; Function pointer: (*name)(params) or (*)(params)
          [(is-punct? tok "*")
           (let ([state (consume-token state)])  ;; consume '*'
             (let-values ([(quals state) (parse-type-qualifiers state)])
               (let ([tok2 (peek-token state)])
                 (cond
                   ;; Named function pointer: (*name)
                   [(is-identifier? tok2)
                    (let ([name (token-value tok2)]
                          [state (consume-token state)])  ;; consume name
                      (let-values ([(_ state) (expect-punct state ")")])  ;; consume ')'
                        ;; Now parse the parameter list
                        (let ([tok3 (peek-token state)])
                          (if (is-punct? tok3 "(")
                              ;; Function pointer
                              (let ([state (consume-token state)])  ;; consume '('
                                (let-values ([(params variadic? state) (parse-parameter-list state)])
                                  (let-values ([(_ state) (expect-punct state ")")])
                                    ;; Build function pointer type
                                    (let ([fn-type (make-function-type outer-type
                                                                       (map param-type params)
                                                                       variadic?)])
                                      ;; Check for array suffix on function pointer
                                      (parse-declarator-suffix state name (make-pointer-type fn-type))))))
                              ;; Just pointer to outer type
                              (values name (make-pointer-type outer-type) state)))))]

                   ;; Abstract function pointer: (*)
                   [(is-punct? tok2 ")")
                    (let ([state (consume-token state)])  ;; consume ')'
                      (let ([tok3 (peek-token state)])
                        (if (is-punct? tok3 "(")
                            ;; Function pointer without name
                            (let ([state (consume-token state)])
                              (let-values ([(params variadic? state) (parse-parameter-list state)])
                                (let-values ([(_ state) (expect-punct state ")")])
                                  (let ([fn-type (make-function-type outer-type
                                                                     (map param-type params)
                                                                     variadic?)])
                                    (values #f (make-pointer-type fn-type) state)))))
                            ;; Just pointer
                            (values #f (make-pointer-type outer-type) state))))]

                   [else
                    (parse-error tok2 "Expected identifier or ')' in declarator")]))))]

          ;; Nested declarator without pointer
          [(is-identifier? tok)
           ;; This handles grouping: int (x)[10]
           (let ([name (token-value tok)]
                 [state (consume-token state)])
             (let-values ([(_ state) (expect-punct state ")")])
               (parse-declarator-suffix state name outer-type)))]

          [else
           (parse-error tok "Expected declarator")]))))

  ;; Parse declarator suffix: arrays [N] and function params (...)
  (define (parse-declarator-suffix state name type)
    (let ([tok (peek-token state)])
      (cond
        ;; Array suffix: [N] or []
        [(is-punct? tok "[")
         (let ([state (consume-token state)])  ;; consume '['
           (let ([tok2 (peek-token state)])
             (cond
               ;; Unsized array: []
               [(is-punct? tok2 "]")
                (let ([state (consume-token state)])
                  (parse-declarator-suffix state name (make-array-type type #f)))]
               ;; Sized array: [N]
               [(number-token? tok2)
                (let ([size (string->number (token-value tok2))]
                      [state (consume-token state)])
                  (let-values ([(_ state) (expect-punct state "]")])
                    (parse-declarator-suffix state name (make-array-type type size))))]
               ;; Expression in array size - skip to ]
               [else
                (let-values ([(state size) (skip-to-bracket state)])
                  (parse-declarator-suffix state name (make-array-type type size)))])))]

        ;; Function suffix (for function declarations, not function pointers)
        ;; This is handled at higher level, so just return here
        [else
         (values name type state)])))

  ;; Skip to closing bracket, try to parse size
  (define (skip-to-bracket state)
    (let loop ([state state] [depth 1])
      (let ([tok (peek-token state)])
        (cond
          [(not tok)
           (values state #f)]
          [(is-punct? tok "]")
           (if (= depth 1)
               (values (consume-token state) #f)
               (loop (consume-token state) (- depth 1)))]
          [(is-punct? tok "[")
           (loop (consume-token state) (+ depth 1))]
          [else
           (loop (consume-token state) depth)]))))

  ;;; Declaration Parsing

  ;; Parse a single declaration
  (define (parse-declaration state)
    (let ([tok (peek-token state)])
      (cond
        [(not tok) (values #f state)]
        [(eof-token? tok) (values #f state)]

        ;; typedef
        [(and (keyword-token? tok) (eq? (token-value tok) 'typedef))
         (parse-typedef state)]

        ;; struct definition (only if followed by name and '{')
        [(and (keyword-token? tok) (eq? (token-value tok) 'struct))
         (if (is-struct-definition? state)
             (parse-struct-definition state)
             (parse-function-or-variable state))]

        ;; union definition (only if followed by name and '{')
        [(and (keyword-token? tok) (eq? (token-value tok) 'union))
         (if (is-union-definition? state)
             (parse-union-definition state)
             (parse-function-or-variable state))]

        ;; enum definition (only if followed by name and '{')
        [(and (keyword-token? tok) (eq? (token-value tok) 'enum))
         (if (is-enum-definition? state)
             (parse-enum-definition state)
             (parse-function-or-variable state))]

        ;; Function or variable declaration
        [else
         (parse-function-or-variable state)])))

  ;; Check if this is a struct definition (struct name { ... })
  (define (is-struct-definition? state)
    (let* ([state (consume-token state)]  ;; skip 'struct'
           [tok1 (peek-token state)])
      (and (identifier-token? tok1)
           (let* ([state (consume-token state)]  ;; skip name
                  [tok2 (peek-token state)])
             (and tok2
                  (punctuator? tok2)
                  (equal? (token-value tok2) "{"))))))

  ;; Check if this is a union definition (union name { ... })
  (define (is-union-definition? state)
    (let* ([state (consume-token state)]  ;; skip 'union'
           [tok1 (peek-token state)])
      (and (identifier-token? tok1)
           (let* ([state (consume-token state)]  ;; skip name
                  [tok2 (peek-token state)])
             (and tok2
                  (punctuator? tok2)
                  (equal? (token-value tok2) "{"))))))

  ;; Check if this is an enum definition (enum name { ... })
  (define (is-enum-definition? state)
    (let* ([state (consume-token state)]  ;; skip 'enum'
           [tok1 (peek-token state)])
      (and (identifier-token? tok1)
           (let* ([state (consume-token state)]  ;; skip name
                  [tok2 (peek-token state)])
             (and tok2
                  (punctuator? tok2)
                  (equal? (token-value tok2) "{"))))))

  ;; Parse typedef declaration
  (define (parse-typedef state)
    (let ([state (consume-token state)])  ;; consume 'typedef'
      ;; Parse base type
      (let-values ([(quals state) (parse-type-qualifiers state)])
        (let-values ([(base-type state) (parse-type-specifier state)])
          (let ([qualified-base (if (null? quals)
                                    base-type
                                    (make-qualified-type quals base-type))])
            ;; Parse declarator
            (let-values ([(name decl-type state) (parse-declarator state qualified-base)])
              (let-values ([(_ state) (expect-punct state ";")])
                (values (make-typedef name decl-type) state))))))))

  ;; Parse struct definition
  (define (parse-struct-definition state)
    (let* ([state (consume-token state)])  ;; consume 'struct'
      (let-values ([(name-tok state) (expect-identifier state)])
        (let* ([name (token-value name-tok)]
               [state (consume-token state)])  ;; consume '{'
          (let-values ([(fields state) (parse-field-list state)])
            (let ([state (consume-token state)])  ;; consume '}'
              (let ([state (consume-token state)])  ;; consume ';'
                (values (make-struct-decl name fields) state))))))))

  ;; Parse union definition
  (define (parse-union-definition state)
    (let* ([state (consume-token state)])  ;; consume 'union'
      (let-values ([(name-tok state) (expect-identifier state)])
        (let* ([name (token-value name-tok)]
               [state (consume-token state)])  ;; consume '{'
          (let-values ([(fields state) (parse-field-list state)])
            (let ([state (consume-token state)])  ;; consume '}'
              (let ([state (consume-token state)])  ;; consume ';'
                (values (make-union-decl name fields) state))))))))

  ;; Parse field list for struct/union
  (define (parse-field-list state)
    (let loop ([state state] [fields '()])
      (let ([tok (peek-token state)])
        (if (and tok (punctuator? tok) (equal? (token-value tok) "}"))
            (values (reverse fields) state)
            (let-values ([(field state) (parse-field state)])
              (loop state (cons field fields)))))))

  ;; Parse a single field
  (define (parse-field state)
    ;; Parse base type
    (let-values ([(quals state) (parse-type-qualifiers state)])
      (let-values ([(base-type state) (parse-type-specifier state)])
        (let ([qualified-base (if (null? quals)
                                  base-type
                                  (make-qualified-type quals base-type))])
          ;; Parse declarator
          (let-values ([(name decl-type state) (parse-declarator state qualified-base)])
            (let-values ([(_ state) (expect-punct state ";")])
              (values (make-field name decl-type) state)))))))

  ;; Parse enum definition
  (define (parse-enum-definition state)
    (let* ([state (consume-token state)])  ;; consume 'enum'
      (let-values ([(name-tok state) (expect-identifier state)])
        (let* ([name (token-value name-tok)]
               [state (consume-token state)])  ;; consume '{'
          (let-values ([(enumerators state) (parse-enumerator-list state)])
            (let ([state (consume-token state)])  ;; consume '}'
              (let ([state (consume-token state)])  ;; consume ';'
                (values (make-enum-decl name enumerators) state))))))))

  ;; Parse enumerator list
  (define (parse-enumerator-list state)
    (let loop ([state state] [enums '()] [current-value 0])
      (let ([tok (peek-token state)])
        (if (and tok (punctuator? tok) (equal? (token-value tok) "}"))
            (values (reverse enums) state)
            (let-values ([(enum state value) (parse-enumerator state current-value)])
              (let ([tok (peek-token state)])
                ;; Skip optional comma
                (let ([state (if (and tok (punctuator? tok) (equal? (token-value tok) ","))
                                 (consume-token state)
                                 state)])
                  (loop state (cons enum enums) value))))))))

  ;; Parse a single enumerator
  (define (parse-enumerator state current-value)
    (let-values ([(name-tok state) (expect-identifier state)])
      (let* ([name (token-value name-tok)]
             [tok (peek-token state)])
        (if (and tok (punctuator? tok) (equal? (token-value tok) "="))
            ;; Has explicit value
            (let* ([state (consume-token state)]
                   [val-tok (peek-token state)])
              (if (number-token? val-tok)
                  (let ([value (string->number (token-value val-tok))]
                        [state (consume-token state)])
                    (values (make-enumerator name value) state (+ value 1)))
                  (parse-error val-tok "Expected number in enumerator")))
            ;; Use current value
            (values (make-enumerator name current-value) state (+ current-value 1))))))

  ;; Parse function or variable declaration
  (define (parse-function-or-variable state)
    ;; Parse base type (without pointer modifiers)
    (let-values ([(quals state) (parse-type-qualifiers state)])
      (let-values ([(base-type state) (parse-type-specifier state)])
        (let ([qualified-base (if (null? quals)
                                  base-type
                                  (make-qualified-type quals base-type))])
          ;; Parse declarator (handles pointers, arrays, function pointers)
          (let-values ([(name decl-type state) (parse-declarator state qualified-base)])
            (let ([tok (peek-token state)])
              (cond
                ;; Function declaration: name(params)
                [(is-punct? tok "(")
                 (let ([state (consume-token state)])  ;; consume '('
                   (let-values ([(params variadic? state) (parse-parameter-list state)])
                     (let-values ([(_ state) (expect-punct state ")")])
                       (let-values ([(_ state) (expect-punct state ";")])
                         (values (make-function-decl name decl-type params variadic?) state)))))]

                ;; Variable declaration or function pointer typedef
                [(is-punct? tok ";")
                 (let ([state (consume-token state)])
                   (cond
                     ;; Function type means this is a function pointer variable
                     [(function-type? decl-type)
                      (values (make-field name decl-type) state)]
                     ;; Pointer to function type
                     [(and (pointer-type? decl-type)
                           (function-type? (pointer-type-pointee decl-type)))
                      (values (make-field name decl-type) state)]
                     ;; Array type
                     [(array-type? decl-type)
                      (values (make-field name decl-type) state)]
                     ;; Regular variable - skip for now
                     [else
                      (values #f state)]))]

                ;; Unexpected token
                [else
                 (parse-error tok "Expected '(' or ';' after declarator")])))))))

  ;; Parse parameter list
  (define (parse-parameter-list state)
    (let loop ([state state] [params '()])
      (let ([tok (peek-token state)])
        (cond
          ;; End of parameters
          [(and tok (punctuator? tok) (equal? (token-value tok) ")"))
           (values (reverse params) #f state)]

          ;; Variadic (...)
          [(and tok (punctuator? tok) (equal? (token-value tok) "..."))
           (values (reverse params) #t (consume-token state))]

          ;; void parameter list (only if void followed directly by ')')
          [(and (null? params) tok (keyword-token? tok) (eq? (token-value tok) 'void))
           (let* ([state (consume-token state)]
                  [next-tok (peek-token state)])
             (if (and next-tok (punctuator? next-tok) (equal? (token-value next-tok) ")"))
                 ;; It's an empty parameter list: foo(void)
                 (values '() #f state)
                 ;; It's a void* or similar - backtrack and parse as parameter
                 ;; We can't backtrack, so we need to manually handle this case
                 (let ([void-type (make-basic-type 'void)])
                   ;; Check if next is a pointer or identifier
                   (if (and next-tok (punctuator? next-tok) (equal? (token-value next-tok) "*"))
                       ;; Parse as void* parameter
                       (let-values ([(ptr-type state) (parse-declarator-type state void-type)])
                         (let ([tok (peek-token state)])
                           (if (identifier-token? tok)
                               ;; Named parameter
                               (let* ([name (token-value tok)]
                                      [state (consume-token state)]
                                      [tok2 (peek-token state)])
                                 ;; Skip optional comma
                                 (let ([state (if (and tok2 (punctuator? tok2) (equal? (token-value tok2) ","))
                                                  (consume-token state)
                                                  state)])
                                   (loop state (list (make-param name ptr-type)))))
                               ;; Unnamed parameter
                               (let ([tok2 (peek-token state)])
                                 ;; Skip optional comma
                                 (let ([state (if (and tok2 (punctuator? tok2) (equal? (token-value tok2) ","))
                                                  (consume-token state)
                                                  state)])
                                   (loop state (list (make-param #f ptr-type))))))))
                       ;; Just plain void - treat as empty parameter list
                       (values '() #f state)))))]

          ;; Parameter
          [else
           (let-values ([(param state) (parse-parameter state)])
             (let ([tok (peek-token state)])
               ;; Skip optional comma
               (let ([state (if (and tok (punctuator? tok) (equal? (token-value tok) ","))
                                (consume-token state)
                                state)])
                 (loop state (cons param params)))))]))))

  ;; Parse a single parameter
  (define (parse-parameter state)
    ;; Parse base type
    (let-values ([(quals state) (parse-type-qualifiers state)])
      (let-values ([(base-type state) (parse-type-specifier state)])
        (let ([qualified-base (if (null? quals)
                                  base-type
                                  (make-qualified-type quals base-type))])
          ;; Parse declarator (may have no name for abstract declarators)
          (let-values ([(name decl-type state) (parse-declarator state qualified-base)])
            (values (make-param name decl-type) state))))))

  ;;; Top-level API

  ;; Parse a list of declarations
  (define (parse-declarations tokens)
    (let loop ([state (make-parser-state tokens)] [decls '()])
      (if (at-eof? state)
          (reverse decls)
          (let-values ([(decl state) (parse-declaration state)])
            (if decl
                (loop state (cons decl decls))
                (loop state decls)))))))
