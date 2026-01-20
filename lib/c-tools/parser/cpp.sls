;; SPDX-License-Identifier: WTFPL
;; C++ Parser - Recursive Descent Parser for C++ Declarations
;; Parses preprocessed tokens into AST nodes
;; Uses closure pattern with effects for context tracking

(library (c-tools parser cpp)
  (export parse-cpp-declarations
          parse-cpp-declaration)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (rnrs lists)
          (rnrs records syntactic)
          (c-tools ast c)
          (c-tools ast cpp)
          (c-tools core conditions)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools effects cpp-lang class)
          (c-tools effects cpp-lang namespace)
          (c-tools effects cpp-lang templates)
          (c-tools lexer cpp)
          (only (chezscheme) format))

  ;; Parser entry point - creates parser context and runs
  (define (parse-cpp-declarations tokens)
    (with-cpp-namespace
      (lambda ()
        (with-cpp-class
          (lambda ()
            (with-cpp-templates
              (lambda ()
                (parse-tokens tokens))))))))

  ;; Internal parser using closure pattern
  (define (parse-tokens tokens)
    ;; Mutable state
    (define pos 0)
    (define token-list (list->vector tokens))
    (define token-count (vector-length token-list))
    (define pushback-stack '())  ;; Stack of synthetic tokens for >> splitting

    ;; Token operations
    (define (peek)
      (if (pair? pushback-stack)
          (car pushback-stack)  ;; Return pushed-back token
          (if (>= pos token-count)
              #f
              (vector-ref token-list pos))))

    (define (peek-ahead n)
      (if (> n 0)
          (if (pair? pushback-stack)
              ;; If we have pushback, need to account for it
              (if (= n 1)
                  (vector-ref token-list pos)  ;; Next real token
                  (let ([idx (+ pos (- n 1))])
                    (if (>= idx token-count)
                        #f
                        (vector-ref token-list idx))))
              (let ([idx (+ pos n)])
                (if (>= idx token-count)
                    #f
                    (vector-ref token-list idx))))
          (peek)))

    (define (advance!)
      (let ([tok (peek)])
        (when tok
          (if (pair? pushback-stack)
              (set! pushback-stack (cdr pushback-stack))  ;; Pop synthetic token
              (set! pos (+ pos 1))))  ;; Advance real position
        tok))

    (define (pushback! tok)
      ;; Push a synthetic token onto the stack
      (set! pushback-stack (cons tok pushback-stack)))

    (define (at-eof?)
      (let ([tok (peek)])
        (or (not tok) (eof-token? tok))))

    ;; Token predicates
    (define (is-keyword? tok kw)
      (and tok
           (keyword-token? tok)
           (eq? (token-value tok) kw)))

    (define (is-punct? tok p)
      (and tok
           (punctuator? tok)
           (equal? (token-value tok) p)))

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

    (define (maybe-punct p)
      (let ([tok (peek)])
        (if (is-punct? tok p)
            (begin (advance!) #t)
            #f)))

    (define (maybe-keyword kw)
      (let ([tok (peek)])
        (if (is-keyword? tok kw)
            (begin (advance!) #t)
            #f)))

    ;; Skip to next semicolon or closing brace (error recovery)
    (define (skip-to-sync!)
      (let loop ()
        (let ([tok (peek)])
          (cond
            [(not tok) #f]
            [(eof-token? tok) #f]
            [(is-punct? tok ";") (advance!) #f]
            [(is-punct? tok "}") #f]  ;; Don't consume, might be needed
            [else (advance!) (loop)]))))

    ;; Parse qualified name (foo::bar::baz)
    (define (parse-qualified-name)
      (define (parse-name-part)
        ;; Could be identifier or operator name
        (let ([tok (peek)])
          (cond
            [(is-identifier? tok)
             (let ([name (token-value (advance!))])
               ;; Check for template args
               (if (is-punct? (peek) "<")
                   (let ([args (parse-template-args)])
                     (make-template-type name args))
                   name))]
            [(is-keyword? tok 'operator)
             (parse-operator-name)]
            [else
             (parse-error "Expected identifier or operator")])))

      (let loop ([parts '()])
        (let ([part (parse-name-part)])
          (if (is-punct? (peek) "::")
              (begin
                (advance!)  ;; consume ::
                (loop (cons part parts)))
              ;; Done
              (if (null? parts)
                  part  ;; Simple name
                  (make-qualified-name (reverse parts) part))))))

    ;; Parse template argument list <...>
    (define (parse-template-args)
      (expect-punct "<")
      (if (is-punct? (peek) ">")
          (begin (advance!) '())  ;; Empty args
          (let loop ([args '()])
            (let ([arg (parse-template-arg)])
              (cond
                [(is-punct? (peek) ",")
                 (advance!)
                 (loop (cons arg args))]
                [(is-punct? (peek) ">")
                 (advance!)
                 (reverse (cons arg args))]
                [(is-punct? (peek) ">>")
                 ;; Handle >> as two closing brackets
                 ;; Consume the >>, push back a synthetic > for outer template
                 (let ([tok (advance!)])
                   (pushback! (make-token 'punctuator ">" (token-location tok)))
                   (reverse (cons arg args)))]
                [else
                 (parse-error "Expected ',' or '>' in template arguments")])))))

    ;; Parse single template argument (type or expression)
    (define (parse-template-arg)
      ;; For now, just parse types. Expressions are more complex.
      (parse-type))

    ;; Parse operator name (operator+, operator[], etc.)
    (define (parse-operator-name)
      (expect-keyword 'operator)
      (let ([tok (peek)])
        (cond
          ;; Conversion operator: operator int()
          [(or (is-keyword? tok 'int)
               (is-keyword? tok 'char)
               (is-keyword? tok 'float)
               (is-keyword? tok 'double)
               (is-keyword? tok 'bool)
               (is-identifier? tok))
           (let ([type (parse-type)])
             (string->symbol (format "operator ~a" type)))]
          ;; Regular operators
          [(punctuator? tok)
           (let ([op (token-value (advance!))])
             ;; Check for () and []
             (cond
               [(equal? op "(")
                (expect-punct ")")
                'operator-call]
               [(equal? op "[")
                (expect-punct "]")
                'operator-index]
               [else
                (string->symbol (format "operator~a" op))]))]
          [else
           (parse-error "Expected operator")])))

    ;; Parse type specifier
    (define (parse-type-specifier)
      (let ([tok (peek)])
        (cond
          [(not tok)
           (parse-error "Expected type specifier")]

          ;; const/volatile as leading qualifier
          [(or (is-keyword? tok 'const) (is-keyword? tok 'volatile))
           (let ([qual (token-value (advance!))])
             (let ([inner (parse-type-specifier)])
               (make-qualified-type (list qual) inner)))]

          ;; Basic types
          [(and (keyword-token? tok)
                (memq (token-value tok)
                      '(void char short int long float double
                        signed unsigned bool)))
           (parse-basic-type)]

          ;; struct/class/union
          [(is-keyword? tok 'struct)
           (advance!)
           (let ([name (parse-qualified-name)])
             (make-named-type 'struct name))]

          [(is-keyword? tok 'class)
           (advance!)
           (let ([name (parse-qualified-name)])
             (make-named-type 'class name))]

          [(is-keyword? tok 'union)
           (advance!)
           (let ([name (parse-qualified-name)])
             (make-named-type 'union name))]

          ;; enum
          [(is-keyword? tok 'enum)
           (advance!)
           (maybe-keyword 'class)  ;; enum class
           (let ([name (parse-qualified-name)])
             (make-named-type 'enum name))]

          ;; typename (in template context)
          [(is-keyword? tok 'typename)
           (advance!)
           (let ([name (parse-qualified-name)])
             (make-named-type 'typename name))]

          ;; decltype
          [(is-keyword? tok 'decltype)
           (advance!)
           (expect-punct "(")
           (skip-balanced-parens!)
           (make-named-type 'decltype 'auto)]

          ;; auto
          [(is-keyword? tok 'auto)
           (advance!)
           (make-basic-type 'auto)]

          ;; Identifier (typedef name or qualified name)
          [(is-identifier? tok)
           (let ([name (parse-qualified-name)])
             (make-named-type 'typedef name))]

          [else
           (parse-error "Expected type specifier")])))

    ;; Skip balanced parentheses
    (define (skip-balanced-parens!)
      (let loop ([depth 1])
        (let ([tok (advance!)])
          (cond
            [(not tok) (parse-error "Unexpected end of input")]
            [(is-punct? tok "(") (loop (+ depth 1))]
            [(is-punct? tok ")")
             (if (> depth 1)
                 (loop (- depth 1))
                 #f)]
            [else (loop depth)]))))

    ;; Parse basic type (handles multi-word types)
    (define (parse-basic-type)
      (let loop ([keywords '()])
        (let ([tok (peek)])
          (if (and (keyword-token? tok)
                   (memq (token-value tok)
                         '(char short int long float double
                           signed unsigned void bool)))
              (begin
                (advance!)
                (loop (cons (token-value tok) keywords)))
              ;; Normalize
              (make-basic-type (normalize-basic-type (reverse keywords)))))))

    ;; Normalize multi-word basic type
    (define (normalize-basic-type keywords)
      (cond
        [(null? keywords) 'int]
        [(equal? keywords '(void)) 'void]
        [(equal? keywords '(bool)) 'bool]
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

    ;; Parse complete type with pointers, references, const
    (define (parse-type)
      (let ([base (parse-type-specifier)])
        (parse-type-modifiers base)))

    ;; Parse type modifiers (*, &, &&, const, volatile)
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

          ;; Lvalue reference
          [(is-punct? tok "&")
           (advance!)
           (make-reference-type base)]

          ;; Rvalue reference (&&)
          [(is-punct? tok "&&")
           (advance!)
           ;; For FFI purposes, treat as regular reference
           (make-reference-type base)]

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

    ;; Parse declaration
    ;; Returns (cons specifiers decl) for function/variable, or (cons '() decl) for others
    (define (parse-declaration)
      (let ([tok (peek)])
        (cond
          [(not tok) #f]
          [(eof-token? tok) #f]

          ;; namespace
          [(is-keyword? tok 'namespace)
           (cons '() (parse-namespace))]

          ;; using declaration/directive
          [(is-keyword? tok 'using)
           (parse-using)
           #f]  ;; Don't emit using declarations in FFI

          ;; extern "C"
          [(is-keyword? tok 'extern)
           (cons '() (parse-extern))]

          ;; template
          [(is-keyword? tok 'template)
           (cons '() (parse-template))]

          ;; class/struct
          [(is-keyword? tok 'class)
           (cons '() (parse-class-definition 'class))]

          [(is-keyword? tok 'struct)
           ;; Could be struct definition or just struct type in declaration
           (if (is-struct-definition?)
               (cons '() (parse-class-definition 'struct))
               (parse-function-or-variable))]

          ;; enum
          [(is-keyword? tok 'enum)
           (if (is-enum-definition?)
               (cons '() (parse-enum-definition))
               (parse-function-or-variable))]

          ;; typedef
          [(is-keyword? tok 'typedef)
           (cons '() (parse-typedef))]

          ;; Access specifier (inside class)
          [(and (in-class?!)
                (memq (token-value tok) '(public private protected)))
           (let ([access (token-value (advance!))])
             (expect-punct ":")
             (set-access! access)
             #f)]

          ;; friend declaration (skip)
          [(is-keyword? tok 'friend)
           (skip-to-sync!)
           #f]

          ;; Function or variable
          [else
           (parse-function-or-variable)])))

    ;; Check if this is a struct/class definition
    (define (is-struct-definition?)
      (let ([tok1 (peek-ahead 1)])
        (and (is-identifier? tok1)
             (let ([tok2 (peek-ahead 2)])
               (or (is-punct? tok2 "{")
                   (is-punct? tok2 ":"))))))  ;; inheritance

    ;; Check if this is an enum definition
    (define (is-enum-definition?)
      (let ([i 1])
        ;; Skip 'class' if present
        (when (is-keyword? (peek-ahead i) 'class)
          (set! i (+ i 1)))
        (let ([tok1 (peek-ahead i)])
          (and (is-identifier? tok1)
               (let ([tok2 (peek-ahead (+ i 1))])
                 (is-punct? tok2 "{"))))))

    ;; Parse namespace declaration
    (define (parse-namespace)
      (expect-keyword 'namespace)
      (let ([name-tok (peek)])
        (cond
          ;; Anonymous namespace - skip
          [(is-punct? name-tok "{")
           (skip-braced-block!)
           #f]
          ;; Named namespace
          [(is-identifier? name-tok)
           (let ([name (token-value (advance!))])
             (expect-punct "{")
             (push-namespace! name)
             (let ([decls (parse-declaration-list)])
               (expect-punct "}")
               (pop-namespace!)
               (make-namespace-decl name decls)))]
          [else
           (parse-error "Expected namespace name or '{'")])))

    ;; Parse using declaration or directive
    (define (parse-using)
      (expect-keyword 'using)
      (cond
        ;; using namespace foo;
        [(is-keyword? (peek) 'namespace)
         (skip-to-sync!)]
        ;; using foo::bar; or using foo = bar;
        [else
         (skip-to-sync!)]))

    ;; Parse extern declaration
    (define (parse-extern)
      (expect-keyword 'extern)
      (let ([tok (peek)])
        (cond
          ;; extern "C" { ... }
          [(and (string-literal? tok)
                (equal? (token-value tok) "C"))
           (advance!)
           (if (is-punct? (peek) "{")
               (begin
                 (advance!)
                 (let ([decls (parse-declaration-list)])
                   (expect-punct "}")
                   ;; Return declarations as-is (they're already C-compatible)
                   decls))
               ;; Single declaration
               (parse-declaration))]
          ;; extern "C++"
          [(and (string-literal? tok)
                (equal? (token-value tok) "C++"))
           (advance!)
           (if (is-punct? (peek) "{")
               (begin
                 (advance!)
                 (let ([decls (parse-declaration-list)])
                   (expect-punct "}")
                   decls))
               (parse-declaration))]
          ;; extern declaration (linkage specifier)
          [else
           (parse-function-or-variable)])))

    ;; Parse template declaration
    (define (parse-template)
      (expect-keyword 'template)
      (expect-punct "<")
      (let ([params (parse-template-params)])
        (expect-punct ">")
        (push-template-params! params)
        (let ([decl (parse-declaration)])
          (pop-template-params!)
          (if decl
              (make-template-decl params decl)
              #f))))

    ;; Parse template parameters
    (define (parse-template-params)
      (if (is-punct? (peek) ">")
          '()  ;; Empty parameter list
          (let loop ([params '()])
            (let ([param (parse-template-param)])
              (cond
                [(is-punct? (peek) ",")
                 (advance!)
                 (loop (cons param params))]
                [(is-punct? (peek) ">")
                 (reverse (cons param params))]
                [else
                 (parse-error "Expected ',' or '>' in template parameters")])))))

    ;; Parse single template parameter
    (define (parse-template-param)
      (let ([tok (peek)])
        (cond
          ;; typename T or class T
          [(or (is-keyword? tok 'typename) (is-keyword? tok 'class))
           (let ([kind (token-value (advance!))])
             (if (is-identifier? (peek))
                 (let ([name (token-value (advance!))])
                   ;; Check for default
                   (when (is-punct? (peek) "=")
                     (advance!)
                     (parse-type))  ;; skip default
                   (cons kind name))
                 (cons kind #f)))]
          ;; Non-type parameter
          [else
           (let ([type (parse-type)])
             (let ([name (if (is-identifier? (peek))
                             (token-value (advance!))
                             #f)])
               ;; Check for default
               (when (is-punct? (peek) "=")
                 (advance!)
                 (skip-to-comma-or-close!))
               (cons 'value name)))])))

    ;; Skip to comma or closing angle bracket
    (define (skip-to-comma-or-close!)
      (let loop ([depth 0])
        (let ([tok (peek)])
          (cond
            [(not tok) #f]
            [(and (= depth 0)
                  (or (is-punct? tok ",") (is-punct? tok ">")))
             #f]
            [(is-punct? tok "<") (advance!) (loop (+ depth 1))]
            [(is-punct? tok ">") (advance!) (loop (- depth 1))]
            [(is-punct? tok "(") (advance!) (loop (+ depth 1))]
            [(is-punct? tok ")") (advance!) (loop (- depth 1))]
            [else (advance!) (loop depth)]))))

    ;; Parse class/struct definition
    (define (parse-class-definition kind)
      (expect-keyword kind)
      (let ([name (if (is-identifier? (peek))
                      (token-value (advance!))
                      #f)])
        ;; Register as template if we have template params
        (when (and name (in-template?!))
          (register-template-name! name))
        ;; Parse base classes
        (let ([bases (if (is-punct? (peek) ":")
                         (parse-base-classes)
                         '())])
          (expect-punct "{")
          (push-class! kind name)
          (let ([members (parse-member-list)])
            (expect-punct "}")
            (pop-class!)
            (maybe-punct ";")
            (make-class-decl kind name bases members)))))

    ;; Parse base class list
    (define (parse-base-classes)
      (expect-punct ":")
      (let loop ([bases '()])
        (let* ([access (cond
                         [(maybe-keyword 'public) 'public]
                         [(maybe-keyword 'protected) 'protected]
                         [(maybe-keyword 'private) 'private]
                         [else 'private])]  ;; Default for class
               [virtual? (maybe-keyword 'virtual)]
               [name (parse-qualified-name)])
          (let ([base (cons access name)])
            (if (is-punct? (peek) ",")
                (begin (advance!) (loop (cons base bases)))
                (reverse (cons base bases)))))))

    ;; Parse class member list
    (define (parse-member-list)
      (let loop ([members '()])
        (let ([tok (peek)])
          (cond
            [(is-punct? tok "}")
             (reverse members)]
            [(not tok)
             (reverse members)]
            [else
             (let ([result (parse-declaration)])
               (if result
                   (let ([specifiers (car result)]
                         [decl (cdr result)])
                     (if decl
                         (loop (cons (make-member-decl (current-access!)
                                                       (member-kind decl)
                                                       decl
                                                       specifiers)
                                     members))
                         (loop members)))
                   (loop members)))]))))

    ;; Determine member kind
    (define (member-kind decl)
      (cond
        [(function-decl? decl) 'method]
        [(field? decl) 'field]
        [else 'other]))

    ;; Parse enum definition
    (define (parse-enum-definition)
      (expect-keyword 'enum)
      (let ([scoped? (maybe-keyword 'class)])
        (let ([name (if (is-identifier? (peek))
                        (token-value (advance!))
                        #f)])
          ;; Optional underlying type
          (when (is-punct? (peek) ":")
            (advance!)
            (parse-type))  ;; skip it
          (expect-punct "{")
          (let ([enumerators (parse-enumerator-list)])
            (expect-punct "}")
            (maybe-punct ";")
            (make-enum-decl name enumerators)))))

    ;; Parse enumerator list
    (define (parse-enumerator-list)
      (if (is-punct? (peek) "}")
          '()
          (let loop ([enums '()] [value 0])
            (let ([name (token-value (expect-identifier))])
              (let ([val (if (is-punct? (peek) "=")
                             (begin
                               (advance!)
                               (parse-constant-expression))
                             value)])
                (let ([enum (make-enumerator name val)])
                  (if (is-punct? (peek) ",")
                      (begin (advance!)
                             (if (is-punct? (peek) "}")
                                 (reverse (cons enum enums))
                                 (loop (cons enum enums) (+ val 1))))
                      (reverse (cons enum enums)))))))))

    ;; Parse constant expression (simplified)
    (define (parse-constant-expression)
      (let ([tok (peek)])
        (cond
          [(number-token? tok)
           (let ([val (token-value (advance!))])
             (string->number val))]
          ;; Could be more complex expression - just return 0
          [else
           (skip-to-comma-or-brace!)
           0])))

    ;; Skip to comma or closing brace
    (define (skip-to-comma-or-brace!)
      (let loop ([depth 0])
        (let ([tok (peek)])
          (cond
            [(not tok) #f]
            [(and (= depth 0)
                  (or (is-punct? tok ",") (is-punct? tok "}")))
             #f]
            [(is-punct? tok "(") (advance!) (loop (+ depth 1))]
            [(is-punct? tok ")") (advance!) (loop (- depth 1))]
            [(is-punct? tok "{") (advance!) (loop (+ depth 1))]
            [(is-punct? tok "}") (advance!) (loop (- depth 1))]
            [else (advance!) (loop depth)]))))

    ;; Parse typedef
    (define (parse-typedef)
      (expect-keyword 'typedef)
      (let ([type (parse-type)])
        (let ([name (token-value (expect-identifier))])
          (expect-punct ";")
          (make-typedef name type))))

    ;; Parse function or variable declaration
    ;; Returns (cons specifiers decl) where specifiers is a list of symbols
    ;; like (static virtual const)
    (define (parse-function-or-variable)
      ;; Handle static, virtual, explicit, inline, constexpr
      (let loop ([specifiers '()])
        (let ([tok (peek)])
          (if (and (keyword-token? tok)
                   (memq (token-value tok)
                         '(static virtual explicit inline constexpr)))
              (begin (advance!) (loop (cons (token-value tok) specifiers)))
              (parse-function-or-variable-with-specifiers (reverse specifiers))))))

    (define (parse-function-or-variable-with-specifiers specifiers)
      ;; Check for destructor first
      (if (is-punct? (peek) "~")
          (begin
            (advance!)
            (let ([name (token-value (expect-identifier))])
              (expect-punct "(")
              (expect-punct ")")
              ;; Skip noexcept, = default, = delete, etc.
              (skip-function-suffix!)
              (cons specifiers
                    (make-function-decl
                      (string->symbol (format "~~~a" name))
                      (make-basic-type 'void)
                      '()
                      #f))))
          ;; Not a destructor - parse normally
          (let ([return-type (parse-type)])
        (let ([tok (peek)])
          (cond
            ;; Possibly constructor or conversion operator
            [(is-punct? tok "(")
             ;; This is a constructor if return-type is a named-type matching class name
             (if (and (in-class?!)
                      (named-type? return-type)
                      (let ([class-info (current-class!)])
                        (and class-info
                             (eq? (named-type-name return-type)
                                  (cdr class-info)))))
                 ;; Constructor
                 (begin
                   (advance!)  ;; consume (
                   (let-values ([(params variadic?) (parse-parameter-list)])
                     (expect-punct ")")
                     (skip-function-suffix!)
                     (cons specifiers
                           (make-function-decl
                             (named-type-name return-type)
                             (make-basic-type 'void)
                             params
                             variadic?))))
                 (parse-error "Expected identifier"))]

            ;; Normal function or variable
            [(is-identifier? tok)
             (let ([name (token-value (advance!))])
               (if (is-punct? (peek) "(")
                   ;; Function
                   (begin
                     (advance!)  ;; consume (
                     (let-values ([(params variadic?) (parse-parameter-list)])
                       (expect-punct ")")
                       ;; Parse const, noexcept, override, final, = 0, = default, = delete
                       (let ([const? (maybe-keyword 'const)])
                         (skip-function-suffix!)
                         (cons (if const? (cons 'const specifiers) specifiers)
                               (make-function-decl name return-type params variadic?)))))
                   ;; Variable
                   (begin
                     (skip-to-sync!)
                     (cons specifiers (make-field name return-type)))))]

            ;; Operator
            [(is-keyword? tok 'operator)
             (let ([op-name (parse-operator-name)])
               (expect-punct "(")
               (let-values ([(params variadic?) (parse-parameter-list)])
                 (expect-punct ")")
                 (let ([const? (maybe-keyword 'const)])
                   (skip-function-suffix!)
                   (cons (if const? (cons 'const specifiers) specifiers)
                         (make-function-decl op-name return-type params variadic?)))))]

            [else
             (skip-to-sync!)
             (cons specifiers #f)])))))

    ;; Skip function suffix (const, noexcept, override, = 0, etc.)
    (define (skip-function-suffix!)
      (let loop ()
        (let ([tok (peek)])
          (cond
            [(is-keyword? tok 'const) (advance!) (loop)]
            [(is-keyword? tok 'volatile) (advance!) (loop)]
            [(is-keyword? tok 'noexcept)
             (advance!)
             (when (is-punct? (peek) "(")
               (advance!)
               (skip-balanced-parens!))
             (loop)]
            [(is-keyword? tok 'override) (advance!) (loop)]
            [(is-keyword? tok 'final) (advance!) (loop)]
            [(is-punct? tok "->")
             ;; Trailing return type
             (advance!)
             (parse-type)
             (loop)]
            [(is-punct? tok "=")
             (advance!)
             (let ([tok2 (peek)])
               (cond
                 [(and (number-token? tok2) (equal? (token-value tok2) "0"))
                  (advance!)]  ;; pure virtual
                 [(is-keyword? tok2 'default) (advance!)]
                 [(is-keyword? tok2 'delete) (advance!)]
                 [else #f]))
             (loop)]
            ;; Check for function body or semicolon
            [(is-punct? tok "{")
             (skip-braced-block!)]
            [(is-punct? tok ";")
             (advance!)]
            [else #f]))))

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
                ;; void parameter
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
        (let ([tok (peek)])
          (cond
            [(is-identifier? tok)
             (let ([name (token-value (advance!))])
               ;; Check for array syntax
               (when (is-punct? (peek) "[")
                 (skip-array-dimensions!))
               ;; Check for default value
               (when (is-punct? (peek) "=")
                 (advance!)
                 (skip-to-comma-or-paren!))
               (make-param name type))]
            [else
             (make-param #f type)]))))

    ;; Skip array dimensions
    (define (skip-array-dimensions!)
      (while (is-punct? (peek) "[")
        (advance!)
        (let loop ([depth 1])
          (let ([tok (advance!)])
            (cond
              [(is-punct? tok "[") (loop (+ depth 1))]
              [(is-punct? tok "]")
               (when (> depth 1)
                 (loop (- depth 1)))]
              [else (loop depth)])))))

    ;; Skip to comma or closing paren
    (define (skip-to-comma-or-paren!)
      (let loop ([depth 0])
        (let ([tok (peek)])
          (cond
            [(not tok) #f]
            [(and (= depth 0)
                  (or (is-punct? tok ",") (is-punct? tok ")")))
             #f]
            [(is-punct? tok "(") (advance!) (loop (+ depth 1))]
            [(is-punct? tok ")") (advance!) (loop (- depth 1))]
            [(is-punct? tok "{") (advance!) (loop (+ depth 1))]
            [(is-punct? tok "}") (advance!) (loop (- depth 1))]
            [(is-punct? tok "<") (advance!) (loop (+ depth 1))]
            [(is-punct? tok ">") (advance!) (loop (- depth 1))]
            [else (advance!) (loop depth)]))))

    ;; Skip braced block
    (define (skip-braced-block!)
      (expect-punct "{")
      (let loop ([depth 1])
        (let ([tok (advance!)])
          (cond
            [(not tok) (parse-error "Unexpected end of input")]
            [(is-punct? tok "{") (loop (+ depth 1))]
            [(is-punct? tok "}")
             (when (> depth 1)
               (loop (- depth 1)))]
            [else (loop depth)]))))

    ;; Parse declaration list
    (define (parse-declaration-list)
      (let loop ([decls '()])
        (let ([tok (peek)])
          (cond
            [(is-punct? tok "}") (reverse decls)]
            [(not tok) (reverse decls)]
            [(eof-token? tok) (reverse decls)]
            [else
             (let ([result (parse-declaration)])
               (if result
                   (let ([decl (cdr result)])  ;; extract decl from (specifiers . decl)
                     (if decl
                         (if (list? decl)
                             (loop (append (reverse decl) decls))
                             (loop (cons decl decls)))
                         (loop decls)))
                   (loop decls)))]))))

    ;; while macro for convenience
    (define-syntax while
      (syntax-rules ()
        [(_ test body ...)
         (let loop ()
           (when test
             body ...
             (loop)))]))

    ;; return macro (uses continuation)
    (define-syntax return
      (syntax-rules ()
        [(_ expr)
         expr]))  ;; Just return the expression

    ;; Main parse loop
    (let loop ([decls '()])
      (if (at-eof?)
          (reverse decls)
          (let ([result (parse-declaration)])
            (if result
                (let ([decl (cdr result)])  ;; extract decl from (specifiers . decl)
                  (if decl
                      (if (list? decl)
                          (loop (append (reverse decl) decls))
                          (loop (cons decl decls)))
                      (loop decls)))
                (loop decls))))))

  ;; Single declaration entry point
  (define (parse-cpp-declaration tokens)
    (let ([decls (parse-cpp-declarations tokens)])
      (if (null? decls)
          #f
          (car decls)))))
