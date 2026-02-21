;; SPDX-License-Identifier: WTFPL
;; C Preprocessor Conditional Compilation Handler
;; Handles #if/#ifdef/#ifndef/#elif
;;
;;=======================================================================
;; Effect handler composition example
;;
;; This handler demonstrates proper use of the `loop` parameter.
;;
;; The conditional handler needs to call symbol-defined?, which is itself
;; an effect (cpp-symbol). To ensure the cpp-symbol effect is properly
;; handled by the symbol handler, we must wrap the call in `loop`:
;;
;;   (loop (lambda () (k (symbol-defined? condition))))
;;
;; Without `loop`, the symbol-defined? effect would escape past the symbol
;; handler and cause an unhandled effect error.
;;
;; Handler nesting for conditionals:
;;   with-cpp-symbols        (outer - tracks defined symbols)
;;     with-cpp-conditional  (middle - evaluates #if/#ifdef)
;;       with-cpp-macros     (inner - stores macro definitions)

(library (c-tools effects cpp conditionals)
  (export with-cpp-conditional
          register-cpp-conditional!
          eval-const-expr)
  (import (rnrs base)
          (rnrs arithmetic bitwise)
          (rnrs lists)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools effects cpp core)
          (c-tools effects registry))

  ;; Ensure value is a number (convert #f to 0)
  (define (ensure-number val)
    (if (number? val) val 0))

  ;; Simple constant expression evaluator for #if directives
  (define (eval-const-expr tokens)
    ;;   Evaluate a constant preprocessor expression, returns integer
    (if (null? tokens)
        0
        (let-values ([(result remaining) (parse-logical-or tokens)])
          (ensure-number result))))

  ;;=======================================================================
  ;; Recursive descent expression parser with proper precedence
  ;;
  ;; C preprocessor operator precedence (lowest to highest):
  ;;   ||
  ;;   &&
  ;;   |
  ;;   ^
  ;;   &
  ;;   == !=
  ;;   < > <= >=
  ;;   << >>
  ;;   + -
  ;;   * / %
  ;;   unary: ! ~ - +
  ;;   defined(...)

  ;; Token to integer conversion
  (define (token->int tok)
    (cond
      [(number-token? tok)
       (let ([val (token-value tok)])
         (if (string? val)
             (string->number val)
             0))]
      [(identifier-token? tok) 0]  ;; undefined identifiers are 0
      [else 0]))

  ;; Logical OR: expr || expr
  (define (parse-logical-or tokens)
    (let-values ([(left rest) (parse-logical-and tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest))
                 (equal? (token-value (car rest)) "||"))
            (let-values ([(right rest2) (parse-logical-and (cdr rest))])
              (loop (if (or (not (zero? left)) (not (zero? right))) 1 0) rest2))
            (values left rest)))))

  ;; Logical AND: expr && expr
  (define (parse-logical-and tokens)
    (let-values ([(left rest) (parse-bitwise-or tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest))
                 (equal? (token-value (car rest)) "&&"))
            (let-values ([(right rest2) (parse-bitwise-or (cdr rest))])
              (loop (if (and (not (zero? left)) (not (zero? right))) 1 0) rest2))
            (values left rest)))))

  ;; Bitwise OR: expr | expr
  (define (parse-bitwise-or tokens)
    (let-values ([(left rest) (parse-bitwise-xor tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest))
                 (equal? (token-value (car rest)) "|")
                 ;; Make sure it's not ||
                 (not (and (pair? (cdr rest))
                           (punctuator? (cadr rest))
                           (equal? (token-value (cadr rest)) "|"))))
            (let-values ([(right rest2) (parse-bitwise-xor (cdr rest))])
              (loop (bitwise-ior left right) rest2))
            (values left rest)))))

  ;; Bitwise XOR: expr ^ expr
  (define (parse-bitwise-xor tokens)
    (let-values ([(left rest) (parse-bitwise-and tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest))
                 (equal? (token-value (car rest)) "^"))
            (let-values ([(right rest2) (parse-bitwise-and (cdr rest))])
              (loop (bitwise-xor left right) rest2))
            (values left rest)))))

  ;; Bitwise AND: expr & expr
  (define (parse-bitwise-and tokens)
    (let-values ([(left rest) (parse-equality tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest))
                 (equal? (token-value (car rest)) "&")
                 ;; Make sure it's not &&
                 (not (and (pair? (cdr rest))
                           (punctuator? (cadr rest))
                           (equal? (token-value (cadr rest)) "&"))))
            (let-values ([(right rest2) (parse-equality (cdr rest))])
              (loop (bitwise-and left right) rest2))
            (values left rest)))))

  ;; Equality: expr == expr, expr != expr
  (define (parse-equality tokens)
    (let-values ([(left rest) (parse-relational tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest)))
            (let ([op (token-value (car rest))])
              (cond
                [(equal? op "==")
                 (let-values ([(right rest2) (parse-relational (cdr rest))])
                   (loop (if (= (ensure-number left) (ensure-number right)) 1 0) rest2))]
                [(equal? op "!=")
                 (let-values ([(right rest2) (parse-relational (cdr rest))])
                   (loop (if (= (ensure-number left) (ensure-number right)) 0 1) rest2))]
                [else (values left rest)]))
            (values left rest)))))

  ;; Relational: expr < expr, expr > expr, expr <= expr, expr >= expr
  (define (parse-relational tokens)
    (let-values ([(left rest) (parse-shift tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest)))
            (let ([op (token-value (car rest))])
              (cond
                [(equal? op "<")
                 (let-values ([(right rest2) (parse-shift (cdr rest))])
                   (loop (if (< (ensure-number left) (ensure-number right)) 1 0) rest2))]
                [(equal? op ">")
                 (let-values ([(right rest2) (parse-shift (cdr rest))])
                   (loop (if (> (ensure-number left) (ensure-number right)) 1 0) rest2))]
                [(equal? op "<=")
                 (let-values ([(right rest2) (parse-shift (cdr rest))])
                   (loop (if (<= (ensure-number left) (ensure-number right)) 1 0) rest2))]
                [(equal? op ">=")
                 (let-values ([(right rest2) (parse-shift (cdr rest))])
                   (loop (if (>= (ensure-number left) (ensure-number right)) 1 0) rest2))]
                [else (values left rest)]))
            (values left rest)))))

  ;; Shift: expr << expr, expr >> expr
  (define (parse-shift tokens)
    (let-values ([(left rest) (parse-additive tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest)))
            (let ([op (token-value (car rest))])
              (cond
                [(equal? op "<<")
                 (let-values ([(right rest2) (parse-additive (cdr rest))])
                   (loop (bitwise-arithmetic-shift-left left right) rest2))]
                [(equal? op ">>")
                 (let-values ([(right rest2) (parse-additive (cdr rest))])
                   (loop (bitwise-arithmetic-shift-right left right) rest2))]
                [else (values left rest)]))
            (values left rest)))))

  ;; Additive: expr + expr, expr - expr
  (define (parse-additive tokens)
    (let-values ([(left rest) (parse-multiplicative tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest)))
            (let ([op (token-value (car rest))])
              (cond
                [(equal? op "+")
                 (let-values ([(right rest2) (parse-multiplicative (cdr rest))])
                   (loop (+ left right) rest2))]
                [(equal? op "-")
                 (let-values ([(right rest2) (parse-multiplicative (cdr rest))])
                   (loop (- left right) rest2))]
                [else (values left rest)]))
            (values left rest)))))

  ;; Multiplicative: expr * expr, expr / expr, expr % expr
  (define (parse-multiplicative tokens)
    (let-values ([(left rest) (parse-unary tokens)])
      (let loop ([left left] [rest rest])
        (if (and (pair? rest)
                 (punctuator? (car rest)))
            (let ([op (token-value (car rest))])
              (cond
                [(equal? op "*")
                 (let-values ([(right rest2) (parse-unary (cdr rest))])
                   (loop (* left right) rest2))]
                [(equal? op "/")
                 (let-values ([(right rest2) (parse-unary (cdr rest))])
                   (loop (if (zero? right) 0 (div left right)) rest2))]
                [(equal? op "%")
                 (let-values ([(right rest2) (parse-unary (cdr rest))])
                   (loop (if (zero? right) 0 (mod left right)) rest2))]
                [else (values left rest)]))
            (values left rest)))))

  ;; Unary: !expr, ~expr, -expr, +expr, defined(...)
  (define (parse-unary tokens)
    (if (null? tokens)
        (values 0 '())
        (let ([tok (car tokens)])
          (cond
            ;; defined(X)
            [(and (identifier-token? tok)
                  (eq? (token-value tok) 'defined)
                  (pair? (cdr tokens))
                  (punctuator? (cadr tokens))
                  (equal? (token-value (cadr tokens)) "("))
             (let ([rest (cddr tokens)])
               (if (and (pair? rest)
                        (identifier-token? (car rest)))
                   (let ([name (token-value (car rest))]
                         [rest2 (cdr rest)])
                     ;; Skip closing )
                     (if (and (pair? rest2)
                              (punctuator? (car rest2))
                              (equal? (token-value (car rest2)) ")"))
                         (values (if (symbol-defined? name) 1 0) (cdr rest2))
                         (values (if (symbol-defined? name) 1 0) rest2)))
                   (values 0 rest)))]

            ;; Unary !
            [(and (punctuator? tok)
                  (equal? (token-value tok) "!"))
             (let-values ([(val rest) (parse-unary (cdr tokens))])
               (values (if (zero? val) 1 0) rest))]

            ;; Unary ~
            [(and (punctuator? tok)
                  (equal? (token-value tok) "~"))
             (let-values ([(val rest) (parse-unary (cdr tokens))])
               (values (bitwise-not val) rest))]

            ;; Unary -
            [(and (punctuator? tok)
                  (equal? (token-value tok) "-"))
             (let-values ([(val rest) (parse-unary (cdr tokens))])
               (values (- val) rest))]

            ;; Unary +
            [(and (punctuator? tok)
                  (equal? (token-value tok) "+"))
             (let-values ([(val rest) (parse-unary (cdr tokens))])
               (values val rest))]

            ;; Primary expression
            [else (parse-primary tokens)]))))

  ;; Primary: number, identifier, (expr)
  (define (parse-primary tokens)
    (if (null? tokens)
        (values 0 '())
        (let ([tok (car tokens)])
          (cond
            ;; Number
            [(number-token? tok)
             (values (token->int tok) (cdr tokens))]

            ;; Identifier (undefined = 0)
            [(identifier-token? tok)
             (values 0 (cdr tokens))]

            ;; Parenthesized expression
            [(and (punctuator? tok)
                  (equal? (token-value tok) "("))
             (let-values ([(val rest) (parse-logical-or (cdr tokens))])
               ;; Skip closing )
               (if (and (pair? rest)
                        (punctuator? (car rest))
                        (equal? (token-value (car rest)) ")"))
                   (values val (cdr rest))
                   (values val rest)))]

            ;; Unknown - treat as 0
            [else (values 0 (cdr tokens))]))))

  ;; Conditional compilation handler
  (define (with-cpp-conditional thunk)
    (with-handler 'cpp-conditional
      (lambda (data k loop)
        (let ([kind (car data)]
              [condition (cdr data)])
          (case kind
            [(ifdef)
             ;; Use loop to re-enter context so symbol-defined? effect is handled
             (loop (lambda () (k (symbol-defined? condition))))]
            [(ifndef)
             (loop (lambda () (k (not (symbol-defined? condition)))))]
            [(if elif)
             ;; eval-const-expr calls symbol-defined?, so use loop
             (loop (lambda ()
               (let ([result (eval-const-expr condition)])
                 (k result))))]
            [else
             (k #f)])))
      (thunk)))

  ;;=========================================================================
  ;; Registration

  (define (register-cpp-conditional!)
    (register-effect! 'cpp-conditional
      (lambda (spec thunk)
        (with-cpp-conditional thunk))))

  ;; Auto-register on load
  (register-cpp-conditional!))
