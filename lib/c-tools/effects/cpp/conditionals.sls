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
          eval-const-expr)
  (import (rnrs base)
          (rnrs lists)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools effects cpp core)
          (c-tools effects registry))

  ;; Simple constant expression evaluator for #if directives
  (define (eval-const-expr tokens)
    ;;   Evaluate a constant preprocessor expression, returns integer
    (if (null? tokens)
        0
        (eval-expr tokens)))

  ;; Token to integer conversion
  (define (token->int tok)
    (cond
      [(number-token? tok)
       (let ([val (token-value tok)])
         (if (string? val)
             (string->number val)
             0))]
      [(identifier-token? tok) 0]
      [else 0]))

  ;; Apply binary operator
  (define (apply-op op left right)
    (cond
      [(equal? op "+") (+ left right)]
      [(equal? op "-") (- left right)]
      [(equal? op "*") (* left right)]
      [(equal? op "/") (if (zero? right) 0 (div left right))]
      [(equal? op "%") (if (zero? right) 0 (mod left right))]
      [(equal? op "==") (if (= left right) 1 0)]
      [(equal? op "!=") (if (= left right) 0 1)]
      [(equal? op "<") (if (< left right) 1 0)]
      [(equal? op ">") (if (> left right) 1 0)]
      [(equal? op "<=") (if (<= left right) 1 0)]
      [(equal? op ">=") (if (>= left right) 1 0)]
      [(equal? op "&&") (if (and (not (zero? left)) (not (zero? right))) 1 0)]
      [(equal? op "||") (if (or (not (zero? left)) (not (zero? right))) 1 0)]
      [else 0]))

  ;; Expression evaluator (simplified)
  (define (eval-expr tokens)
    (cond
      ;; Empty
      [(null? tokens) 0]

      ;; Single number token
      [(and (= (length tokens) 1) (number-token? (car tokens)))
       (token->int (car tokens))]

      ;; defined(X) operator
      [(and (>= (length tokens) 4)
            (identifier-token? (car tokens))
            (eq? (token-value (car tokens)) 'defined)
            (punctuator? (cadr tokens))
            (equal? (token-value (cadr tokens)) "(")
            (identifier-token? (caddr tokens)))
       (if (symbol-defined? (token-value (caddr tokens))) 1 0)]

      ;; Binary operators - simple left-to-right evaluation
      [else
       (let loop ([toks tokens] [left 0] [op #f] [right-tokens '()])
         (cond
           [(null? toks)
            (if op
                (apply-op op left (eval-expr (reverse right-tokens)))
                left)]

           [(number-token? (car toks))
            (if op
                (loop (cdr toks) left op (cons (car toks) right-tokens))
                (loop (cdr toks) (token->int (car toks)) op right-tokens))]

           [(punctuator? (car toks))
            (let ([op-str (token-value (car toks))])
              (if (member op-str '("+" "-" "*" "/" "%" "==" "!=" "<" ">" "<=" ">=" "&&" "||"))
                  (if op
                      (loop toks (apply-op op left (eval-expr (reverse right-tokens))) #f '())
                      (loop (cdr toks) left op-str right-tokens))
                  (loop (cdr toks) left op right-tokens)))]

           [else
            (loop (cdr toks) left op right-tokens)]))]))

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
                 (k (not (zero? result))))))]
            [else
             (k #f)])))
      (thunk)))

  ;;=========================================================================
  ;; Registration

  (register-effect! 'cpp-conditional
    (lambda (spec thunk)
      (with-cpp-conditional thunk)))
)
