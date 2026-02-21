;; SPDX-License-Identifier: WTFPL
;; C Preprocessor - Main Entry Point
;; Integrates tokenizer, directive processing, and macro expansion

(library (c-tools preprocess c)
  (export preprocess-string
          preprocess-file
          preprocess-tokens)
  (import (rnrs base)
          (rnrs bytevectors)
          (rnrs control)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs lists)
          (c-tools core conditions)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools effects cpp conditionals)
          (c-tools effects cpp core)
          (c-tools effects cpp diagnostics)
          (c-tools effects cpp includes)
          (c-tools effects cpp location)
          (c-tools effects cpp macros)
          (c-tools effects cpp symbols)
          (c-tools effects files)
          (c-tools lexer c)
          (only (c-tools utility) format last-pair))

  ;; Conditional stack entry: (taken? . active?)
  ;;   taken? - has a true branch been executed in this if-elif-else chain?
  ;;   active? - are we currently executing this branch?

  ;; Check if we should skip based on conditional stack
  (define (should-skip? cond-stack)
    (and (pair? cond-stack)
         (not (cdr (car cond-stack)))))  ;; skip if top level is not active

  ;; Process a list of tokens, handling directives and expanding macros
  (define (preprocess-tokens tokens)
    ;;   Process a token stream, handling all preprocessor directives
    (let loop ([toks tokens]
               [output '()]
               [cond-stack '()])  ;; stack of (taken? . active?) pairs

      (if (null? toks)
          (reverse output)

          (let ([tok (car toks)])
            (cond
              ;; Preprocessor directive
              [(preprocessor-directive? tok)
               (let* ([directive-line (location-line (token-location tok))]
                      [result (collect-directive-line (cdr toks) directive-line)]
                      [directive-tokens (car result)]
                      [remaining-tokens (cdr result)])
                 (process-directive directive-tokens cond-stack
                   (lambda (new-cond-stack prepend-tokens)
                     (loop (append prepend-tokens remaining-tokens)
                           output
                           new-cond-stack))))]

              ;; Skip tokens inside false conditional
              [(should-skip? cond-stack)
               (loop (cdr toks) output cond-stack)]

              ;; Identifier - might be a macro
              [(identifier-token? tok)
               ;; Check if followed by '(' for function-like macro
               (let* ([rest (cdr toks)]
                      [has-lparen? (and (pair? rest)
                                        (punctuator? (car rest))
                                        (equal? (token-value (car rest)) "("))])
                 (if has-lparen?
                     ;; Try function-like macro expansion
                     (let-values ([(args remaining) (collect-macro-args (cdr rest))])
                       (let ([expanded (expand-macro! (token-value tok) args)])
                         (cond
                           ;; Undefined or painted - keep identifier and args as-is
                            [(or (eq? expanded 'undefined) (eq? expanded 'painted))
                             ;; Re-emit identifier, '(', args with commas, ')' and continue
                             ;; Put arg tokens back into stream for continued macro expansion
                             (let* ([lparen (car rest)]
                                    [rparen-loc (if (pair? args)
                                                    (token-location (car (last-pair (car (last-pair args)))))
                                                    (token-location lparen))]
                                    [rparen (make-token 'punctuator ")" rparen-loc)]
                                    [arg-tokens (flatten-args-with-commas-for-output args)]
                                    ;; Put args back in stream for macro expansion, not output
                                    [stream-tokens (append arg-tokens (list rparen) remaining)])
                               (loop stream-tokens (cons lparen (cons tok output)) cond-stack))]
                           ;; Expanded - insert expanded tokens
                           [else
                            (loop (append expanded remaining) output cond-stack)])))
                     ;; No '(' - try object-like macro
                     (let ([expanded (expand-macro! (token-value tok) #f)])
                       (cond
                         ;; Undefined or painted - keep as-is
                         [(or (eq? expanded 'undefined) (eq? expanded 'painted))
                          (loop (cdr toks) (cons tok output) cond-stack)]
                         ;; Expanded - insert expanded tokens
                         [else
                          (loop (append expanded (cdr toks)) output cond-stack)]))))]

              ;; EOF - done
              [(eof-token? tok)
               (reverse output)]

              ;; Comment - skip
              [(comment-token? tok)
               (loop (cdr toks) output cond-stack)]

              ;; Other tokens - pass through
              [else
               (loop (cdr toks) (cons tok output) cond-stack)])))))

  ;; Collect tokens until end of line (for directive processing)
  ;; Returns (collected-tokens . remaining-tokens)
  (define (collect-directive-line tokens directive-line)
    ;;   Collect tokens from the same line as the directive
    (let loop ([toks tokens] [collected '()])
      (if (null? toks)
          (cons (reverse collected) '())
          (let ([tok (car toks)])
            (cond
              ;; EOF - return what we have
              [(eof-token? tok)
               (cons (reverse collected) toks)]
              ;; Different line - this token starts the next line
              [(not (= (location-line (token-location tok)) directive-line))
               (cons (reverse collected) toks)]
              ;; Same line - collect it
              [else
               (loop (cdr toks) (cons tok collected))])))))

  ;; Process a preprocessor directive
  (define (process-directive tokens cond-stack continue)
    ;;   Process a directive and call CONTINUE with new cond-stack
    (if (null? tokens)
        (continue cond-stack '())

        (let ([first (car tokens)])
          (cond
            ;; #define
            [(and (identifier-token? first)
                  (eq? (token-value first) 'define))
             (if (should-skip? cond-stack)
                 (continue cond-stack '())
                 (process-define (cdr tokens) cond-stack continue))]

            ;; #undef
            [(and (identifier-token? first)
                  (eq? (token-value first) 'undef))
             (if (should-skip? cond-stack)
                 (continue cond-stack '())
                 (process-undef (cdr tokens) cond-stack continue))]

            ;; #ifdef
            [(and (identifier-token? first)
                  (eq? (token-value first) 'ifdef))
             (process-ifdef (cdr tokens) cond-stack continue)]

            ;; #ifndef
            [(and (identifier-token? first)
                  (eq? (token-value first) 'ifndef))
             (process-ifndef (cdr tokens) cond-stack continue)]

            ;; #if
            [(and (or (identifier-token? first) (keyword-token? first))
                  (eq? (token-value first) 'if))
             (process-if (cdr tokens) cond-stack continue)]

            ;; #elif
            [(and (or (identifier-token? first) (keyword-token? first))
                  (eq? (token-value first) 'elif))
             (process-elif (cdr tokens) cond-stack continue)]

            ;; #else
            [(and (or (identifier-token? first) (keyword-token? first))
                  (eq? (token-value first) 'else))
             (process-else (cdr tokens) cond-stack continue)]

            ;; #endif
            [(and (or (identifier-token? first) (keyword-token? first))
                  (eq? (token-value first) 'endif))
             (process-endif (cdr tokens) cond-stack continue)]

            ;; #include
            [(and (identifier-token? first)
                  (eq? (token-value first) 'include))
             (if (should-skip? cond-stack)
                 (continue cond-stack '())
                 (process-include (cdr tokens) cond-stack continue))]

            ;; Unknown directive - ignore
            [else
             (continue cond-stack '())]))))

  ;; Process #define directive
  (define (process-define tokens cond-stack continue)
    (if (null? tokens)
        (continue cond-stack '())
        (let ([name-tok (car tokens)])
          (if (identifier-token? name-tok)
              (let ([name (token-value name-tok)]
                    [rest (cdr tokens)])
                ;; Check if function-like macro (has '(' immediately after name)
                (if (and (pair? rest)
                        (punctuator? (car rest))
                        (equal? (token-value (car rest)) "("))
                    ;; Function-like macro - parse parameters
                    (let-values ([(params body-start) (parse-macro-params (cdr rest))])
                      (define-macro! name params body-start)
                      (continue cond-stack '()))
                    ;; Object-like macro
                    (begin
                      (define-macro! name #f rest)
                      (continue cond-stack '()))))
              (continue cond-stack '())))))

  ;; parse-macro-params : (list token) => (values (list symbol) (list token))
  ;;   Parse (a, b, c) or (a, b, ...) and return (values params remaining-tokens)
  ;;   For variadic macros, the last param will be '__VA_ARGS__'
  (define (parse-macro-params tokens)
    (let loop ([toks tokens] [params '()])
      (if (null? toks)
          (values (reverse params) '())
          (let ([tok (car toks)])
            (cond
              [(and (punctuator? tok) (equal? (token-value tok) ")"))
               (values (reverse params) (cdr toks))]
              ;; Variadic: ...
              [(and (punctuator? tok) (equal? (token-value tok) "..."))
               (loop (cdr toks) (cons '__VA_ARGS__ params))]
              [(identifier-token? tok)
               (loop (cdr toks) (cons (token-value tok) params))]
              [else
               (loop (cdr toks) params)])))))

  ;; Collect macro invocation arguments: (arg1, arg2, ...)
  ;; Returns (values list-of-arg-token-lists remaining-tokens)
  ;; Each arg is a list of tokens; handles nested parens and commas
  (define (collect-macro-args tokens)
    ;;   Collect arguments from a function-like macro invocation
    (let loop ([toks tokens]
               [current-arg '()]    ;; tokens for current argument
               [args '()]           ;; collected arguments (reversed)
               [paren-depth 0])     ;; track nested parens
      (if (null? toks)
          ;; Unexpected end - return what we have
          (values (reverse (if (null? current-arg)
                               args
                               (cons (reverse current-arg) args)))
                  '())
          (let ([tok (car toks)])
            (cond
              ;; Closing paren at depth 0 - done
              [(and (punctuator? tok)
                    (equal? (token-value tok) ")")
                    (= paren-depth 0))
               (values (reverse (if (null? current-arg)
                                    args
                                    (cons (reverse current-arg) args)))
                       (cdr toks))]

              ;; Opening paren - increase depth, add to current arg
              [(and (punctuator? tok) (equal? (token-value tok) "("))
               (loop (cdr toks)
                     (cons tok current-arg)
                     args
                     (+ paren-depth 1))]

              ;; Closing paren at depth > 0 - decrease depth, add to current arg
              [(and (punctuator? tok)
                    (equal? (token-value tok) ")")
                    (> paren-depth 0))
               (loop (cdr toks)
                     (cons tok current-arg)
                     args
                     (- paren-depth 1))]

              ;; Comma at depth 0 - end current arg, start new one
              [(and (punctuator? tok)
                    (equal? (token-value tok) ",")
                    (= paren-depth 0))
               (loop (cdr toks)
                     '()
                     (cons (reverse current-arg) args)
                     0)]

              ;; Any other token - add to current arg
              [else
               (loop (cdr toks)
                     (cons tok current-arg)
                     args
                     paren-depth)])))))

  ;; Flatten args back into token list with commas for output
  (define (flatten-args-with-commas-for-output args)
    (if (null? args)
        '()
        (let loop ([remaining args] [result '()])
          (cond
            [(null? remaining) (reverse result)]
            [(null? (cdr remaining))
             ;; Last arg - just append
             (loop '() (append (reverse (car remaining)) result))]
            [else
             ;; Not last - append with comma
             (let* ([arg-tokens (car remaining)]
                    [loc (if (pair? arg-tokens)
                             (token-location (car arg-tokens))
                             (macro-expansion-location))]
                    [comma (make-token 'punctuator "," loc)])
               (loop (cdr remaining)
                     (cons comma (append (reverse arg-tokens) result))))]))))

  ;; Process #undef directive
  (define (process-undef tokens cond-stack continue)
    (if (null? tokens)
        (continue cond-stack '())
        (let ([name-tok (car tokens)])
          (if (identifier-token? name-tok)
              (begin
                (undefine-macro! (token-value name-tok))
                (continue cond-stack '()))
              (continue cond-stack '())))))

  ;; Process #ifdef directive
  (define (process-ifdef tokens cond-stack continue)
    ;;   Push new conditional level based on whether symbol is defined
    (if (null? tokens)
        ;; No condition - push inactive level
        (continue (cons (cons #f #f) cond-stack) '())
        (let ([name-tok (car tokens)])
          (if (identifier-token? name-tok)
              (let* ([parent-active? (or (null? cond-stack)
                                         (cdr (car cond-stack)))]
                     [defined? (if parent-active?
                                   (eval-conditional! 'ifdef (token-value name-tok))
                                   #f)]
                     [active? (and parent-active? defined?)])
                (continue (cons (cons defined? active?) cond-stack) '()))
              ;; Invalid condition - push inactive level
              (continue (cons (cons #f #f) cond-stack) '())))))

  ;; Process #ifndef directive
  (define (process-ifndef tokens cond-stack continue)
    ;;   Push new conditional level based on whether symbol is not defined
    (if (null? tokens)
        ;; No condition - push inactive level
        (continue (cons (cons #f #f) cond-stack) '())
        (let ([name-tok (car tokens)])
          (if (identifier-token? name-tok)
              (let* ([parent-active? (or (null? cond-stack)
                                         (cdr (car cond-stack)))]
                     [not-defined? (if parent-active?
                                       (eval-conditional! 'ifndef (token-value name-tok))
                                       #f)]
                     [active? (and parent-active? not-defined?)])
                (continue (cons (cons not-defined? active?) cond-stack) '()))
              ;; Invalid condition - push inactive level
              (continue (cons (cons #f #f) cond-stack) '())))))

  ;; Process #if directive
  (define (process-if tokens cond-stack continue)
    ;;   Push new conditional level based on constant expression
    (let* ([parent-active? (or (null? cond-stack)
                               (cdr (car cond-stack)))]
           [result (if parent-active?
                       (eval-conditional! 'if tokens)
                       0)]
           [taken? (and (number? result) (not (zero? result)))]
           [active? (and parent-active? taken?)])
      ;;(display (format "#if: result=~a taken?=~a active?=~a\n" result taken? active?))
      (continue (cons (cons taken? active?) cond-stack) '())))

  ;; Process #elif directive
  (define (process-elif tokens cond-stack continue)
    ;;   Switch to elif branch if no previous branch was taken
    (if (null? cond-stack)
        ;; #elif without #if - ignore
        (continue cond-stack '())
        (let* ([top (car cond-stack)]
               [parent-stack (cdr cond-stack)]
               [taken? (car top)]
               [parent-active? (or (null? parent-stack)
                                   (cdr (car parent-stack)))])
          (cond
            ;; Already took a branch - skip elif
            [taken?
             (continue (cons (cons #t #f) parent-stack) '())]
            ;; Parent is inactive - stay inactive
            [(not parent-active?)
             (continue (cons (cons #f #f) parent-stack) '())]
            ;; Evaluate elif condition
            [else
             (let* ([result (eval-conditional! 'elif tokens)]
                    [taken? (and (number? result) (not (zero? result)))]
                    [active? (and parent-active? taken?)])
               (continue (cons (cons taken? active?) parent-stack) '()))]))))

  ;; Process #else directive
  (define (process-else tokens cond-stack continue)
    ;;   Switch to else branch if no previous branch was taken
    (if (null? cond-stack)
        ;; #else without #if - ignore
        (continue cond-stack '())
        (let* ([top (car cond-stack)]
               [parent-stack (cdr cond-stack)]
               [taken? (car top)]
               [parent-active? (or (null? parent-stack)
                                   (cdr (car parent-stack)))]
               [active? (and parent-active? (not taken?))])
          ;;(display (format "#else: taken?=~a parent-active?=~a active?=~a\n" taken? parent-active? active?))
          (continue (cons (cons #t active?) parent-stack) '()))))

  ;; Process #endif directive
  (define (process-endif tokens cond-stack continue)
    ;;   Pop conditional level
    (if (null? cond-stack)
        ;; #endif without #if - ignore
        (continue cond-stack '())
        (continue (cdr cond-stack) '())))

  ;; Process #include directive
  (define (process-include tokens cond-stack continue)
    (if (null? tokens)
        (continue cond-stack '())
        (let ([tok (car tokens)])
          (cond
            ;; #include <file>
            [(and (punctuator? tok) (equal? (token-value tok) "<"))
             (let ([filename (extract-angle-include (cdr tokens))])
               (if filename
                   (let ([result (resolve-include! filename #t)])
                     (if result
                         (let* ([path (car result)]
                                [content (cdr result)]
                                [content-str (if (bytevector? content)
                                                 (utf8->string content)
                                                 content)]
                                [inc-tokens (tokenize-string content-str path)])
                           ;; Return raw tokens for preprocessing in current context
                           (continue cond-stack inc-tokens))
                         (continue cond-stack '())))
                   (continue cond-stack '())))]

            ;; #include "file"
            [(string-literal? tok)
             (let* ([filename (token-value tok)]
                    [result (resolve-include! filename #f)])
               (if result
                   (let* ([path (car result)]
                          [content (cdr result)]
                          [content-str (if (bytevector? content)
                                           (utf8->string content)
                                           content)]
                          [inc-tokens (tokenize-string content-str path)])
                     ;; Return raw tokens for preprocessing in current context
                     (continue cond-stack inc-tokens))
                   (continue cond-stack '())))]

            ;; #include MACRO - expand macro and retry
            [(identifier-token? tok)
             (let ([expanded (expand-macro! (token-value tok) #f)])
               (if (or (eq? expanded 'undefined) (eq? expanded 'painted))
                   ;; Macro not defined or painted, skip include
                   (continue cond-stack '())
                   ;; Macro expanded, process the result as include path
                   (process-include expanded cond-stack continue)))]

            [else
             (continue cond-stack '())]))))

  ;; Extract filename from <...> include
  (define (extract-angle-include tokens)
    (let loop ([toks tokens] [parts '()])
      (if (null? toks)
          #f
          (let ([tok (car toks)])
            (cond
              [(and (punctuator? tok) (equal? (token-value tok) ">"))
               (apply string-append (reverse parts))]
              [(identifier-token? tok)
               (loop (cdr toks) (cons (symbol->string (token-value tok)) parts))]
              [(punctuator? tok)
               (loop (cdr toks) (cons (token-value tok) parts))]
              [else
               (loop (cdr toks) parts)])))))

  ;; Preprocess a string of C source code
  (define (preprocess-string str filename)
    ;;   Preprocess C source string, returns preprocessed tokens
    (let ([tokens (tokenize-string str filename)])
      (preprocess-tokens tokens)))

  ;; Preprocess a C source file
  (define (preprocess-file path)
    ;;   Preprocess C source file, returns preprocessed tokens
    (let ([tokens (tokenize-file path)])
      (preprocess-tokens tokens))))
