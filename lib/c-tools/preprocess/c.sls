;; C Preprocessor - Main Entry Point
;; Integrates tokenizer, directive processing, and macro expansion

(library (c-tools preprocess c)
  (export preprocess-string
          preprocess-file
          preprocess-tokens)
  (import (rnrs base)
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
          (only (chezscheme) format last-pair))

  ;; Process a list of tokens, handling directives and expanding macros
  (define (preprocess-tokens tokens)
    ;;   Process a token stream, handling all preprocessor directives
    (let loop ([toks tokens]
               [output '()]
               [skip-depth 0])  ;; > 0 when inside false conditional

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
                 (process-directive directive-tokens skip-depth
                   (lambda (new-skip-depth)
                     (loop remaining-tokens
                           output
                           new-skip-depth))))]

              ;; Skip tokens inside false conditional
              [(> skip-depth 0)
               (loop (cdr toks) output skip-depth)]

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
                               (loop stream-tokens (cons lparen (cons tok output)) skip-depth))]
                           ;; Expanded - insert expanded tokens
                           [else
                            (loop (append expanded remaining) output skip-depth)])))
                     ;; No '(' - try object-like macro
                     (let ([expanded (expand-macro! (token-value tok) #f)])
                       (cond
                         ;; Undefined or painted - keep as-is
                         [(or (eq? expanded 'undefined) (eq? expanded 'painted))
                          (loop (cdr toks) (cons tok output) skip-depth)]
                         ;; Expanded - insert expanded tokens
                         [else
                          (loop (append expanded (cdr toks)) output skip-depth)]))))]

              ;; EOF - done
              [(eof-token? tok)
               (reverse output)]

              ;; Comment - skip
              [(comment-token? tok)
               (loop (cdr toks) output skip-depth)]

              ;; Other tokens - pass through
              [else
               (loop (cdr toks) (cons tok output) skip-depth)])))))

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
  (define (process-directive tokens skip-depth continue)
    ;;   Process a directive and call CONTINUE with new skip depth
    (if (null? tokens)
        (continue skip-depth)

        (let ([first (car tokens)])
          (cond
            ;; #define
            [(and (identifier-token? first)
                  (eq? (token-value first) 'define))
             (if (> skip-depth 0)
                 (continue skip-depth)
                 (process-define (cdr tokens) skip-depth continue))]

            ;; #undef
            [(and (identifier-token? first)
                  (eq? (token-value first) 'undef))
             (if (> skip-depth 0)
                 (continue skip-depth)
                 (process-undef (cdr tokens) skip-depth continue))]

            ;; #ifdef
            [(and (identifier-token? first)
                  (eq? (token-value first) 'ifdef))
             (process-ifdef (cdr tokens) skip-depth continue)]

            ;; #ifndef
            [(and (identifier-token? first)
                  (eq? (token-value first) 'ifndef))
             (process-ifndef (cdr tokens) skip-depth continue)]

            ;; #if
            [(and (identifier-token? first)
                  (eq? (token-value first) 'if))
             (process-if (cdr tokens) skip-depth continue)]

            ;; #endif
            [(and (identifier-token? first)
                  (eq? (token-value first) 'endif))
             (continue (max 0 (- skip-depth 1)))]

            ;; #include
            [(and (identifier-token? first)
                  (eq? (token-value first) 'include))
             (if (> skip-depth 0)
                 (continue skip-depth)
                 (process-include (cdr tokens) skip-depth continue))]

            ;; Unknown directive - ignore
            [else
             (continue skip-depth)]))))

  ;; Process #define directive
  (define (process-define tokens skip-depth continue)
    (if (null? tokens)
        (continue skip-depth)
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
                      (continue skip-depth))
                    ;; Object-like macro
                    (begin
                      (define-macro! name #f rest)
                      (continue skip-depth))))
              (continue skip-depth)))))

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
  (define (process-undef tokens skip-depth continue)
    (if (null? tokens)
        (continue skip-depth)
        (let ([name-tok (car tokens)])
          (if (identifier-token? name-tok)
              (begin
                (undefine-macro! (token-value name-tok))
                (continue skip-depth))
              (continue skip-depth)))))

  ;; Process #ifdef directive
  (define (process-ifdef tokens skip-depth continue)
    (if (null? tokens)
        (continue (+ skip-depth 1))
        (let ([name-tok (car tokens)])
          (if (identifier-token? name-tok)
              (let ([defined? (eval-conditional! 'ifdef (token-value name-tok))])
                (continue (if defined? skip-depth (+ skip-depth 1))))
              (continue (+ skip-depth 1))))))

  ;; Process #ifndef directive
  (define (process-ifndef tokens skip-depth continue)
    (if (null? tokens)
        (continue (+ skip-depth 1))
        (let ([name-tok (car tokens)])
          (if (identifier-token? name-tok)
              (let ([not-defined? (eval-conditional! 'ifndef (token-value name-tok))])
                (continue (if not-defined? skip-depth (+ skip-depth 1))))
              (continue (+ skip-depth 1))))))

  ;; Process #if directive
  (define (process-if tokens skip-depth continue)
    (let ([result (eval-conditional! 'if tokens)])
      (continue (if result skip-depth (+ skip-depth 1)))))

  ;; Process #include directive
  (define (process-include tokens skip-depth continue)
    (if (null? tokens)
        (continue skip-depth)
        (let ([tok (car tokens)])
          (cond
            ;; #include <file>
            [(and (punctuator? tok) (equal? (token-value tok) "<"))
             (let ([filename (extract-angle-include (cdr tokens))])
               (if filename
                   (begin
                     (resolve-include! filename #t)
                     (continue skip-depth))
                   (continue skip-depth)))]

            ;; #include "file"
            [(string-literal? tok)
             (let ([filename (token-value tok)])
               (resolve-include! filename #f)
               (continue skip-depth))]

            [else
             (continue skip-depth)]))))

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
