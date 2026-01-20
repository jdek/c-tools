;; SPDX-License-Identifier: WTFPL
;; C Preprocessor Macro Expansion Handlers
;; Object-like and function-like macro expansion with blue paint

(library (c-tools effects cpp macros)
  (export with-cpp-macros
          register-cpp-macros!
          make-macro-def macro-def?
          macro-def-name macro-def-params macro-def-body
          macro-def-is-function?)
  (import (rnrs base)
          (rnrs control)
          (rnrs hashtables)
          (rnrs lists)
          (rnrs records syntactic)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools effects registry)
          (only (chezscheme) format))

  ;; Macro definition record
  ;; - name: symbol (macro name)
  ;; - params: #f for object-like, list of symbols for function-like
  ;; - body: list of tokens representing the replacement
  (define-record-type macro-def
    (fields name params body))

  (define (macro-def-is-function? def)
    (and (macro-def? def)
         (list? (macro-def-params def))))

  ;; Build parameter -> argument mapping
  ;; Handles variadic macros: if last param is __VA_ARGS__, it gets all remaining args
  (define (make-param-map params args)
    ;;   Create hashtable mapping parameter symbols to argument token lists
    (let ([map (make-eq-hashtable)])
      (when (and params args)
        (let loop ([ps params] [as args])
          (cond
            ;; No more params
            [(null? ps) #f]
            ;; Variadic: __VA_ARGS__ gets all remaining args
            [(and (eq? (car ps) '__VA_ARGS__) (null? (cdr ps)))
             ;; Flatten remaining args with comma tokens between them
             (let ([va-tokens (flatten-args-with-commas as)])
               (hashtable-set! map '__VA_ARGS__ va-tokens))]
            ;; Regular param
            [(pair? as)
             (hashtable-set! map (car ps) (car as))
             (loop (cdr ps) (cdr as))]
            ;; No more args but still have params - leave unset
            [else #f])))
      map))

  ;; Flatten multiple argument lists into one, inserting comma tokens between
  (define (flatten-args-with-commas args)
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
                    [comma (make-token 'punctuator "," (macro-expansion-location))])
               (loop (cdr remaining)
                     (cons comma (append (reverse arg-tokens) result))))]))))

  ;; Check if token is the # operator (stringification)
  ;; Note: Lexer may produce 'directive or 'punctuator for #
  (define (stringify-op? tok)
    (or (and (punctuator? tok)
             (equal? (token-value tok) "#"))
        (and (preprocessor-directive? tok))))

  ;; Check if token is the ## operator
  (define (paste-op? tok)
    (and (punctuator? tok)
         (equal? (token-value tok) "##")))

  ;; Convert token list to string for stringification
  (define (tokens->string tokens)
    ;;   Convert a list of tokens to their string representation
    (if (null? tokens)
        ""
        (let loop ([toks tokens] [parts '()])
          (if (null? toks)
              (apply string-append (reverse parts))
              (let ([tok (car toks)])
                (cond
                  [(identifier-token? tok)
                   (loop (cdr toks) (cons (symbol->string (token-value tok)) parts))]
                  [(keyword-token? tok)
                   (loop (cdr toks) (cons (symbol->string (token-value tok)) parts))]
                  [(number-token? tok)
                   (loop (cdr toks) (cons (token-value tok) parts))]
                  [(string-literal? tok)
                   (loop (cdr toks) (cons (string-append "\"" (token-value tok) "\"") parts))]
                  [(char-literal? tok)
                   (loop (cdr toks) (cons (string-append "'" (token-value tok) "'") parts))]
                  [(punctuator? tok)
                   (loop (cdr toks) (cons (token-value tok) parts))]
                  [else
                   (loop (cdr toks) parts)]))))))

  ;; Paste two tokens together
  (define (paste-tokens tok1 tok2 location)
    ;;   Concatenate two tokens into a single token
    (let ([str1 (cond
                  [(identifier-token? tok1) (symbol->string (token-value tok1))]
                  [(keyword-token? tok1) (symbol->string (token-value tok1))]
                  [(number-token? tok1) (token-value tok1)]
                  [(punctuator? tok1) (token-value tok1)]
                  [else ""])]
          [str2 (cond
                  [(identifier-token? tok2) (symbol->string (token-value tok2))]
                  [(keyword-token? tok2) (symbol->string (token-value tok2))]
                  [(number-token? tok2) (token-value tok2)]
                  [(punctuator? tok2) (token-value tok2)]
                  [else ""])])
      (let ([pasted (string-append str1 str2)])
        ;; Try to determine the type of the pasted token
        ;; For now, just make it an identifier
        (make-token 'identifier (string->symbol pasted) location))))

  ;; Substitute parameters with arguments in macro body
  (define (substitute-tokens body params args location)
    ;;   Substitute parameters with arguments, handling # and ## operators
    ;; If no params (object-like macro), just return body
    (if (not params)
        body
        (let ([param-map (make-param-map params args)])
          (let loop ([tokens body] [result '()])
            (cond
              ;; End of tokens
              [(null? tokens)
               (reverse result)]

              ;; # operator (stringification)
              [(and (pair? tokens)
                    (stringify-op? (car tokens))
                    (pair? (cdr tokens))
                    (identifier-token? (cadr tokens)))
               (let* ([param (token-value (cadr tokens))]
                      [arg (hashtable-ref param-map param #f)])
                 (if arg
                     ;; Stringify the argument
                     (let ([str (tokens->string arg)])
                       (loop (cddr tokens)
                             (cons (make-token 'string str location) result)))
                     ;; Not a parameter, keep as-is
                     (loop (cdr tokens) (cons (car tokens) result))))]

              ;; ## operator (token pasting)
              [(and (pair? tokens)
                    (pair? result)
                    (paste-op? (car tokens))
                    (pair? (cdr tokens)))
               (let ([left (car result)]
                     [right (cadr tokens)])
                 ;; Substitute right side if it's a parameter
                 (let ([right-subst (if (identifier-token? right)
                                       (let ([arg (hashtable-ref param-map (token-value right) #f)])
                                         (if (and arg (pair? arg))
                                             (car arg)  ;; Use first token of argument
                                             right))
                                       right)])
                   ;; Paste left and right
                   (loop (cddr tokens)
                         (cons (paste-tokens left right-subst location) (cdr result)))))]

              ;; Regular token - check if it's a parameter
              [(identifier-token? (car tokens))
               (let* ([param (token-value (car tokens))]
                      [arg (hashtable-ref param-map param #f)])
                 (if arg
                     ;; Replace with argument tokens
                     (loop (cdr tokens) (append (reverse arg) result))
                     ;; Not a parameter, keep as-is
                     (loop (cdr tokens) (cons (car tokens) result))))]

              ;; Other tokens, keep as-is
              [else
               (loop (cdr tokens) (cons (car tokens) result))])))))

  ;; Rescan tokens for further macro expansion
  (define (rescan-tokens tokens painted-table macros k loop)
    ;;   Recursively expand macros in token list, respecting blue paint
    ;; For now, just return tokens unchanged
    ;; Full rescanning would require re-entering the expansion handler
    ;; which is complex and will be implemented when we have the full preprocessor
    tokens)

  ;; Main macro handler
  ;; Combines cpp-define, cpp-undef, cpp-expand, and cpp-symbol into one handler
  ;; with shared state
  (define (with-cpp-macros thunk)
    (let ([macros (make-eq-hashtable)]      ;; symbol -> macro-def
          [painted (make-eq-hashtable)])    ;; symbol -> #t (blue paint)

      ;; Innermost: cpp-symbol (query if defined)
      (with-handler 'cpp-symbol
        (lambda (data k loop)
          (k (hashtable-contains? macros data)))

        ;; cpp-expand
        (with-handler 'cpp-expand
        (lambda (data k loop)
          (let ([name (car data)]
                [args (cdr data)])

            ;; Check blue paint (prevents infinite recursion)
            (if (hashtable-ref painted name #f)
                ;; Painted - return special marker to indicate no expansion
                (k 'painted)

                ;; Not painted - try to expand
                (let ([macro (hashtable-ref macros name #f)])
                  (if (not macro)
                      ;; Undefined macro
                      (k 'undefined)

                      ;; Found macro - check if invocation matches definition
                      (let ([params (macro-def-params macro)]
                            [body (macro-def-body macro)])

                        ;; Validate function-like vs object-like
                        (cond
                          ;; Function-like macro requires args
                          [(and (list? params) (not args))
                           (k 'undefined)]  ;; Invoked without (), treat as undefined

                          ;; Object-like macro shouldn't have args
                          [(and (not params) args)
                           (k 'undefined)]  ;; Object-like invoked with (), invalid

                          ;; Perform substitution
                          [else
                           (let* ([location (if (pair? body)
                                               (token-location (car body))
                                               (macro-expansion-location))]
                                  [substituted (substitute-tokens body params args location)])

                             ;; Apply blue paint
                             (hashtable-set! painted name #t)

                             ;; Rescan the expanded tokens
                             (let ([rescanned (rescan-tokens substituted painted macros k loop)])

                               ;; Remove blue paint
                               (hashtable-delete! painted name)

                               ;; Return expanded tokens
                               (k rescanned)))])))))))

        ;; Middle: cpp-define
        (with-handler 'cpp-define
          (lambda (data k loop)
            (let ([name (car data)]
                  [params (cadr data)]
                  [body (caddr data)])

              ;; Store macro definition
              (hashtable-set! macros name (make-macro-def name params body))
              (k #f)))

          ;; Outer: cpp-undef
          (with-handler 'cpp-undef
            (lambda (data k loop)
              (hashtable-delete! macros data)
              (k #f))

            (thunk)))))))

  ;;=========================================================================
  ;; Registration

  (define (register-cpp-macros!)
    (register-effect! 'cpp-macros
      (lambda (spec thunk)
        (with-cpp-macros thunk))))

  ;; Auto-register on load
  (register-cpp-macros!))
