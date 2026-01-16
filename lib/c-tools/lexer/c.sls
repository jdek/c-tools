;; C Tokenizer - Lexical Analysis
;; Port-based tokenizer with security limits
;; Uses closure pattern - all helpers close over port and state

(library (c-tools lexer c)
  (export tokenize-port
          tokenize-string
          tokenize-file
          c-keywords)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs lists)
          (c-tools core base)
          (c-tools core conditions)
          (c-tools core limits)
          (c-tools core tokens)
          (only (chezscheme) format))

  ;; C keywords
  (define c-keywords
    '(auto break case char const continue default do
      double else enum extern float for goto if
      inline int long register restrict return short
      signed sizeof static struct switch typedef
      union unsigned void volatile while
      _Bool _Complex _Imaginary))

  ;;=======================================================================
  ;; Tokenization Entry Points

  ;; tokenize-port : input-port string => (list token)
  ;;   effects: io/read
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C source from input port, returns list of tokens.
  (define (tokenize-port port filename)
    ;; Mutable state - closed over, not threaded
    (define line 1)
    (define column 1)
    (define bytes-read 0)
    (define token-count 0)

    ;; Port operations - close over port
    (define (peek)
      (lookahead-char port))

    (define (advance!)
      (let ([c (get-char port)])
        (when (char? c)
          (if (char=? c #\newline)
              (begin (set! line (+ line 1))
                     (set! column 1))
              (set! column (+ column 1)))
          (set! bytes-read (+ bytes-read 1))
          (when (> bytes-read max-header-size)
            (raise
              (condition
                (make-cpp-limit-exceeded 'bytes-read bytes-read)
                (make-message-condition "Header size limit exceeded")))))
        c))

    (define (current-location)
      (make-location filename line column))

    ;; Skip whitespace
    (define (skip-whitespace!)
      (let loop ()
        (let ([c (peek)])
          (when (and (char? c) (c-whitespace? c))
            (advance!)
            (loop)))))

    ;; Read while predicate holds, return string
    (define (read-while pred)
      (call-with-string-output-port
        (lambda (out)
          (let loop ()
            (let ([c (peek)])
              (when (and (char? c) (pred c))
                (put-char out (advance!))
                (loop)))))))

    ;; Tokenize identifier or keyword
    (define (tokenize-identifier)
      (let ([loc (current-location)]
            [str (read-while c-identifier-char?)])
        (when (> (string-length str) max-token-length)
          (raise
            (condition
              (make-cpp-parse-error loc)
              (make-message-condition "Identifier too long")
              (make-irritants-condition (list str)))))
        (let ([sym (string->symbol str)])
          (if (memq sym c-keywords)
              (make-token 'keyword sym loc)
              (make-token 'identifier sym loc)))))

    ;; Tokenize number (decimal, hex, octal, float)
    (define (tokenize-number)
      (let ([loc (current-location)])
        (let ([c (peek)])
          (cond
            ;; Check for hex: 0x or 0X
            [(char=? c #\0)
             (advance!)  ;; consume '0'
             (let ([c2 (peek)])
               (if (and (char? c2) (or (char=? c2 #\x) (char=? c2 #\X)))
                   ;; Hex number
                   (begin
                     (advance!)  ;; consume 'x'/'X'
                     (let ([digits (read-while c-hex-digit?)])
                       (make-token 'number
                                   (call-with-string-output-port
                                     (lambda (out)
                                       (put-string out "0x")
                                       (put-string out digits)))
                                   loc)))
                   ;; Octal or just "0" or "0.xxx"
                   (let ([rest (read-while c-digit?)])
                     (let ([c3 (peek)])
                       (if (and (char? c3) (char=? c3 #\.))
                           ;; Float starting with 0
                           (begin
                             (advance!)  ;; consume '.'
                             (let ([frac (read-while c-digit?)])
                               (make-token 'number
                                           (call-with-string-output-port
                                             (lambda (out)
                                               (put-char out #\0)
                                               (put-string out rest)
                                               (put-char out #\.)
                                               (put-string out frac)))
                                           loc)))
                           ;; Integer (octal or zero)
                           (make-token 'number
                                       (call-with-string-output-port
                                         (lambda (out)
                                           (put-char out #\0)
                                           (put-string out rest)))
                                       loc))))))]
            ;; Decimal number
            [else
             (let ([digits (read-while c-digit?)])
               (let ([c2 (peek)])
                 (if (and (char? c2) (char=? c2 #\.))
                     ;; Float
                     (begin
                       (advance!)  ;; consume '.'
                       (let ([frac (read-while c-digit?)])
                         (make-token 'number
                                     (call-with-string-output-port
                                       (lambda (out)
                                         (put-string out digits)
                                         (put-char out #\.)
                                         (put-string out frac)))
                                     loc)))
                     ;; Integer
                     (make-token 'number digits loc))))]))))

    ;; Tokenize string literal
    (define (tokenize-string-literal)
      (let ([loc (current-location)])
        (advance!)  ;; consume opening "
        (let ([str (call-with-string-output-port
                     (lambda (out)
                       (let loop ()
                         (let ([c (peek)])
                           (cond
                             [(eof-object? c)
                              (raise
                                (condition
                                  (make-cpp-parse-error loc)
                                  (make-message-condition "Unterminated string literal")))]
                             [(char=? c #\")
                              (advance!)  ;; consume closing "
                              #f]  ;; done
                             [(char=? c #\\)
                              (put-char out (advance!))  ;; put backslash
                              (let ([escaped (advance!)])
                                (if (eof-object? escaped)
                                    (raise
                                      (condition
                                        (make-cpp-parse-error loc)
                                        (make-message-condition "Unterminated escape sequence")))
                                    (begin
                                      (put-char out escaped)
                                      (loop))))]
                             [else
                              (put-char out (advance!))
                              (loop)])))))])
          (when (> (string-length str) max-token-length)
            (raise
              (condition
                (make-cpp-parse-error loc)
                (make-message-condition "String literal too long")
                (make-irritants-condition (list str)))))
          (make-token 'string str loc))))

    ;; Tokenize character literal
    (define (tokenize-char-literal)
      (let ([loc (current-location)])
        (advance!)  ;; consume opening '
        (let ([str (call-with-string-output-port
                     (lambda (out)
                       (let ([c (advance!)])
                         (cond
                           [(eof-object? c)
                            (raise
                              (condition
                                (make-cpp-parse-error loc)
                                (make-message-condition "Unterminated char literal")))]
                           [(char=? c #\\)
                            (put-char out c)
                            (let ([escaped (advance!)])
                              (if (eof-object? escaped)
                                  (raise
                                    (condition
                                      (make-cpp-parse-error loc)
                                      (make-message-condition "Unterminated escape sequence")))
                                  (put-char out escaped)))]
                           [else
                            (put-char out c)]))))])
          (let ([close (advance!)])
            (if (and (char? close) (char=? close #\'))
                (make-token 'char str loc)
                (raise
                  (condition
                    (make-cpp-parse-error loc)
                    (make-message-condition "Unterminated char literal"))))))))

    ;; Tokenize line comment (// ...)
    (define (tokenize-line-comment)
      (let ([loc (current-location)])
        (advance!)  ;; consume first /
        (advance!)  ;; consume second /
        (let ([text (call-with-string-output-port
                      (lambda (out)
                        (let loop ()
                          (let ([c (peek)])
                            (unless (or (eof-object? c) (char=? c #\newline))
                              (put-char out (advance!))
                              (loop))))))])
          (make-token 'comment text loc))))

    ;; Tokenize block comment (/* ... */)
    (define (tokenize-block-comment)
      (let ([loc (current-location)])
        (advance!)  ;; consume /
        (advance!)  ;; consume *
        (let ([text (call-with-string-output-port
                      (lambda (out)
                        (let loop ()
                          (let ([c (advance!)])
                            (cond
                              [(eof-object? c)
                               (raise
                                 (condition
                                   (make-cpp-parse-error loc)
                                   (make-message-condition "Unterminated block comment")))]
                              [(char=? c #\*)
                               (let ([c2 (peek)])
                                 (if (and (char? c2) (char=? c2 #\/))
                                     (begin (advance!) #f)  ;; done
                                     (begin (put-char out c) (loop))))]
                              [else
                               (put-char out c)
                               (loop)])))))])
          (make-token 'comment text loc))))

    ;; Tokenize punctuator (handles multi-char operators)
    (define (tokenize-punctuator)
      (let ([loc (current-location)]
            [c1 (advance!)])
        ;; Check for three-char "..."
        (if (char=? c1 #\.)
            (let ([c2 (peek)])
              (if (and (char? c2) (char=? c2 #\.))
                  (begin
                    (advance!)  ;; consume second '.'
                    (let ([c3 (peek)])
                      (if (and (char? c3) (char=? c3 #\.))
                          (begin
                            (advance!)  ;; consume third '.'
                            (make-token 'punctuator "..." loc))
                          ;; Just ".." - not valid C, but return it
                          (make-token 'punctuator ".." loc))))
                  (make-token 'punctuator "." loc)))
            ;; Check for two-char operators
            (let ([c2 (peek)])
              (if (char? c2)
                  (let ([two-char (call-with-string-output-port
                                    (lambda (out)
                                      (put-char out c1)
                                      (put-char out c2)))])
                    (if (member two-char c-two-char-operators)
                        (begin
                          (advance!)
                          (make-token 'punctuator two-char loc))
                        (make-token 'punctuator (string c1) loc)))
                  (make-token 'punctuator (string c1) loc))))))

    ;; Main tokenization loop
    (define (tokenize-all)
      (let loop ([tokens '()])
        (skip-whitespace!)
        (let ([c (peek)])
          (cond
            [(eof-object? c)
             (set! token-count (+ token-count 1))
             (when (> token-count max-tokens-per-file)
               (raise
                 (condition
                   (make-cpp-limit-exceeded 'token-count token-count)
                   (make-message-condition "Token count limit exceeded"))))
             (reverse (cons (make-token 'eof #f (current-location)) tokens))]

            ;; Preprocessor directive
            [(char=? c #\#)
             (let ([loc (current-location)])
               (advance!)
               (loop (cons (make-token 'directive "#" loc) tokens)))]

            ;; Identifier or keyword
            [(c-identifier-start? c)
             (loop (cons (tokenize-identifier) tokens))]

            ;; Number
            [(c-digit? c)
             (loop (cons (tokenize-number) tokens))]

            ;; String literal
            [(char=? c #\")
             (loop (cons (tokenize-string-literal) tokens))]

            ;; Char literal
            [(char=? c #\')
             (loop (cons (tokenize-char-literal) tokens))]

            ;; Comment or division
            [(char=? c #\/)
             (advance!)  ;; consume first /
             (let ([c2 (peek)])
               (cond
                 [(and (char? c2) (char=? c2 #\/))
                  ;; Back up column for location
                  (set! column (- column 1))
                  (loop (cons (tokenize-line-comment) tokens))]
                 [(and (char? c2) (char=? c2 #\*))
                  ;; Back up column for location
                  (set! column (- column 1))
                  (loop (cons (tokenize-block-comment) tokens))]
                 [else
                  ;; Just a / punctuator - already consumed
                  (loop (cons (make-token 'punctuator "/" (make-location filename line (- column 1)))
                              tokens))]))]

            ;; Punctuator
            [else
             (loop (cons (tokenize-punctuator) tokens))]))))

    ;; Entry point
    (tokenize-all))

  ;; tokenize-string : string string => (list token)
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C source from string, returns list of tokens.
  (define (tokenize-string str filename)
    (call-with-port (open-string-input-port str)
      (lambda (port)
        (tokenize-port port filename))))

  ;; tokenize-file : string => (list token)
  ;;   effects: io/read
  ;;   raises: on malformed input, I/O error, or security limit exceeded
  ;;   Tokenizes C source from file, returns list of tokens.
  (define (tokenize-file path)
    (call-with-input-file path
      (lambda (port)
        (tokenize-port port path)))))
