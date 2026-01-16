;; C++ Tokenizer - Lexical Analysis
;; Port-based tokenizer with security limits
;; Uses closure pattern - all helpers close over port and state

(library (c-tools lexer cpp)
  (export tokenize-cpp-port
          tokenize-cpp-string
          tokenize-cpp-file
          cpp-keywords
          cpp-two-char-operators
          cpp-three-char-operators)
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

  ;; C++ keywords (extends C keywords)
  (define cpp-keywords
    '(;; C keywords
      auto break case char const continue default do
      double else enum extern float for goto if
      inline int long register restrict return short
      signed sizeof static struct switch typedef
      union unsigned void volatile while
      _Bool _Complex _Imaginary
      ;; C++ keywords
      namespace using
      class public private protected virtual
      template typename
      operator new delete
      const_cast static_cast dynamic_cast reinterpret_cast
      try catch throw
      explicit mutable constexpr consteval constinit
      bool true false
      this
      friend
      override final
      noexcept nullptr
      decltype
      concept requires
      co_await co_return co_yield
      export import module))

  ;; C++ two-character operators (extends C)
  (define cpp-two-char-operators
    '("==" "!=" "<=" ">=" "&&" "||"
      "++" "--" "<<" ">>"
      "+=" "-=" "*=" "/=" "%="
      "&=" "|=" "^="
      "->" "##"
      ;; C++ specific
      "::" ".*" "<:" ":>" "<%" "%>"))

  ;; C++ three-character operators
  (define cpp-three-char-operators
    '("..." "<<=" ">>=" "->*" "<=>"))

  ;; Main tokenization entry point
  ;; All internal functions close over port, filename, and mutable state
  (define (tokenize-cpp-port port filename)
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
          (if (memq sym cpp-keywords)
              (make-token 'keyword sym loc)
              (make-token 'identifier sym loc)))))

    ;; Tokenize number (decimal, hex, octal, float, binary, separators)
    (define (tokenize-number)
      (define (digit-or-sep? c)
        (or (c-digit? c) (and (char? c) (char=? c #\'))))

      (define (hex-or-sep? c)
        (or (c-hex-digit? c) (and (char? c) (char=? c #\'))))

      (define (binary-digit? c)
        (and (char? c) (or (char=? c #\0) (char=? c #\1))))

      (define (binary-or-sep? c)
        (or (binary-digit? c) (and (char? c) (char=? c #\'))))

      (let ([loc (current-location)])
        (let ([c (peek)])
          (cond
            ;; Check for hex/binary/octal: 0x, 0X, 0b, 0B
            [(char=? c #\0)
             (advance!)  ;; consume '0'
             (let ([c2 (peek)])
               (cond
                 ;; Hex: 0x or 0X
                 [(and (char? c2) (or (char=? c2 #\x) (char=? c2 #\X)))
                  (advance!)  ;; consume 'x'/'X'
                  (let ([digits (read-while hex-or-sep?)])
                    (make-token 'number
                                (call-with-string-output-port
                                  (lambda (out)
                                    (put-string out "0x")
                                    (put-string out digits)))
                                loc))]
                 ;; Binary: 0b or 0B
                 [(and (char? c2) (or (char=? c2 #\b) (char=? c2 #\B)))
                  (advance!)  ;; consume 'b'/'B'
                  (let ([digits (read-while binary-or-sep?)])
                    (make-token 'number
                                (call-with-string-output-port
                                  (lambda (out)
                                    (put-string out "0b")
                                    (put-string out digits)))
                                loc))]
                 ;; Octal or float starting with 0
                 [else
                  (let ([rest (read-while digit-or-sep?)])
                    (let ([c3 (peek)])
                      (if (and (char? c3) (char=? c3 #\.))
                          ;; Float starting with 0
                          (begin
                            (advance!)  ;; consume '.'
                            (let ([frac (read-while digit-or-sep?)])
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
                                      loc))))]))]
            ;; Decimal number
            [else
             (let ([digits (read-while digit-or-sep?)])
               (let ([c2 (peek)])
                 (if (and (char? c2) (char=? c2 #\.))
                     ;; Float
                     (begin
                       (advance!)  ;; consume '.'
                       (let ([frac (read-while digit-or-sep?)])
                         ;; Check for exponent
                         (let ([c3 (peek)])
                           (if (and (char? c3) (or (char=? c3 #\e) (char=? c3 #\E)))
                               (begin
                                 (advance!)  ;; consume 'e'/'E'
                                 (let* ([sign (let ([c4 (peek)])
                                                (if (and (char? c4)
                                                         (or (char=? c4 #\+) (char=? c4 #\-)))
                                                    (string (advance!))
                                                    ""))]
                                        [exp (read-while digit-or-sep?)])
                                   (make-token 'number
                                               (call-with-string-output-port
                                                 (lambda (out)
                                                   (put-string out digits)
                                                   (put-char out #\.)
                                                   (put-string out frac)
                                                   (put-char out #\e)
                                                   (put-string out sign)
                                                   (put-string out exp)))
                                               loc)))
                               (make-token 'number
                                           (call-with-string-output-port
                                             (lambda (out)
                                               (put-string out digits)
                                               (put-char out #\.)
                                               (put-string out frac)))
                                           loc)))))
                     ;; Integer
                     (make-token 'number digits loc))))]))))

    ;; Tokenize string literal (including raw strings R"...")
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

    ;; Tokenize raw string literal R"delim(...)delim"
    (define (tokenize-raw-string)
      (let ([loc (current-location)])
        (advance!)  ;; consume 'R'
        (advance!)  ;; consume '"'
        ;; Read delimiter (chars before '(')
        (let ([delim (call-with-string-output-port
                       (lambda (out)
                         (let loop ()
                           (let ([c (peek)])
                             (cond
                               [(eof-object? c)
                                (raise
                                  (condition
                                    (make-cpp-parse-error loc)
                                    (make-message-condition "Unterminated raw string")))]
                               [(char=? c #\()
                                (advance!)
                                #f]  ;; done with delimiter
                               [else
                                (put-char out (advance!))
                                (loop)])))))])
          ;; Read content until )delim"
          (let ([end-marker (call-with-string-output-port
                              (lambda (out)
                                (put-char out #\))
                                (put-string out delim)
                                (put-char out #\")))])
            (let ([str (call-with-string-output-port
                         (lambda (out)
                           (let loop ()
                             (let ([c (peek)])
                               (cond
                                 [(eof-object? c)
                                  (raise
                                    (condition
                                      (make-cpp-parse-error loc)
                                      (make-message-condition "Unterminated raw string")))]
                                 [(char=? c #\))
                                  ;; Check if this is the end marker
                                  (let check ([i 0] [chars '()])
                                    (if (>= i (string-length end-marker))
                                        #f  ;; found end marker, done
                                        (let ([c2 (advance!)])
                                          (if (and (char? c2)
                                                   (char=? c2 (string-ref end-marker i)))
                                              (check (+ i 1) (cons c2 chars))
                                              ;; Not end marker, put chars back to output
                                              (begin
                                                (for-each (lambda (ch) (put-char out ch))
                                                          (reverse chars))
                                                (when (char? c2)
                                                  (put-char out c2))
                                                (loop))))))]
                                 [else
                                  (put-char out (advance!))
                                  (loop)])))))])
              (make-token 'string str loc))))))

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
        ;; Check for three-char operators first
        (let ([c2 (peek)])
          (if (char? c2)
              (let ([two-str (call-with-string-output-port
                               (lambda (out)
                                 (put-char out c1)
                                 (put-char out c2)))])
                (cond
                  ;; Check for "..."
                  [(and (char=? c1 #\.) (char=? c2 #\.))
                   (advance!)  ;; consume second '.'
                   (let ([c3 (peek)])
                     (if (and (char? c3) (char=? c3 #\.))
                         (begin
                           (advance!)  ;; consume third '.'
                           (make-token 'punctuator "..." loc))
                         ;; Just ".." - not standard but return it
                         (make-token 'punctuator ".." loc)))]
                  ;; Check for <<= or >>=
                  [(or (string=? two-str "<<") (string=? two-str ">>"))
                   (advance!)  ;; consume second char
                   (let ([c3 (peek)])
                     (if (and (char? c3) (char=? c3 #\=))
                         (begin
                           (advance!)
                           (make-token 'punctuator
                                       (call-with-string-output-port
                                         (lambda (out)
                                           (put-string out two-str)
                                           (put-char out #\=)))
                                       loc))
                         (make-token 'punctuator two-str loc)))]
                  ;; Check for ->*
                  [(string=? two-str "->")
                   (advance!)  ;; consume >
                   (let ([c3 (peek)])
                     (if (and (char? c3) (char=? c3 #\*))
                         (begin
                           (advance!)
                           (make-token 'punctuator "->*" loc))
                         (make-token 'punctuator "->" loc)))]
                  ;; Check for <=> (spaceship operator)
                  [(and (char=? c1 #\<) (char=? c2 #\=))
                   (advance!)  ;; consume =
                   (let ([c3 (peek)])
                     (if (and (char? c3) (char=? c3 #\>))
                         (begin
                           (advance!)
                           (make-token 'punctuator "<=>" loc))
                         (make-token 'punctuator "<=" loc)))]
                  ;; Other two-char operators
                  [(member two-str cpp-two-char-operators)
                   (advance!)
                   (make-token 'punctuator two-str loc)]
                  ;; Single char
                  [else
                   (make-token 'punctuator (string c1) loc)]))
              ;; EOF after first char
              (make-token 'punctuator (string c1) loc)))))

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

            ;; Identifier, keyword, or raw string literal
            [(c-identifier-start? c)
             ;; Check for raw string: R"..."
             (if (char=? c #\R)
                 (let ()
                   (advance!)  ;; consume R
                   (let ([c2 (peek)])
                     (if (and (char? c2) (char=? c2 #\"))
                         ;; Raw string
                         (begin
                           (set! column (- column 1))  ;; back up for location
                           (loop (cons (tokenize-raw-string) tokens)))
                         ;; Just identifier starting with R
                         (begin
                           (set! column (- column 1))  ;; back up for location
                           (let ([rest (read-while c-identifier-char?)])
                             (let ([full-str (call-with-string-output-port
                                               (lambda (out)
                                                 (put-char out #\R)
                                                 (put-string out rest)))])
                               (let ([sym (string->symbol full-str)]
                                     [loc (make-location filename line (- column (string-length full-str)))])
                                 (loop (cons (if (memq sym cpp-keywords)
                                                 (make-token 'keyword sym loc)
                                                 (make-token 'identifier sym loc))
                                             tokens)))))))))
                 ;; Regular identifier
                 (loop (cons (tokenize-identifier) tokens)))]

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
                  ;; Check for /=
                  (if (and (char? c2) (char=? c2 #\=))
                      (begin
                        (advance!)
                        (loop (cons (make-token 'punctuator "/=" (make-location filename line (- column 2)))
                                    tokens)))
                      ;; Just /
                      (loop (cons (make-token 'punctuator "/" (make-location filename line (- column 1)))
                                  tokens)))]))]

            ;; Punctuator
            [else
             (loop (cons (tokenize-punctuator) tokens))]))))

    ;; Entry point
    (tokenize-all))

  ;; Tokenize from string
  (define (tokenize-cpp-string str filename)
    (call-with-port (open-string-input-port str)
      (lambda (port)
        (tokenize-cpp-port port filename))))

  ;; Tokenize from file
  (define (tokenize-cpp-file path)
    (call-with-input-file path
      (lambda (port)
        (tokenize-cpp-port port path)))))
