;; SPDX-License-Identifier: WTFPL
;; Shared Lexer Core
;; Parameterized tokenization logic shared between C and C++ lexers
;; Uses closure pattern with proper location tracking (no backward mutation)

(library (c-tools lexer core)
  (export tokenize-port-with-config)

  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (rnrs io ports)
          (rnrs lists)
          (c-tools core base)
          (c-tools core conditions)
          (c-tools core cst)
          (c-tools core limits)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools lexer config)
          (only (chezscheme) format))

  ;; tokenize-port-with-config : lexer-config input-port string => (list cst-node)
  ;;   effects: io/read, check-limit (via effects)
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes source from input port using config, returns CST nodes
  (define (tokenize-port-with-config config port filename)
    ;; Mutable state - closed over, not threaded
    (define line 1)
    (define column 1)
    (define start-line 1)     ;; Capture at token start
    (define start-column 1)   ;; No more backward mutation!
    (define bytes-read 0)
    (define token-count 0)
    (define trivia-buffer '()) ;; Accumulate whitespace/comments

    ;; Extract config fields for convenience
    (define keywords (lexer-config-keywords config))
    (define two-char-ops (lexer-config-two-char-operators config))
    (define three-char-ops (lexer-config-three-char-operators config))
    (define supports-binary? (lexer-config-supports-binary? config))
    (define supports-separators? (lexer-config-supports-separators? config))
    (define supports-exponent? (lexer-config-supports-exponent? config))
    (define supports-raw-strings? (lexer-config-supports-raw-strings? config))
    (define preserve-whitespace? (lexer-config-preserve-whitespace? config))
    (define preserve-comments? (lexer-config-preserve-comments? config))

    ;;=======================================================================
    ;; Location Tracking (NO BACKWARD MUTATION)

    ;; Capture current position as token start location
    (define (capture-location!)
      (set! start-line line)
      (set! start-column column))

    ;; Get the captured location
    (define (get-location)
      (make-location filename start-line start-column))

    ;;=======================================================================
    ;; Port Operations

    (define (peek)
      (lookahead-char port))

    (define (advance!)
      (let ([c (get-char port)])
        (when (char? c)
          ;; Update position
          (if (char=? c #\newline)
              (begin (set! line (+ line 1))
                     (set! column 1))
              (set! column (+ column 1)))
          ;; Track bytes via effect for limit enforcement
          (set! bytes-read (+ bytes-read 1))
          (perform (make-effect 'check-limit
            (list 'bytes-read bytes-read max-header-size (get-location)))))
        c))

    ;; Read while predicate holds, return string
    (define (read-while pred)
      (call-with-string-output-port
        (lambda (out)
          (let loop ()
            (let ([c (peek)])
              (when (and (char? c) (pred c))
                (put-char out (advance!))
                (loop)))))))

    ;;=======================================================================
    ;; Trivia Management

    ;; Add trivia to buffer
    (define (add-trivia! type text loc)
      (when (or (and preserve-whitespace? (eq? type 'whitespace))
                (and preserve-comments? (eq? type 'comment)))
        (set! trivia-buffer (cons (make-trivia type text loc) trivia-buffer))))

    ;; Consume accumulated trivia and reset buffer
    (define (consume-trivia!)
      (let ([trivia (reverse trivia-buffer)])
        (set! trivia-buffer '())
        trivia))

    ;; Make CST node with accumulated trivia
    (define (make-cst-with-trivia token)
      (make-cst-node token (consume-trivia!) '()))

    ;;=======================================================================
    ;; Tokenizers

    ;; Tokenize whitespace
    (define (tokenize-whitespace!)
      (capture-location!)
      (let ([ws (read-while c-whitespace?)])
        (add-trivia! 'whitespace ws (get-location))))

    ;; Tokenize identifier or keyword
    (define (tokenize-identifier)
      (capture-location!)
      (let ([str (read-while c-identifier-char?)])
        (when (> (string-length str) max-token-length)
          (perform (make-effect 'lexer-error
            (cons (format "Identifier too long: ~a" str) (get-location)))))
        (let ([sym (string->symbol str)])
          (if (memq sym keywords)
              (make-token 'keyword sym (get-location))
              (make-token 'identifier sym (get-location))))))

    ;; Tokenize number (parameterized by config)
    (define (tokenize-number)
      ;; Helper: digit or separator predicate
      (define (digit-or-sep? c)
        (or (c-digit? c)
            (and supports-separators? (char? c) (char=? c #\'))))

      (define (hex-or-sep? c)
        (or (c-hex-digit? c)
            (and supports-separators? (char? c) (char=? c #\'))))

      (define (binary-digit? c)
        (and (char? c) (or (char=? c #\0) (char=? c #\1))))

      (define (binary-or-sep? c)
        (or (binary-digit? c)
            (and supports-separators? (char? c) (char=? c #\'))))

      (capture-location!)
      (let ([c (peek)])
        (cond
          ;; Check for 0x/0b prefix
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
                              (get-location)))]

               ;; Binary: 0b or 0B (C++ only)
               [(and supports-binary?
                     (char? c2)
                     (or (char=? c2 #\b) (char=? c2 #\B)))
                (advance!)  ;; consume 'b'/'B'
                (let ([digits (read-while binary-or-sep?)])
                  (make-token 'number
                              (call-with-string-output-port
                                (lambda (out)
                                  (put-string out "0b")
                                  (put-string out digits)))
                              (get-location)))]

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
                                        (get-location))))
                        ;; Integer (octal or zero)
                        (make-token 'number
                                    (call-with-string-output-port
                                      (lambda (out)
                                        (put-char out #\0)
                                        (put-string out rest)))
                                    (get-location)))))]))]

          ;; Decimal number
          [else
           (let ([digits (read-while digit-or-sep?)])
             (let ([c2 (peek)])
               (if (and (char? c2) (char=? c2 #\.))
                   ;; Float
                   (begin
                     (advance!)  ;; consume '.'
                     (let ([frac (read-while digit-or-sep?)])
                       ;; Check for exponent (C++ or if config allows)
                       (let ([c3 (peek)])
                         (if (and supports-exponent?
                                  (char? c3)
                                  (or (char=? c3 #\e) (char=? c3 #\E)))
                             ;; Scientific notation
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
                                             (get-location))))
                             ;; No exponent
                             (make-token 'number
                                         (call-with-string-output-port
                                           (lambda (out)
                                             (put-string out digits)
                                             (put-char out #\.)
                                             (put-string out frac)))
                                         (get-location))))))
                   ;; Integer
                   (make-token 'number digits (get-location)))))])))

    ;; Tokenize string literal
    (define (tokenize-string-literal)
      (capture-location!)
      (advance!)  ;; consume opening "
      (let ([str (call-with-string-output-port
                   (lambda (out)
                     (let loop ()
                       (let ([c (peek)])
                         (cond
                           [(eof-object? c)
                            (perform (make-effect 'lexer-error
                              (cons "Unterminated string literal" (get-location))))]
                           [(char=? c #\")
                            (advance!)  ;; consume closing "
                            #f]  ;; done
                           [(char=? c #\\)
                            (put-char out (advance!))  ;; put backslash
                            (let ([escaped (advance!)])
                              (if (eof-object? escaped)
                                  (perform (make-effect 'lexer-error
                                    (cons "Unterminated escape sequence" (get-location))))
                                  (begin
                                    (put-char out escaped)
                                    (loop))))]
                           [else
                            (put-char out (advance!))
                            (loop)])))))])
        (when (> (string-length str) max-token-length)
          (perform (make-effect 'lexer-error
            (cons (format "String literal too long: ~a chars" (string-length str))
                  (get-location)))))
        (make-token 'string str (get-location))))

    ;; Tokenize character literal
    (define (tokenize-char-literal)
      (capture-location!)
      (advance!)  ;; consume opening '
      (let ([str (call-with-string-output-port
                   (lambda (out)
                     (let ([c (advance!)])
                       (cond
                         [(eof-object? c)
                          (perform (make-effect 'lexer-error
                            (cons "Unterminated char literal" (get-location))))]
                         [(char=? c #\\)
                          ;; Escape sequence
                          (put-char out c)
                          (let ([escaped (advance!)])
                            (cond
                              [(eof-object? escaped)
                               (perform (make-effect 'lexer-error
                                 (cons "Unterminated escape sequence" (get-location))))]
                              ;; Hex escape: \xHH
                              [(and (char? escaped) (or (char=? escaped #\x) (char=? escaped #\X)))
                               (put-char out escaped)
                               ;; Read hex digits
                               (let loop ()
                                 (let ([c (peek)])
                                   (when (and (char? c) (c-hex-digit? c))
                                     (put-char out (advance!))
                                     (loop))))]
                              ;; Octal escape: \OOO (up to 3 octal digits)
                              [(and (char? escaped) (char>=? escaped #\0) (char<=? escaped #\7))
                               (put-char out escaped)
                               ;; Read up to 2 more octal digits
                               (let loop ([count 1])
                                 (when (< count 3)
                                   (let ([c (peek)])
                                     (when (and (char? c) (char>=? c #\0) (char<=? c #\7))
                                       (put-char out (advance!))
                                       (loop (+ count 1))))))]
                              ;; Single character escape
                              [else
                               (put-char out escaped)]))]
                         [else
                          (put-char out c)]))))])
        (let ([close (advance!)])
          (if (and (char? close) (char=? close #\'))
              (make-token 'char str (get-location))
              (begin
                (perform (make-effect 'lexer-error
                  (cons "Unterminated char literal" (get-location))))
                (make-token 'char str (get-location)))))))

    ;; Tokenize line comment (// ...)
    (define (tokenize-line-comment)
      (capture-location!)
      (advance!)  ;; consume first /
      (advance!)  ;; consume second /
      (let ([text (call-with-string-output-port
                    (lambda (out)
                      (let loop ()
                        (let ([c (peek)])
                          (unless (or (eof-object? c) (char=? c #\newline))
                            (put-char out (advance!))
                            (loop))))))])
        (add-trivia! 'comment text (get-location))))

    ;; Tokenize block comment (/* ... */)
    (define (tokenize-block-comment)
      (capture-location!)
      (advance!)  ;; consume /
      (advance!)  ;; consume *
      (let ([text (call-with-string-output-port
                    (lambda (out)
                      (let loop ()
                        (let ([c (advance!)])
                          (cond
                            [(eof-object? c)
                             (perform (make-effect 'lexer-error
                               (cons "Unterminated block comment" (get-location))))]
                            [(char=? c #\*)
                             (let ([c2 (peek)])
                               (if (and (char? c2) (char=? c2 #\/))
                                   (begin (advance!) #f)  ;; done
                                   (begin (put-char out c) (loop))))]
                            [else
                             (put-char out c)
                             (loop)])))))])
        (add-trivia! 'comment text (get-location))))

    ;; Tokenize raw string literal R"delim(...)delim" (C++ only)
    ;; Note: caller has already consumed 'R', we start at '"'
    (define (tokenize-raw-string)
      (advance!)  ;; consume '"'
      ;; Read delimiter (chars before '(')
      (let ([delim (call-with-string-output-port
                     (lambda (out)
                       (let loop ()
                         (let ([c (peek)])
                           (cond
                             [(eof-object? c)
                              (perform (make-effect 'lexer-error
                                (cons "Unterminated raw string" (get-location))))]
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
                                (perform (make-effect 'lexer-error
                                  (cons "Unterminated raw string" (get-location))))]
                               [(char=? c #\))
                                ;; Check if this is the end marker
                                (advance!)  ;; consume the )
                                (let check ([i 1] [chars (list #\))])
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
            (make-token 'string str (get-location))))))

    ;; Tokenize punctuator (handles multi-char operators)
    (define (tokenize-punctuator)
      (capture-location!)
      (let ([c1 (advance!)])
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
                           (make-token 'punctuator "..." (get-location)))
                         ;; Just ".." - not standard but return it
                         (make-token 'punctuator ".." (get-location))))]

                  ;; Check for three-char operators in config
                  [else
                   (let ([c3 (peek)])
                     (if (char? c3)
                         (let ([three-str (call-with-string-output-port
                                            (lambda (out)
                                              (put-string out two-str)
                                              (put-char out c3)))])
                           (if (member three-str three-char-ops)
                               (begin
                                 (advance!)
                                 (advance!)
                                 (make-token 'punctuator three-str (get-location)))
                               ;; Not three-char, check two-char
                               (if (member two-str two-char-ops)
                                   (begin
                                     (advance!)
                                     (make-token 'punctuator two-str (get-location)))
                                   ;; Single char
                                   (make-token 'punctuator (string c1) (get-location)))))
                         ;; No c3, check two-char
                         (if (member two-str two-char-ops)
                             (begin
                               (advance!)
                               (make-token 'punctuator two-str (get-location)))
                             ;; Single char
                             (make-token 'punctuator (string c1) (get-location)))))]))
              ;; EOF after first char
              (make-token 'punctuator (string c1) (get-location))))))

    ;;=======================================================================
    ;; Main Tokenization Loop

    (define (tokenize-all)
      (let loop ([cst-nodes '()])
        (let skip-ws ()
          (let ([c (peek)])
            (when (and (char? c) (c-whitespace? c))
              (tokenize-whitespace!)
              (skip-ws))))

        (let ([c (peek)])
          (cond
            ;; EOF
            [(eof-object? c)
             (capture-location!)
             (set! token-count (+ token-count 1))
             (perform (make-effect 'check-limit
               (list 'token-count token-count max-tokens-per-file (get-location))))
             (let* ([eof-token (make-token 'eof #f (get-location))]
                    [eof-cst (make-cst-node eof-token (consume-trivia!) '())])
               (reverse (cons eof-cst cst-nodes)))]

            ;; Preprocessor directive
            [(char=? c #\#)
             (capture-location!)
             (advance!)
             (let ([tok (make-token 'directive "#" (get-location))])
               (loop (cons (make-cst-with-trivia tok) cst-nodes)))]

            ;; Identifier, keyword, or raw string literal (C++)
            [(c-identifier-start? c)
             ;; Check for raw string: R"..."
             (if (and supports-raw-strings? (char=? c #\R))
                 (let ()
                   (capture-location!)
                   (advance!)  ;; consume R
                   (let ([c2 (peek)])
                     (if (and (char? c2) (char=? c2 #\"))
                         ;; Raw string - restore position is handled by capture
                         (let ([tok (tokenize-raw-string)])
                           (loop (cons (make-cst-with-trivia tok) cst-nodes)))
                         ;; Just identifier starting with R - re-read
                         (begin
                           ;; We already consumed 'R', read rest of identifier
                           (let ([rest (read-while c-identifier-char?)])
                             (let* ([full-str (call-with-string-output-port
                                                (lambda (out)
                                                  (put-char out #\R)
                                                  (put-string out rest)))]
                                    [sym (string->symbol full-str)]
                                    [tok (if (memq sym keywords)
                                             (make-token 'keyword sym (get-location))
                                             (make-token 'identifier sym (get-location)))])
                               (loop (cons (make-cst-with-trivia tok) cst-nodes))))))))
                 ;; Regular identifier
                 (let ([tok (tokenize-identifier)])
                   (loop (cons (make-cst-with-trivia tok) cst-nodes))))]

            ;; Number
            [(c-digit? c)
             (let ([tok (tokenize-number)])
               (loop (cons (make-cst-with-trivia tok) cst-nodes)))]

            ;; String literal
            [(char=? c #\")
             (let ([tok (tokenize-string-literal)])
               (loop (cons (make-cst-with-trivia tok) cst-nodes)))]

            ;; Char literal
            [(char=? c #\')
             (let ([tok (tokenize-char-literal)])
               (loop (cons (make-cst-with-trivia tok) cst-nodes)))]

            ;; Comment or division
            [(char=? c #\/)
             (capture-location!)
             (advance!)  ;; consume first /
             (let ([c2 (peek)])
               (cond
                 [(and (char? c2) (char=? c2 #\/))
                  ;; Line comment
                  (tokenize-line-comment)
                  (loop cst-nodes)]

                 [(and (char? c2) (char=? c2 #\*))
                  ;; Block comment
                  (tokenize-block-comment)
                  (loop cst-nodes)]

                 ;; Check for /=
                 [(and (char? c2) (char=? c2 #\=))
                  (advance!)
                  (let ([tok (make-token 'punctuator "/=" (get-location))])
                    (loop (cons (make-cst-with-trivia tok) cst-nodes)))]

                 ;; Just / operator
                 [else
                  (let ([tok (make-token 'punctuator "/" (get-location))])
                    (loop (cons (make-cst-with-trivia tok) cst-nodes)))]))]

            ;; Punctuator
            [else
             (let ([tok (tokenize-punctuator)])
               (loop (cons (make-cst-with-trivia tok) cst-nodes)))]))))

    ;; Entry point
    (tokenize-all)))
