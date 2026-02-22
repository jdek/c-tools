;; Fuzzing Mutation Engine - 35+ mutation strategies
;; Applies various mutations to test inputs for fuzzing (byte/data-level mutations)

(library (c-tools testing fuzzing mutation)
  (export
    ;; Main API
    mutate-input              ;; string seed mutation-type => string
    mutate-corpus             ;; corpus seed config => mutated-corpus

    ;; Configuration
    make-mutation-config
    mutation-config?
    mutation-config-rate
    mutation-config-strategies
    mutation-config-max-mutations

    ;; Mutation strategies (35+ types)
    ;; Character mutations
    char-insert
    char-delete
    char-replace
    char-flip
    char-swap

    ;; Structural mutations
    duplicate-chunk
    remove-chunk
    swap-chunks
    reverse-chunk
    truncate-start
    truncate-end
    truncate-middle

    ;; Repeating mutations
    repeat-char
    repeat-chunk
    triple-repeat

    ;; Boundary mutations
    insert-null
    insert-boundary-value
    insert-max-value
    insert-negative

    ;; Encoding mutations
    uppercase
    lowercase
    mixedcase
    insert-unicode
    insert-control-char
    insert-escape-sequence

    ;; Whitespace mutations
    add-whitespace
    remove-whitespace
    replace-with-whitespace
    insert-newlines
    insert-tabs

    ;; Special character mutations
    insert-special-chars
    insert-format-strings
    insert-path-traversal
    insert-sql-chars
    insert-script-tags

    ;; Numeric mutations
    flip-sign
    increment-number
    decrement-number
    multiply-number
    insert-overflow

    ;; C-specific mutations
    insert-c-keywords
    insert-preprocessor-directive
    insert-unmatched-brace
    insert-pointer-stars
    insert-array-brackets

    ;; Get all mutations
    all-mutations
    c-specific-mutations)

  (import (rnrs base)
          (rnrs records syntactic)
          (rnrs lists)
          (rnrs unicode)
          (only (chezscheme) random random-seed fxand fxior fxxor fxsll fxsrl make-list))

  ;;=======================================================================
  ;; Mutation configuration

  (define-record-type mutation-config
    (fields
      (immutable rate)           ;; Mutation probability 0.0-1.0
      (immutable strategies)     ;; List of mutation procedures to use
      (immutable max-mutations))) ;; Max mutations per input

  ;;=======================================================================
  ;; Random helpers

  (define (random-boolean)
    (= (random 2) 0))

  (define (random-pick lst)
    (if (null? lst)
        (error 'random-pick "Cannot pick from empty list")
        (list-ref lst (random (length lst)))))

  (define (random-char)
    (integer->char (+ 32 (random 95))))  ;; Printable ASCII

  ;; For accessing existing characters (string-ref, char at position)
  ;; Returns valid index: 0 to length-1
  (define (random-index str)
    (if (= (string-length str) 0)
        (error 'random-index "Cannot get index from empty string")
        (random (string-length str))))

  ;; For insertion positions (between characters)
  ;; Returns valid position: 0 to length (inclusive)
  (define (random-position str)
    (random (+ (string-length str) 1)))

  (define (random-range str)
    (let* ([len (string-length str)]
           [start (random (+ len 1))]
           [end (+ start (random (+ 1 (- len start))))])
      (cons start end)))

  ;;=======================================================================
  ;; Main mutation functions

  ;; mutate-input : string fixnum procedure => string
  ;;   Sets random seed for deterministic mutation.
  (define (mutate-input input seed mutation-type)
    (random-seed seed)
    (mutation-type input))

  ;; mutate-corpus : list fixnum mutation-config => list
  ;;   Mutates entire corpus with given configuration.
  (define (mutate-corpus corpus seed config)
    (random-seed seed)
    (let ([strategies (mutation-config-strategies config)]
          [max-muts (mutation-config-max-mutations config)]
          [rate (mutation-config-rate config)])
      (map (lambda (input)
             (if (< (random 100) (* rate 100))
                 (apply-n-mutations input (+ 1 (random max-muts)) strategies)
                 input))
           corpus)))

  ;; apply-n-mutations : string fixnum list => string
  ;;   Applies N random mutations from strategy list.
  (define (apply-n-mutations input n strategies)
    (if (<= n 0)
        input
        (let ([mutated ((random-pick strategies) input)])
          (apply-n-mutations mutated (- n 1) strategies))))

  ;;=======================================================================
  ;; CHARACTER MUTATIONS

  ;; char-insert : string => string
  ;;   Inserts random character at random position.
  (define (char-insert str)
    (if (= (string-length str) 0)
        (string (random-char))
        (let* ([pos (random-position str)]
               [prefix (substring str 0 pos)]
               [suffix (substring str pos (string-length str))])
          (string-append prefix (string (random-char)) suffix))))

  ;; char-delete : string => string
  ;;   Deletes character at random position.
  (define (char-delete str)
    (if (= (string-length str) 0)
        str
        (let ([pos (random-index str)])
          (string-append (substring str 0 pos)
                        (if (< (+ pos 1) (string-length str))
                            (substring str (+ pos 1) (string-length str))
                            "")))))

  ;; char-replace : string => string
  ;;   Replaces character at random position.
  (define (char-replace str)
    (if (= (string-length str) 0)
        str
        (let ([pos (random-index str)])
          (string-append (substring str 0 pos)
                        (string (random-char))
                        (if (< (+ pos 1) (string-length str))
                            (substring str (+ pos 1) (string-length str))
                            "")))))

  ;; char-flip : string => string
  ;;   Flips bit in random character.
  (define (char-flip str)
    (if (= (string-length str) 0)
        str
        (let* ([pos (random-index str)]
               [ch (string-ref str pos)]
               [code (char->integer ch)]
               [bit (random 7)]
               [flipped (fxxor code (fxsll 1 bit))]
               [new-char (integer->char (fxand flipped 127))])
          (string-append (substring str 0 pos)
                        (string new-char)
                        (if (< (+ pos 1) (string-length str))
                            (substring str (+ pos 1) (string-length str))
                            "")))))

  ;; char-swap : string => string
  ;;   Swaps two adjacent characters.
  (define (char-swap str)
    (if (< (string-length str) 2)
        str
        (let* ([pos (random (- (string-length str) 1))]
               [c1 (string-ref str pos)]
               [c2 (string-ref str (+ pos 1))])
          (string-append (substring str 0 pos)
                        (string c2 c1)
                        (substring str (+ pos 2) (string-length str))))))

  ;;=======================================================================
  ;; STRUCTURAL MUTATIONS

  ;; duplicate-chunk : string => string
  ;;   Duplicates random chunk.
  (define (duplicate-chunk str)
    (if (= (string-length str) 0)
        str
        (let* ([range (random-range str)]
               [start (car range)]
               [end (cdr range)]
               [chunk (substring str start end)])
          (string-append (substring str 0 end)
                        chunk
                        (substring str end (string-length str))))))

  ;; remove-chunk : string => string
  ;;   Removes random chunk.
  (define (remove-chunk str)
    (if (= (string-length str) 0)
        str
        (let* ([range (random-range str)]
               [start (car range)]
               [end (cdr range)])
          (string-append (substring str 0 start)
                        (substring str end (string-length str))))))

  ;; swap-chunks : string => string
  ;;   Swaps two random chunks.
  (define (swap-chunks str)
    (if (< (string-length str) 2)
        str
        (let* ([len (string-length str)]
               [pos1 (random len)]
               [pos2 (random len)]
               [start (min pos1 pos2)]
               [end (max pos1 pos2)]
               [mid (div (+ start end) 2)])
          (string-append (substring str 0 start)
                        (substring str mid end)
                        (substring str start mid)
                        (substring str end len)))))

  ;; reverse-chunk : string => string
  ;;   Reverses random chunk.
  (define (reverse-chunk str)
    (if (= (string-length str) 0)
        str
        (let* ([range (random-range str)]
               [start (car range)]
               [end (cdr range)]
               [chunk (substring str start end)]
               [reversed (list->string (reverse (string->list chunk)))])
          (string-append (substring str 0 start)
                        reversed
                        (substring str end (string-length str))))))

  ;; truncate-start : string => string
  ;;   Truncates from start.
  (define (truncate-start str)
    (if (= (string-length str) 0)
        str
        (let ([cut (random (string-length str))])
          (substring str cut (string-length str)))))

  ;; truncate-end : string => string
  ;;   Truncates from end.
  (define (truncate-end str)
    (if (= (string-length str) 0)
        str
        (let ([cut (random (string-length str))])
          (substring str 0 cut))))

  ;; truncate-middle : string => string
  ;;   Truncates from middle.
  (define (truncate-middle str)
    (if (< (string-length str) 2)
        str
        (let* ([len (string-length str)]
               [start (div len 4)]
               [end (div (* 3 len) 4)])
          (string-append (substring str 0 start)
                        (substring str end len)))))

  ;;=======================================================================
  ;; REPEATING MUTATIONS

  ;; repeat-char : string => string
  ;;   Repeats random character multiple times.
  (define (repeat-char str)
    (if (= (string-length str) 0)
        "AAAA"
        (let* ([pos (random-index str)]
               [ch (string-ref str pos)]
               [count (+ 2 (random 8))]
               [repeated (make-string count ch)])
          (string-append (substring str 0 pos)
                        repeated
                        (substring str pos (string-length str))))))

  ;; repeat-chunk : string => string
  ;;   Repeats random chunk.
  (define (repeat-chunk str)
    (if (= (string-length str) 0)
        str
        (let* ([range (random-range str)]
               [start (car range)]
               [end (cdr range)]
               [chunk (substring str start end)]
               [count (+ 2 (random 4))]
               [repeated (apply string-append (make-list count chunk))])
          (string-append (substring str 0 start)
                        repeated
                        (substring str end (string-length str))))))

  ;; triple-repeat : string => string
  ;;   Triples entire input.
  (define (triple-repeat str)
    (string-append str str str))

  ;;=======================================================================
  ;; BOUNDARY MUTATIONS

  ;; insert-null : string => string
  ;;   Inserts null byte.
  (define (insert-null str)
    (let ([pos (random-position str)])
      (string-append (substring str 0 pos)
                    (string #\nul)
                    (substring str pos (string-length str)))))

  ;; insert-boundary-value : string => string
  ;;   Inserts boundary value.
  (define (insert-boundary-value str)
    (let ([val (random-pick '("0" "-1" "255" "256" "65535" "65536"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    val
                    (substring str pos (string-length str)))))

  ;; insert-max-value : string => string
  ;;   Inserts maximum values.
  (define (insert-max-value str)
    (let ([val (random-pick '("2147483647" "4294967295" "9223372036854775807"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    val
                    (substring str pos (string-length str)))))

  ;; insert-negative : string => string
  ;;   Inserts negative values.
  (define (insert-negative str)
    (let ([val (random-pick '("-1" "-128" "-32768" "-2147483648"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    val
                    (substring str pos (string-length str)))))

  ;;=======================================================================
  ;; ENCODING MUTATIONS

  ;; uppercase : string => string
  ;;   Converts to uppercase.
  (define (uppercase str)
    (string-upcase str))

  ;; lowercase : string => string
  ;;   Converts to lowercase.
  (define (lowercase str)
    (string-downcase str))

  ;; mixedcase : string => string
  ;;   Converts to mixed case.
  (define (mixedcase str)
    (list->string
      (map (lambda (ch)
             (if (random-boolean)
                 (char-upcase ch)
                 (char-downcase ch)))
           (string->list str))))

  ;; insert-unicode : string => string
  ;;   Inserts unicode characters.
  (define (insert-unicode str)
    (let ([unicode (random-pick (list
                     (string (integer->char #x00E9))  ;; e with acute
                     (string (integer->char #x4E2D))  ;; Chinese character
                     (string (integer->char #x0041))  ;; A
                     (string (integer->char #xFFFD))  ;; replacement char
                     (string (integer->char #x200B))))] ;; Zero-width space
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    unicode
                    (substring str pos (string-length str)))))

  ;; insert-control-char : string => string
  ;;   Inserts control characters.
  (define (insert-control-char str)
    (let ([ctrl (random-pick (list #\nul #\alarm #\tab #\newline #\return #\esc))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    (string ctrl)
                    (substring str pos (string-length str)))))

  ;; insert-escape-sequence : string => string
  ;;   Inserts escape sequences.
  (define (insert-escape-sequence str)
    (let ([esc (random-pick '("\\n" "\\r" "\\t" "\\0" "\\\\" "\\\"" "\\'"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    esc
                    (substring str pos (string-length str)))))

  ;;=======================================================================
  ;; WHITESPACE MUTATIONS

  ;; add-whitespace : string => string
  ;;   Adds whitespace.
  (define (add-whitespace str)
    (let ([ws (random-pick (list #\space #\tab #\newline))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    (string ws)
                    (substring str pos (string-length str)))))

  ;; remove-whitespace : string => string
  ;;   Removes whitespace.
  (define (remove-whitespace str)
    (list->string
      (filter (lambda (ch) (not (char-whitespace? ch)))
              (string->list str))))

  ;; replace-with-whitespace : string => string
  ;;   Replaces characters with whitespace.
  (define (replace-with-whitespace str)
    (if (= (string-length str) 0)
        str
        (let ([pos (random (string-length str))])
          (string-append (substring str 0 pos)
                        " "
                        (substring str (+ pos 1) (string-length str))))))

  ;; insert-newlines : string => string
  ;;   Inserts multiple newlines.
  (define (insert-newlines str)
    (let ([newlines (make-string (+ 1 (random 5)) #\newline)]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    newlines
                    (substring str pos (string-length str)))))

  ;; insert-tabs : string => string
  ;;   Inserts tabs.
  (define (insert-tabs str)
    (let ([tabs (make-string (+ 1 (random 5)) #\tab)]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    tabs
                    (substring str pos (string-length str)))))

  ;;=======================================================================
  ;; SPECIAL CHARACTER MUTATIONS

  ;; insert-special-chars : string => string
  ;;   Inserts special characters.
  (define (insert-special-chars str)
    (let ([special (random-pick '("!@#$%^&*()" "<>?{}[]|" "~`" ";:'\",." "_-+="))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    special
                    (substring str pos (string-length str)))))

  ;; insert-format-strings : string => string
  ;;   Inserts format strings.
  (define (insert-format-strings str)
    (let ([fmt (random-pick '("%s" "%d" "%x" "%n" "%p" "~a" "~s"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    fmt
                    (substring str pos (string-length str)))))

  ;; insert-path-traversal : string => string
  ;;   Inserts path traversal sequences.
  (define (insert-path-traversal str)
    (let ([path (random-pick '("../" "../../" "../../../" "..\\" "..\\..\\'"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    path
                    (substring str pos (string-length str)))))

  ;; insert-sql-chars : string => string
  ;;   Inserts SQL special characters.
  (define (insert-sql-chars str)
    (let ([sql (random-pick '("'" "\"" "--" "/*" "*/" ";" "||" "AND" "OR"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    sql
                    (substring str pos (string-length str)))))

  ;; insert-script-tags : string => string
  ;;   Inserts script tags.
  (define (insert-script-tags str)
    (let ([tag (random-pick '("<script>" "</script>" "<img>" "javascript:" "onerror="))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    tag
                    (substring str pos (string-length str)))))

  ;;=======================================================================
  ;; NUMERIC MUTATIONS

  ;; flip-sign : string => string
  ;;   Flips sign of number.
  (define (flip-sign str)
    (if (and (> (string-length str) 0)
             (char-numeric? (string-ref str 0)))
        (string-append "-" str)
        str))

  ;; increment-number : string => string
  ;;   Increments number in string.
  (define (increment-number str)
    (if (string->number str)
        (number->string (+ (string->number str) 1))
        str))

  ;; decrement-number : string => string
  ;;   Decrements number in string.
  (define (decrement-number str)
    (if (string->number str)
        (number->string (- (string->number str) 1))
        str))

  ;; multiply-number : string => string
  ;;   Multiplies number.
  (define (multiply-number str)
    (if (string->number str)
        (number->string (* (string->number str) (random-pick '(2 10 100 1000))))
        str))

  ;; insert-overflow : string => string
  ;;   Inserts overflow value.
  (define (insert-overflow str)
    (let ([overflow (random-pick '("999999999999999999" "-999999999999999999" "1e308"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    overflow
                    (substring str pos (string-length str)))))

  ;;=======================================================================
  ;; C-SPECIFIC MUTATIONS

  ;; insert-c-keywords : string => string
  ;;   Inserts C keywords.
  (define (insert-c-keywords str)
    (let ([keyword (random-pick '("int" "char" "void" "struct" "enum" "typedef"
                                  "static" "const" "volatile" "unsigned" "signed"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    keyword
                    (substring str pos (string-length str)))))

  ;; insert-preprocessor-directive : string => string
  ;;   Inserts preprocessor directives.
  (define (insert-preprocessor-directive str)
    (let ([directive (random-pick '("#include" "#define" "#ifdef" "#endif"
                                    "#ifndef" "#pragma" "#error"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    directive
                    (substring str pos (string-length str)))))

  ;; insert-unmatched-brace : string => string
  ;;   Inserts unmatched braces/brackets.
  (define (insert-unmatched-brace str)
    (let ([brace (random-pick '("{" "}" "(" ")" "[" "]" "<" ">"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    brace
                    (substring str pos (string-length str)))))

  ;; insert-pointer-stars : string => string
  ;;   Inserts pointer stars.
  (define (insert-pointer-stars str)
    (let ([stars (random-pick '("*" "**" "***" "****"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    stars
                    (substring str pos (string-length str)))))

  ;; insert-array-brackets : string => string
  ;;   Inserts array bracket notation.
  (define (insert-array-brackets str)
    (let ([brackets (random-pick '("[]" "[0]" "[1]" "[10]" "[100]" "[-1]"))]
          [pos (random-position str)])
      (string-append (substring str 0 pos)
                    brackets
                    (substring str pos (string-length str)))))

  ;;=======================================================================
  ;; Mutation Lists

  ;; c-specific-mutations : list
  ;;   C-specific mutation strategies.
  (define c-specific-mutations
    (list insert-c-keywords
          insert-preprocessor-directive
          insert-unmatched-brace
          insert-pointer-stars
          insert-array-brackets))

  ;; all-mutations : list
  ;;   All mutation strategies.
  (define all-mutations
    (list
      ;; Character (5)
      char-insert char-delete char-replace char-flip char-swap
      ;; Structural (7)
      duplicate-chunk remove-chunk swap-chunks reverse-chunk
      truncate-start truncate-end truncate-middle
      ;; Repeating (3)
      repeat-char repeat-chunk triple-repeat
      ;; Boundary (4)
      insert-null insert-boundary-value insert-max-value insert-negative
      ;; Encoding (6)
      uppercase lowercase mixedcase insert-unicode
      insert-control-char insert-escape-sequence
      ;; Whitespace (5)
      add-whitespace remove-whitespace replace-with-whitespace
      insert-newlines insert-tabs
      ;; Special (5)
      insert-special-chars insert-format-strings insert-path-traversal
      insert-sql-chars insert-script-tags
      ;; Numeric (5)
      flip-sign increment-number decrement-number multiply-number insert-overflow
      ;; C-specific (5)
      insert-c-keywords insert-preprocessor-directive insert-unmatched-brace
      insert-pointer-stars insert-array-brackets))

) ;; end library
