;; SPDX-License-Identifier: WTFPL
;; Common utility functions used across codegen modules

(library (c-tools utility)
  (export string-join
          string-split
          string-split-char
          string-trim
          string-trim-left
          string-trim-right
          string-prefix?
          string-suffix?
          string-contains?
          string-index
          string-replace
          string-search
          filter-map
          symbol-append
          extract-exports
          format
          box
          unbox
          set-box!
          last-pair
          ormap
          make-parameter
          make-mutex
          with-mutex
          hashtable-values
          pretty-print
          void)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (rnrs lists)
          (rnrs io ports)
          (rnrs hashtables)
          (rnrs records syntactic)
          (rnrs unicode))

  ;;=======================================================================
  ;; String Utilities

  ;; string-join : list-of-string string => string
  ;;   Joins strings with separator.
  (define (string-join strs sep)
    (if (null? strs)
        ""
        (let loop ([strs (cdr strs)] [result (car strs)])
          (if (null? strs)
              result
              (loop (cdr strs) (string-append result sep (car strs)))))))

  ;; string-split-char : string char => list-of-string
  ;;   Splits string on delimiter character.
  (define (string-split-char str delim)
    (let ([len (string-length str)])
      (if (= len 0)
          '("")
          (let loop ([start 0] [i 0] [acc '()])
            (cond
              [(>= i len)
               (reverse (cons (substring str start i) acc))]
              [(char=? (string-ref str i) delim)
               (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
              [else
               (loop start (+ i 1) acc)])))))

  ;; string-split : string string => list-of-string
  ;;   Splits string by separator string.
  (define (string-split str sep)
    (if (string=? sep "")
        (list str)
        (let ([sep-len (string-length sep)])
          (let loop ([start 0] [parts '()])
            (let ([pos (string-search str sep start)])
              (if pos
                  (loop (+ pos sep-len)
                        (cons (substring str start pos) parts))
                  (reverse (cons (substring str start (string-length str)) parts))))))))

  ;; string-trim-left : string => string
  ;;   Trims leading whitespace.
  (define (string-trim-left str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) ""]
          [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
          [else (substring str i len)]))))

  ;; string-trim-right : string => string
  ;;   Trims trailing whitespace.
  (define (string-trim-right str)
    (let ([len (string-length str)])
      (let loop ([i (- len 1)])
        (cond
          [(< i 0) ""]
          [(char-whitespace? (string-ref str i)) (loop (- i 1))]
          [else (substring str 0 (+ i 1))]))))

  ;; string-trim : string => string
  ;;   Trims leading and trailing whitespace.
  (define (string-trim str)
    (let* ([len (string-length str)]
           [start (let loop ([i 0])
                    (cond
                      [(>= i len) i]
                      [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
                      [else i]))]
           [end (let loop ([i (- len 1)])
                  (cond
                    [(< i 0) 0]
                    [(char-whitespace? (string-ref str i)) (loop (- i 1))]
                    [else (+ i 1)]))])
      (if (> start end)
          ""
          (substring str start end))))

  ;; string-prefix? : string string => boolean
  ;;   Checks if string starts with prefix.
  (define (string-prefix? str prefix)
    (let ([slen (string-length str)]
          [plen (string-length prefix)])
      (and (>= slen plen)
           (string=? (substring str 0 plen) prefix))))

  ;; string-suffix? : string string => boolean
  ;;   Checks if string ends with suffix.
  (define (string-suffix? str suffix)
    (let ([slen (string-length str)]
          [suflen (string-length suffix)])
      (and (>= slen suflen)
           (string=? (substring str (- slen suflen) slen) suffix))))

  ;; string-index : string char => fixnum | #f
  ;;   Finds first occurrence of character in string.
  (define (string-index str ch)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))

  ;; string-contains? : string string => boolean
  ;;   Checks if string contains substring.
  (define (string-contains? str substr)
    (and (string-search str substr 0) #t))

  ;; string-search : string string fixnum => fixnum | #f
  ;;   Finds position of needle in haystack starting at start.
  (define (string-search haystack needle start)
    (define (string-prefix-at? str prefix pos)
      (let ([prefix-len (string-length prefix)])
        (and (<= (+ pos prefix-len) (string-length str))
             (let loop ([i 0])
               (if (>= i prefix-len)
                   #t
                   (and (char=? (string-ref str (+ pos i))
                               (string-ref prefix i))
                        (loop (+ i 1))))))))
    (let ([needle-len (string-length needle)]
          [hay-len (string-length haystack)])
      (let loop ([pos start])
        (if (> (+ pos needle-len) hay-len)
            #f
            (if (string-prefix-at? haystack needle pos)
                pos
                (loop (+ pos 1)))))))

  ;; string-replace : string string string => string
  ;;   Replaces all occurrences of 'from' substring with 'to' substring.
  (define (string-replace str from to)
    (let ([from-len (string-length from)]
          [result '()])
      (let loop ([i 0] [start 0])
        (cond
          [(>= i (string-length str))
           (if (= start i)
               (apply string-append (reverse result))
               (apply string-append (reverse (cons (substring str start i) result))))]
          [(and (<= (+ i from-len) (string-length str))
                (string=? (substring str i (+ i from-len)) from))
           (set! result (cons to (cons (substring str start i) result)))
           (loop (+ i from-len) (+ i from-len))]
          [else
           (loop (+ i 1) start)]))))

  ;;=======================================================================
  ;; Symbol Utilities

  ;; symbol-append : symbol ... => symbol
  ;;   Concatenates symbols into a single symbol.
  (define (symbol-append . syms)
    (string->symbol
      (apply string-append (map symbol->string syms))))

  ;;=======================================================================
  ;; List Utilities

  ;; filter-map : (a => b | #f) list-of-a => list-of-b
  ;;   Maps procedure over list, filtering out #f results.
  (define (filter-map proc lst)
    (let loop ([lst lst] [result '()])
      (cond
        [(null? lst) (reverse result)]
        [else
         (let ([val (proc (car lst))])
           (if val
               (loop (cdr lst) (cons val result))
               (loop (cdr lst) result)))])))

  ;;=======================================================================
  ;; FFI Form Utilities

  ;; extract-exports : list-of-form => list-of-symbol
  ;;   Extracts export names from FFI forms.
  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              [(and (pair? form) (eq? (car form) 'define))
               (loop (cdr forms) (cons (cadr form) exports))]
              [(and (pair? form) (eq? (car form) 'begin))
               ;; Extract from begin block (enums)
               (loop (append (cdr form) (cdr forms)) exports)]
              [(and (pair? form) (eq? (car form) 'comment))
               (loop (cdr forms) exports)]
              [else
               (loop (cdr forms) exports)])))))

  ;;=======================================================================
  ;; String Formatting

  ;; format : string any ... => string
  ;;   Simple format implementation supporting ~a, ~s, and ~%
  (define (format fmt . args)
    (call-with-string-output-port
      (lambda (port)
        (let loop ([i 0] [args args])
          (if (< i (string-length fmt))
              (let ([c (string-ref fmt i)])
                (cond
                  [(and (char=? c #\~)
                        (< (+ i 1) (string-length fmt)))
                   (let ([directive (string-ref fmt (+ i 1))])
                     (case directive
                       [(#\a)
                        ;; ~a - display representation
                        (when (pair? args)
                          (put-string port (format-value (car args)))
                          (loop (+ i 2) (cdr args)))]
                       [(#\s)
                        ;; ~s - write representation
                        (when (pair? args)
                          (write-value (car args) port)
                          (loop (+ i 2) (cdr args)))]
                       [(#\%)
                        ;; ~% - newline
                        (put-char port #\newline)
                        (loop (+ i 2) args)]
                       [else
                        ;; Unknown directive - output literal
                        (put-char port c)
                        (loop (+ i 1) args)]))]
                  [else
                   (put-char port c)
                   (loop (+ i 1) args)]))
              #f)))))

  ;; format-value : any => string
  ;;   Converts value to display representation
  (define (format-value val)
    (cond
      [(string? val) val]
      [(symbol? val) (symbol->string val)]
      [(number? val) (number->string val)]
      [(boolean? val) (if val "#t" "#f")]
      [(null? val) "()"]
      [(pair? val)
       (call-with-string-output-port
         (lambda (port)
           (put-char port #\()
           (let loop ([lst val] [first? #t])
             (cond
               [(null? lst)
                (put-char port #\))]
               [(pair? lst)
                (unless first? (put-char port #\space))
                (put-string port (format-value (car lst)))
                (loop (cdr lst) #f)]
               [else
                (put-string port " . ")
                (put-string port (format-value lst))
                (put-char port #\))]))))]
      [else "#<unknown>"]))

  ;; write-value : any output-port => void
  ;;   Writes value in write representation
  (define (write-value val port)
    (cond
      [(string? val)
       (put-char port #\")
       (let loop ([i 0])
         (when (< i (string-length val))
           (let ([c (string-ref val i)])
             (case c
               [(#\") (put-string port "\\\"")]
               [(#\\) (put-string port "\\\\")]
               [(#\newline) (put-string port "\\n")]
               [(#\return) (put-string port "\\r")]
               [(#\tab) (put-string port "\\t")]
               [else (put-char port c)]))
           (loop (+ i 1))))
       (put-char port #\")]
      [else
       (put-string port (format-value val))]))

  ;;=======================================================================
  ;; Box Type (mutable cell)

  (define-record-type box-type
    (fields (mutable value))
    (protocol
      (lambda (new)
        (lambda (val)
          (new val)))))

  ;; box : any => box
  ;;   Creates a mutable box containing a value.
  (define box make-box-type)

  ;; unbox : box => any
  ;;   Retrieves the value from a box.
  (define unbox box-type-value)

  ;; set-box! : box any => void
  ;;   Sets the value in a box.
  (define set-box! box-type-value-set!)

  ;;=======================================================================
  ;; List Utilities (Extended)

  ;; last-pair : pair => pair
  ;;   Returns the last pair in a list.
  (define (last-pair lst)
    (if (pair? (cdr lst))
        (last-pair (cdr lst))
        lst))

  ;; ormap : (a => bool) list-of-a => bool
  ;;   Returns #t if predicate is true for any element.
  (define (ormap proc lst)
    (and (pair? lst)
         (or (proc (car lst))
             (ormap proc (cdr lst)))))

  ;;=======================================================================
  ;; Parameter Type (dynamic variables)

  ;; make-parameter : any [procedure] => parameter
  ;;   Creates a parameter object with optional converter.
  (define make-parameter
    (case-lambda
      [(init)
       (let ([value (box init)])
         (lambda args
           (if (null? args)
               (unbox value)
               (begin
                 (set-box! value (car args))
                 (unbox value)))))]
      [(init converter)
       (let ([value (box (converter init))])
         (lambda args
           (if (null? args)
               (unbox value)
               (begin
                 (set-box! value (converter (car args)))
                 (unbox value)))))]))

  ;;=======================================================================
  ;; Mutex Type (no-op for single-threaded R6RS)

  (define-record-type mutex-type
    (fields)
    (protocol
      (lambda (new)
        (lambda ()
          (new)))))

  ;; make-mutex : => mutex
  ;;   Creates a mutex (no-op in single-threaded context).
  (define make-mutex make-mutex-type)

  ;; with-mutex : mutex thunk => any
  ;;   Executes thunk with mutex held (no-op in single-threaded context).
  (define (with-mutex mutex thunk)
    (thunk))

  ;;=======================================================================
  ;; Hashtable Utilities

  ;; hashtable-values : hashtable => list
  ;;   Returns a list of all values in the hashtable.
  (define (hashtable-values ht)
    (let-values ([(keys vals) (hashtable-entries ht)])
      (vector->list vals)))

  ;;=======================================================================
  ;; Pretty Printing

  ;; pretty-print : any [output-port] => void
  ;;   Pretty prints a value to a port or current output.
  (define pretty-print
    (case-lambda
      [(obj)
       (pretty-print obj (current-output-port))]
      [(obj port)
       (put-string port (format-value obj))
       (newline port)]))

  ;;=======================================================================
  ;; Void

  ;; void : => unspecified
  ;;   Returns an unspecified value (we use #f).
  (define (void) #f)

  ) ;; end library
