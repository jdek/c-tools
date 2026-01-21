;; SPDX-License-Identifier: WTFPL
;; Common utility functions used across codegen modules

(library (c-tools utility)
  (export string-join
          filter-map
          symbol-append
          extract-exports
          format
          box
          unbox
          set-box!
          last-pair
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
          (rnrs records syntactic))

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
