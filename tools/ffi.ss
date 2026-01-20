#!/usr/bin/env scheme-script
;; FFI Binding Generator for C/C++ Headers
;; Generates Chez Scheme FFI bindings from C or C++ header files

(import (rnrs)
        (only (chezscheme) command-line-arguments getenv format pretty-print)
        (c-tools lexer c)
        (c-tools lexer cpp)
        (c-tools preprocess c)
        (c-tools preprocess cpp)
        (c-tools parser c)
        (c-tools parser cpp)
        (c-tools codegen ffi)
        (c-tools codegen cpp-ffi)
        (c-tools effects files)
        (c-tools effects cpp core)
        (c-tools effects cpp macros)
        (c-tools effects cpp includes)
        (c-tools effects cpp conditionals)
        (c-tools effects registry))

;;=======================================================================
;; Main FFI generation pipeline

(define (generate-c-ffi filename lib-name output-file)
  ;; C FFI generation pipeline
  (with-file-system #f "."
    (lambda ()
      (with-effects '((cpp-include ())
                      cpp-macros
                      cpp-conditional)
        (lambda ()
          (let* ([tokens (preprocess-file filename)]
                 [decls (parse-declarations tokens)]
                 [ffi-code (generate-ffi-code decls lib-name)])
            (if output-file
                (call-with-output-file output-file
                  (lambda (port)
                    (pretty-print ffi-code port)))
                (pretty-print ffi-code))))))))

(define (generate-cpp-ffi filename lib-name output-file)
  ;; C++ FFI generation pipeline
  (with-file-system #f "."
    (lambda ()
      (with-effects '((cpp-include ())
                      cpp-macros
                      cpp-conditional)
        (lambda ()
          (let* ([tokens (preprocess-cpp-file filename)]
                 [decls (parse-cpp-declarations tokens)]
                 [ffi-code (generate-cpp-ffi-code decls lib-name lib-name)])
            (if output-file
                (call-with-output-file output-file
                  (lambda (port)
                    (pretty-print ffi-code port)))
                (pretty-print ffi-code))))))))

;;=======================================================================
;; CLI argument parsing

(define (show-help)
  (display "Usage: ffi.ss [OPTIONS] HEADER_FILE\n")
  (display "\n")
  (display "Generate Chez Scheme FFI bindings from C/C++ headers\n")
  (display "\n")
  (display "Options:\n")
  (display "  -x LANG        Force language mode (c or c++)\n")
  (display "  -l LIBNAME     Library name for FFI bindings\n")
  (display "  -o FILE        Output file (default: stdout)\n")
  (display "  --help         Show this help message\n")
  (display "\n")
  (display "Examples:\n")
  (display "  ffi.ss mylib.h -l mylib -o bindings.sls\n")
  (display "  ffi.ss -x c++ myclass.hpp -l myclass\n")
  (newline))

(define (detect-language filename)
  ;; Detect C vs C++ from file extension
  (cond
    [(string-suffix? ".hpp" filename) 'c++]
    [(string-suffix? ".hxx" filename) 'c++]
    [(string-suffix? ".h++" filename) 'c++]
    [(string-suffix? ".cpp" filename) 'c++]
    [(string-suffix? ".cc" filename) 'c++]
    [(string-suffix? ".cxx" filename) 'c++]
    [else 'c]))

(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

(define (derive-lib-name filename)
  ;; Derive library name from filename
  (let* ([slash-pos (string-index-right filename #\/)]
         [name (if slash-pos
                   (substring filename (+ 1 slash-pos) (string-length filename))
                   filename)]
         [dot-pos (string-index name #\.)]
         [base (if dot-pos
                   (substring name 0 dot-pos)
                   name)])
    base))

(define (string-contains? str ch)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #f]
      [(char=? (string-ref str i) ch) #t]
      [else (loop (+ i 1))])))

(define (string-index str ch)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #f]
      [(char=? (string-ref str i) ch) i]
      [else (loop (+ i 1))])))

(define (string-index-right str ch)
  (let loop ([i (- (string-length str) 1)])
    (cond
      [(< i 0) #f]
      [(char=? (string-ref str i) ch) i]
      [else (loop (- i 1))])))

(define (parse-args args)
  ;; Parse command-line arguments
  (let loop ([args args]
             [lang #f]
             [lib-name #f]
             [output-file #f]
             [input-file #f])
    (cond
      [(null? args)
       (if input-file
           (list lang lib-name output-file input-file)
           (begin
             (display "Error: No input file specified\n")
             (show-help)
             (exit 1)))]
      [(string=? (car args) "--help")
       (show-help)
       (exit 0)]
      [(string=? (car args) "-x")
       (if (null? (cdr args))
           (begin
             (display "Error: -x requires language argument\n")
             (exit 1))
           (let ([lang-str (cadr args)])
             (loop (cddr args)
                   (cond
                     [(string=? lang-str "c") 'c]
                     [(string=? lang-str "c++") 'c++]
                     [(string=? lang-str "cpp") 'c++]
                     [else (begin
                             (display (format "Error: Unknown language: ~a\n" lang-str))
                             (exit 1))])
                   lib-name
                   output-file
                   input-file)))]
      [(string=? (car args) "-l")
       (if (null? (cdr args))
           (begin
             (display "Error: -l requires library name\n")
             (exit 1))
           (loop (cddr args)
                 lang
                 (cadr args)
                 output-file
                 input-file))]
      [(string=? (car args) "-o")
       (if (null? (cdr args))
           (begin
             (display "Error: -o requires filename\n")
             (exit 1))
           (loop (cddr args)
                 lang
                 lib-name
                 (cadr args)
                 input-file))]
      [(not input-file)
       (loop (cdr args)
             lang
             lib-name
             output-file
             (car args))]
      [else
       (display (format "Error: Unexpected argument: ~a\n" (car args)))
       (exit 1)])))

;;=======================================================================
;; Main entry point

(define (main)
  ;; Force effect registration by referencing the register functions
  (register-cpp-include!)
  (register-cpp-macros!)
  (register-cpp-conditional!)

  (let* ([args (command-line-arguments)]
         [parsed (parse-args args)]
         [lang-arg (car parsed)]
         [lib-name-arg (cadr parsed)]
         [output-file (caddr parsed)]
         [input-file (cadddr parsed)]
         [lang (or lang-arg (detect-language input-file))]
         [lib-name (or lib-name-arg (derive-lib-name input-file))])

    (display (format "Processing ~a as ~a (library: ~a)\n"
                    input-file
                    (if (eq? lang 'c++) "C++" "C")
                    lib-name))

    (if (eq? lang 'c++)
        (generate-cpp-ffi input-file lib-name output-file)
        (generate-c-ffi input-file lib-name output-file))

    (when output-file
      (display (format "Generated FFI bindings: ~a\n" output-file)))))

(main)
