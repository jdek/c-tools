;; C Tokenizer - Lexical Analysis
;; Thin wrapper around shared lexer core with C configuration

(library (c-tools lexer c)
  (export tokenize-port
          tokenize-string
          tokenize-file
          ;; CST API (new)
          tokenize-cst-port
          tokenize-cst-string
          tokenize-cst-file
          ;; Export keywords for compatibility
          c-keywords)

  (import (rnrs base)
          (rnrs io ports)
          (rnrs io simple)
          (c-tools core cst)
          (c-tools core tokens)
          (c-tools effects lexer diagnostics)
          (c-tools effects lexer limits)
          (c-tools lexer config)
          (c-tools lexer core))

  ;; Re-export c-keywords from config
  (define c-keywords (lexer-config-keywords c-lexer-config))

  ;;=======================================================================
  ;; CST API (returns CST nodes with trivia)

  ;; tokenize-cst-port : input-port string => (list cst-node)
  ;;   effects: io/read
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C source from input port, returns CST nodes with trivia
  (define (tokenize-cst-port port filename)
    (with-lexer-limits
      (lambda ()
        (tokenize-port-with-config c-lexer-config port filename))))

  ;; tokenize-cst-string : string string => (list cst-node)
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C source from string, returns CST nodes with trivia
  (define (tokenize-cst-string str filename)
    (call-with-port (open-string-input-port str)
      (lambda (port)
        (tokenize-cst-port port filename))))

  ;; tokenize-cst-file : string => (list cst-node)
  ;;   effects: io/read
  ;;   raises: on malformed input, I/O error, or security limit exceeded
  ;;   Tokenizes C source from file, returns CST nodes with trivia
  (define (tokenize-cst-file path)
    (call-with-input-file path
      (lambda (port)
        (tokenize-cst-port port path))))

  ;;=======================================================================
  ;; Token API (backward compatible - returns tokens only)

  ;; tokenize-port : input-port string => (list token)
  ;;   effects: io/read
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C source from input port, returns list of tokens
  (define (tokenize-port port filename)
    (cst-list->token-list (tokenize-cst-port port filename)))

  ;; tokenize-string : string string => (list token)
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C source from string, returns list of tokens
  (define (tokenize-string str filename)
    (call-with-port (open-string-input-port str)
      (lambda (port)
        (tokenize-port port filename))))

  ;; tokenize-file : string => (list token)
  ;;   effects: io/read
  ;;   raises: on malformed input, I/O error, or security limit exceeded
  ;;   Tokenizes C source from file, returns list of tokens
  (define (tokenize-file path)
    (call-with-input-file path
      (lambda (port)
        (tokenize-port port path)))))
