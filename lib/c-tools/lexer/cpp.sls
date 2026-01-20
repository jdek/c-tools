;; C++ Tokenizer - Lexical Analysis
;; Thin wrapper around shared lexer core with C++ configuration

(library (c-tools lexer cpp)
  (export tokenize-cpp-port
          tokenize-cpp-string
          tokenize-cpp-file
          ;; CST API (new)
          tokenize-cpp-cst-port
          tokenize-cpp-cst-string
          tokenize-cpp-cst-file
          ;; Export keywords and operators for compatibility
          cpp-keywords
          cpp-two-char-operators
          cpp-three-char-operators)

  (import (rnrs base)
          (rnrs io ports)
          (rnrs io simple)
          (c-tools core cst)
          (c-tools core tokens)
          (c-tools effects lexer diagnostics)
          (c-tools effects lexer limits)
          (c-tools lexer config)
          (c-tools lexer core))

  ;; Re-export keywords and operators from config
  (define cpp-keywords (lexer-config-keywords cpp-lexer-config))
  (define cpp-two-char-operators (lexer-config-two-char-operators cpp-lexer-config))
  (define cpp-three-char-operators (lexer-config-three-char-operators cpp-lexer-config))

  ;;=======================================================================
  ;; CST API (returns CST nodes with trivia)

  ;; tokenize-cpp-cst-port : input-port string => (list cst-node)
  ;;   effects: io/read
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C++ source from input port, returns CST nodes with trivia
  (define (tokenize-cpp-cst-port port filename)
    (with-lexer-limits
      (lambda ()
        (tokenize-port-with-config cpp-lexer-config port filename))))

  ;; tokenize-cpp-cst-string : string string => (list cst-node)
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C++ source from string, returns CST nodes with trivia
  (define (tokenize-cpp-cst-string str filename)
    (call-with-port (open-string-input-port str)
      (lambda (port)
        (tokenize-cpp-cst-port port filename))))

  ;; tokenize-cpp-cst-file : string => (list cst-node)
  ;;   effects: io/read
  ;;   raises: on malformed input, I/O error, or security limit exceeded
  ;;   Tokenizes C++ source from file, returns CST nodes with trivia
  (define (tokenize-cpp-cst-file path)
    (call-with-input-file path
      (lambda (port)
        (tokenize-cpp-cst-port port path))))

  ;;=======================================================================
  ;; Token API (backward compatible - returns tokens only)

  ;; tokenize-cpp-port : input-port string => (list token)
  ;;   effects: io/read
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C++ source from input port, returns list of tokens
  (define (tokenize-cpp-port port filename)
    (cst-list->token-list (tokenize-cpp-cst-port port filename)))

  ;; tokenize-cpp-string : string string => (list token)
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C++ source from string, returns list of tokens
  (define (tokenize-cpp-string str filename)
    (call-with-port (open-string-input-port str)
      (lambda (port)
        (tokenize-cpp-port port filename))))

  ;; tokenize-cpp-file : string => (list token)
  ;;   effects: io/read
  ;;   raises: on malformed input, I/O error, or security limit exceeded
  ;;   Tokenizes C++ source from file, returns list of tokens
  (define (tokenize-cpp-file path)
    (call-with-input-file path
      (lambda (port)
        (tokenize-cpp-port port path)))))
