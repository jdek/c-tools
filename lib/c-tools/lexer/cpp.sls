;; SPDX-License-Identifier: WTFPL
;; C++ Tokenizer - Lexical Analysis
;; Thin wrapper around shared lexer core with C++ configuration

(library (c-tools lexer cpp)
  (export tokenize-cpp-port
          tokenize-cpp-string
          tokenize-cpp-file
          ;; CST API
          tokenize-cpp-cst-port
          tokenize-cpp-cst-string
          tokenize-cpp-cst-file
          ;; Diagnostic API
          tokenize-cpp-with-diagnostics
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

  (define cpp-keywords (lexer-config-keywords cpp-lexer-config))
  (define cpp-two-char-operators (lexer-config-two-char-operators cpp-lexer-config))
  (define cpp-three-char-operators (lexer-config-three-char-operators cpp-lexer-config))

  ;;=======================================================================
  ;; Diagnostic API

  ;; tokenize-cpp-with-diagnostics : input-port string => (values (list cst-node) (list diagnostic))
  ;;   effects: io/read
  ;;   raises: on malformed input or security limit exceeded
  ;;   Returns both CST nodes and collected diagnostics
  (define (tokenize-cpp-with-diagnostics port filename)
    (with-lexer-limits
      (lambda ()
        (with-lexer-diagnostics
          (lambda ()
            (tokenize-port-with-config cpp-lexer-config port filename))))))

  ;;=======================================================================
  ;; CST API

  ;; tokenize-cpp-cst-port : input-port string => (list cst-node)
  ;;   effects: io/read
  ;;   raises: on malformed input or security limit exceeded
  ;;   Tokenizes C++ source from input port, returns CST nodes with trivia
  (define (tokenize-cpp-cst-port port filename)
    (call-with-values
      (lambda () (tokenize-cpp-with-diagnostics port filename))
      (lambda (cst-nodes diagnostics)
        ;; Discard diagnostics for simple API
        cst-nodes)))

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
  ;; Token API

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
