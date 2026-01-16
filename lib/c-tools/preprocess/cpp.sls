;; C++ Preprocessor - Main Entry Point
;; Reuses C preprocessor logic with C++ tokenizer

(library (c-tools preprocess cpp)
  (export preprocess-cpp-string
          preprocess-cpp-file
          preprocess-cpp-tokens)
  (import (rnrs base)
          (rnrs io ports)
          (rnrs io simple)
          (c-tools core conditions)
          (c-tools core tokens)
          (c-tools effects core)
          (c-tools effects cpp conditionals)
          (c-tools effects cpp core)
          (c-tools effects cpp diagnostics)
          (c-tools effects cpp includes)
          (c-tools effects cpp location)
          (c-tools effects cpp macros)
          (c-tools effects cpp symbols)
          (c-tools effects files)
          (c-tools lexer cpp)
          (c-tools preprocess c))  ;; reuse preprocess-tokens

  ;; preprocess-cpp-string : string string => (list token)
  ;;   Preprocess C++ source string, returns preprocessed tokens
  (define (preprocess-cpp-string str filename)
    (let ([tokens (tokenize-cpp-string str filename)])
      (preprocess-tokens tokens)))

  ;; preprocess-cpp-file : string => (list token)
  ;;   Preprocess C++ source file, returns preprocessed tokens
  (define (preprocess-cpp-file path)
    (let ([tokens (tokenize-cpp-file path)])
      (preprocess-tokens tokens)))

  ;; Re-export preprocess-tokens for C++ (same logic works for both)
  (define preprocess-cpp-tokens preprocess-tokens))
