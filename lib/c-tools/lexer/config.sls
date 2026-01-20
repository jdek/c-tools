;; SPDX-License-Identifier: WTFPL
;; Lexer Configuration
;; Parameterizes language-specific lexer features

(library (c-tools lexer config)
  (export make-lexer-config lexer-config?
          lexer-config-keywords
          lexer-config-two-char-operators
          lexer-config-three-char-operators
          lexer-config-supports-binary?
          lexer-config-supports-separators?
          lexer-config-supports-exponent?
          lexer-config-supports-raw-strings?
          lexer-config-preserve-whitespace?
          lexer-config-preserve-comments?

          ;; Predefined configurations
          c-lexer-config
          cpp-lexer-config)

  (import (rnrs base)
          (rnrs records syntactic)
          (c-tools core base))

  ;; lexer-config : configuration for language-specific lexer features
  (define-record-type lexer-config
    (fields keywords              ;; List of keyword symbols
            two-char-operators    ;; List of 2-char operator strings
            three-char-operators  ;; List of 3-char operator strings
            supports-binary?      ;; Boolean: supports 0b prefix
            supports-separators?  ;; Boolean: supports digit separators (')
            supports-exponent?    ;; Boolean: supports scientific notation
            supports-raw-strings? ;; Boolean: supports R"(...)" syntax
            preserve-whitespace?  ;; Boolean: capture whitespace as trivia
            preserve-comments?))  ;; Boolean: capture comments as trivia

  ;;=======================================================================
  ;; C Configuration

  ;; C keywords
  (define c-keywords
    '(auto break case char const continue default do
      double else enum extern float for goto if
      inline int long register restrict return short
      signed sizeof static struct switch typedef
      union unsigned void volatile while
      _Bool _Complex _Imaginary))

  ;; C configuration instance
  (define c-lexer-config
    (make-lexer-config
      c-keywords
      c-two-char-operators
      c-three-char-operators
      #f  ;; no binary literals
      #f  ;; no digit separators
      #f  ;; no exponents in C (actually C does have them, but simpler parsing)
      #f  ;; no raw strings
      #t  ;; preserve whitespace for CST
      #t)) ;; preserve comments for CST

  ;;=======================================================================
  ;; C++ Configuration

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

  ;; C++ configuration instance
  (define cpp-lexer-config
    (make-lexer-config
      cpp-keywords
      cpp-two-char-operators
      cpp-three-char-operators
      #t  ;; binary literals (0b prefix)
      #t  ;; digit separators (')
      #t  ;; exponents (scientific notation)
      #t  ;; raw strings R"(...)"
      #t  ;; preserve whitespace
      #t))) ;; preserve comments
