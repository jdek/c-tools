;; SPDX-License-Identifier: WTFPL
;; C Tokenizer - Token Types
;; Token record types for C lexical elements

(library (c-tools core tokens)
  (export make-token token?
          token-type token-value token-location

          ;; Token type predicates
          identifier-token? keyword-token? number-token? string-literal?
          char-literal? punctuator? preprocessor-directive?
          whitespace-token? comment-token? eof-token?

          ;; Location helpers
          make-location location?
          location-file location-line location-column
          macro-expansion-location)
  (import (rnrs base)
          (rnrs records syntactic))

  ;; Source location record
  (define-record-type location
    (fields file line column))

  ;; Token record: type, value, location
  ;; Type is a symbol: 'identifier, 'keyword, 'number, 'string, etc.
  ;; Value is the actual text or parsed value
  ;; Location is a location record for error reporting
  (define-record-type token
    (fields type value location))

  ;; Token type predicates (renamed to avoid conflicts with R6RS)
  (define (identifier-token? tok)
    (and (token? tok) (eq? (token-type tok) 'identifier)))

  (define (keyword-token? tok)
    (and (token? tok) (eq? (token-type tok) 'keyword)))

  (define (number-token? tok)
    (and (token? tok) (eq? (token-type tok) 'number)))

  (define (string-literal? tok)
    (and (token? tok) (eq? (token-type tok) 'string)))

  (define (char-literal? tok)
    (and (token? tok) (eq? (token-type tok) 'char)))

  (define (punctuator? tok)
    (and (token? tok) (eq? (token-type tok) 'punctuator)))

  (define (preprocessor-directive? tok)
    (and (token? tok) (eq? (token-type tok) 'directive)))

  (define (whitespace-token? tok)
    (and (token? tok) (eq? (token-type tok) 'whitespace)))

  (define (comment-token? tok)
    (and (token? tok) (eq? (token-type tok) 'comment)))

  (define (eof-token? tok)
    (and (token? tok) (eq? (token-type tok) 'eof)))

  ;;=======================================================================
  ;; Location Helpers

  ;; macro-expansion-location : => location
  ;;   Returns a standard location for macro-expanded tokens
  (define (macro-expansion-location)
    (make-location "macro" 1 1)))
