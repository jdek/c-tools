;; Concrete Syntax Tree Nodes
;; CST nodes wrap tokens with surrounding trivia (whitespace and comments)
;; Enables formatters and tools that need to preserve exact source layout

(library (c-tools core cst)
  (export ;; CST node type
          make-cst-node cst-node?
          cst-node-token cst-node-trivia-before cst-node-trivia-after

          ;; Trivia type
          make-trivia trivia?
          trivia-type trivia-text trivia-location

          ;; Conversion utilities
          cst->token
          cst-list->token-list
          token->cst)

  (import (rnrs base)
          (rnrs lists)
          (rnrs records syntactic)
          (c-tools core tokens))

  ;; trivia : type text location => trivia
  ;;   Represents non-semantic source elements (whitespace, comments)
  ;;   type: 'whitespace or 'comment
  ;;   text: the actual text content
  ;;   location: source location
  (define-record-type trivia
    (fields type text location))

  ;; cst-node : token trivia-before trivia-after => cst-node
  ;;   Wraps a semantic token with surrounding trivia
  ;;   token: the semantic token (identifier, keyword, number, etc.)
  ;;   trivia-before: list of trivia elements before the token
  ;;   trivia-after: list of trivia elements after the token
  (define-record-type cst-node
    (fields token trivia-before trivia-after))

  ;; cst->token : cst-node => token
  ;;   Extract the semantic token from a CST node, discarding trivia
  (define (cst->token node)
    (cst-node-token node))

  ;; cst-list->token-list : (list cst-node) => (list token)
  ;;   Convert a list of CST nodes to a list of tokens
  ;;   Used for backward compatibility with token-based APIs
  (define (cst-list->token-list cst-nodes)
    (map cst->token cst-nodes))

  ;; token->cst : token => cst-node
  ;;   Wrap a token in a CST node with no trivia
  ;;   Useful for creating CST nodes from existing tokens
  (define (token->cst tok)
    (make-cst-node tok '() '())))
