;; SPDX-License-Identifier: WTFPL
;; Pattern Matcher for FFI Backend DSL
;; Supports type pattern matching with unification

(library (c-tools codegen dsl patterns)
  (export pattern-match
          match-type
          compile-type-map)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs sorting)
          (c-tools ast c))

  ;;=======================================================================
  ;; Pattern Matching

  ;; pattern-match : pattern value => (or bindings #f)
  ;;   Attempts to match pattern against value, returns bindings or #f.
  ;;   Patterns:
  ;;     symbol - matches if eq?
  ;;     (pattern ...) - matches list structure recursively
  ;;     ?var - pattern variable, captures any value
  (define (pattern-match pattern value)
    (pattern-match-helper pattern value '()))

  ;; pattern-match-helper : pattern value bindings => (or bindings #f)
  (define (pattern-match-helper pattern value bindings)
    (cond
      ;; Pattern variable ?var
      [(and (symbol? pattern)
            (char=? (string-ref (symbol->string pattern) 0) #\?))
       (let ([var (string->symbol (substring (symbol->string pattern) 1
                                             (string-length (symbol->string pattern))))])
         (let ([existing (assoc var bindings)])
           (if existing
               ;; Variable already bound - check consistency
               (if (equal? (cdr existing) value)
                   bindings
                   #f)
               ;; Bind new variable
               (cons (cons var value) bindings))))]

      ;; Literal symbol
      [(symbol? pattern)
       (if (and (symbol? value) (eq? pattern value))
           bindings
           #f)]

      ;; List pattern
      [(pair? pattern)
       (if (pair? value)
           (let ([car-bindings (pattern-match-helper (car pattern) (car value) bindings)])
             (if car-bindings
                 (pattern-match-helper (cdr pattern) (cdr value) car-bindings)
                 #f))
           #f)]

      ;; Empty list
      [(null? pattern)
       (if (null? value) bindings #f)]

      ;; Literal match
      [else
       (if (equal? pattern value) bindings #f)]))

  ;;=======================================================================
  ;; Type Pattern Matching

  ;; match-type : type-map type => (or result #f)
  ;;   Tries each pattern in type-map until one matches.
  ;;   Returns result (symbol, lambda, or list) or #f.
  (define (match-type type-map type)
    (let loop ([rules type-map])
      (if (null? rules)
          #f
          (let* ([rule (car rules)]
                 [pattern (car rule)]
                 [result-template (cdr rule)]
                 [bindings (match-type-pattern pattern type)])
            (if bindings
                ;; Match found - evaluate result
                (evaluate-result result-template bindings)
                ;; Try next rule
                (loop (cdr rules)))))))

  ;; match-type-pattern : pattern type => (or bindings #f)
  ;;   Matches pattern against C AST type structure.
  (define (match-type-pattern pattern type)
    (cond
      ;; Pattern variable
      [(and (symbol? pattern)
            (> (string-length (symbol->string pattern)) 0)
            (char=? (string-ref (symbol->string pattern) 0) #\?))
       (list (cons (string->symbol (substring (symbol->string pattern) 1
                                              (string-length (symbol->string pattern))))
                   type))]

      ;; Basic type literal
      [(symbol? pattern)
       (if (and (basic-type? type)
                (eq? pattern (basic-type-name type)))
           '()
           #f)]

      ;; Compound pattern
      [(pair? pattern)
       (case (car pattern)
         ;; (pointer pattern)
         [(pointer)
          (if (pointer-type? type)
              (match-type-pattern (cadr pattern) (pointer-type-pointee type))
              #f)]

         ;; (struct pattern)
         [(struct)
          (if (and (named-type? type)
                   (eq? (named-type-kind type) 'struct))
              (if (null? (cdr pattern))
                  ;; (struct) matches any struct
                  '()
                  ;; (struct ?name) captures name
                  (match-type-pattern (cadr pattern) (named-type-name type)))
              #f)]

         ;; (union pattern)
         [(union)
          (if (and (named-type? type)
                   (eq? (named-type-kind type) 'union))
              (if (null? (cdr pattern))
                  '()
                  (match-type-pattern (cadr pattern) (named-type-name type)))
              #f)]

         ;; (enum)
         [(enum)
          (if (and (named-type? type)
                   (eq? (named-type-kind type) 'enum))
              '()
              #f)]

         ;; (array element-pattern size-pattern)
         [(array)
          (if (array-type? type)
              (let ([elem-bindings (match-type-pattern (cadr pattern)
                                                       (array-type-element type))])
                (if elem-bindings
                    (if (> (length pattern) 2)
                        ;; Match size too
                        (let ([size-bindings (match-type-pattern (caddr pattern)
                                                                 (array-type-size type))])
                          (if size-bindings
                              (append elem-bindings size-bindings)
                              #f))
                        elem-bindings)
                    #f))
              #f)]

         [else #f])]

      ;; Qualified type - strip qualifiers
      [(qualified-type? type)
       (match-type-pattern pattern (qualified-type-type type))]

      [else #f]))

  ;; evaluate-result : result-template bindings => result
  ;;   Evaluates result template with bindings.
  (define (evaluate-result result-template bindings)
    (cond
      ;; Lambda - call with bound values
      [(procedure? result-template)
       (apply-lambda result-template bindings)]

      ;; Symbol or list - return as-is
      [else result-template]))

  ;; apply-lambda : lambda bindings => value
  ;;   Applies lambda to values from bindings in order they appear.
  (define (apply-lambda proc bindings)
    (let ([args (map cdr bindings)])
      (apply proc args)))

  ;;=======================================================================
  ;; Type Map Compilation

  ;; compile-type-map : list-of-rules => compiled-type-map
  ;;   Compiles type map rules, sorting by specificity.
  ;;   Rules: ((pattern → result) ...)
  (define (compile-type-map rules)
    (let ([parsed-rules (map parse-rule rules)])
      ;; Sort by specificity (more specific first)
      (sort-by-specificity parsed-rules)))

  ;; parse-rule : rule => (pattern . result)
  ;;   Parses rule in form (pattern result) - just a 2-element list.
  (define (parse-rule rule)
    (if (and (list? rule)
             (= (length rule) 2))
        (cons (car rule) (cadr rule))
        (error 'parse-rule "invalid type map rule: ~a" rule)))

  ;; sort-by-specificity : list-of-rules => list-of-rules
  ;;   Sorts rules so more specific patterns come first.
  (define (sort-by-specificity rules)
    (list-sort
      (lambda (rule1 rule2)
        (> (pattern-specificity (car rule1))
           (pattern-specificity (car rule2))))
      rules))

  ;; pattern-specificity : pattern => fixnum
  ;;   Returns specificity score (higher = more specific).
  (define (pattern-specificity pattern)
    (cond
      ;; Pattern variable - least specific
      [(and (symbol? pattern)
            (> (string-length (symbol->string pattern)) 0)
            (char=? (string-ref (symbol->string pattern) 0) #\?))
       0]

      ;; Literal symbol - somewhat specific
      [(symbol? pattern) 10]

      ;; Compound patterns - more specific
      [(pair? pattern)
       (case (car pattern)
         ;; (pointer char) more specific than (pointer ?T)
         [(pointer)
          (+ 20 (pattern-specificity (cadr pattern)))]
         [(struct union)
          (if (> (length pattern) 1)
              (+ 20 (pattern-specificity (cadr pattern)))
              15)]
         [(array)
          (+ 15 (pattern-specificity (cadr pattern)))]
         [else 5])]

      [else 0]))

) ;; end library
