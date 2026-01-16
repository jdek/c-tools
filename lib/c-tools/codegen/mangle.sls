;; C++ Name Mangling - Itanium C++ ABI
;; Implements name mangling for C++ symbols
;; Reference: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling

(library (c-tools codegen mangle)
  (export mangle-function
          mangle-method
          mangle-constructor
          mangle-destructor
          mangle-variable
          demangle)
  (import (rnrs base)
          (rnrs control)
          (rnrs io ports)
          (rnrs lists)
          (rnrs mutable-pairs)
          (rnrs unicode)
          (c-tools ast c)
          (c-tools ast cpp))

  ;; Substitution table for compression
  ;; Each entry is a string representing a substitutable component
  ;; Substitutions are encoded as S_, S0_, S1_, S2_, etc.
  ;; where S_ = first substitution, S0_ = second, S1_ = third, etc.
  
  (define (make-substitution-table)
    (list '()))  ;; mutable cell containing list of substitutions
  
  (define (substitution-table-entries table)
    (car table))
  
  (define (substitution-table-add! table entry)
    ;; Only add if not already present
    (unless (member entry (car table))
      (set-car! table (append (car table) (list entry)))))
  
  (define (substitution-table-lookup table entry)
    ;; Returns substitution index if found, #f otherwise
    (let loop ([entries (car table)] [idx 0])
      (cond
        [(null? entries) #f]
        [(equal? (car entries) entry) idx]
        [else (loop (cdr entries) (+ idx 1))])))
  
  (define (encode-substitution-index idx)
    ;; S_ for first (idx=0), S0_ for second (idx=1), S1_ for third, etc.
    (if (= idx 0)
        "S_"
        (string-append "S" (encode-base36 (- idx 1)) "_")))
  
  (define (encode-base36 n)
    ;; Encode number in base 36 (0-9, A-Z)
    (if (< n 10)
        (string (integer->char (+ n (char->integer #\0))))
        (if (< n 36)
            (string (integer->char (+ (- n 10) (char->integer #\A))))
            ;; Multi-digit
            (string-append (encode-base36 (div n 36))
                           (encode-base36 (mod n 36))))))

  ;; Mangle a function declaration
  ;; Returns mangled name string
  (define (mangle-function decl namespaces)
    (let ([subs (make-substitution-table)])
      (call-with-string-output-port
        (lambda (out)
          (put-string out "_Z")
          (mangle-nested-name out subs
                              (function-decl-name decl)
                              namespaces
                              #f)  ;; not const
          (mangle-function-params out subs (function-decl-params decl))))))

  ;; Mangle a method (inside class)
  (define (mangle-method decl class-name namespaces const?)
    (let ([subs (make-substitution-table)])
      (call-with-string-output-port
        (lambda (out)
          (put-string out "_Z")
          (mangle-nested-name out subs
                              (function-decl-name decl)
                              (append namespaces (list class-name))
                              const?)
          (mangle-function-params out subs (function-decl-params decl))))))

  ;; Mangle a constructor
  ;; ctor-kind: 'complete (C1), 'base (C2), or 'allocating (C3)
  ;; C1 = complete object constructor
  ;; C2 = base object constructor (for inheritance)
  ;; C3 = allocating constructor (rare, not used by most compilers)
  (define (mangle-constructor class-name namespaces params ctor-kind)
    (let ([subs (make-substitution-table)])
      (call-with-string-output-port
        (lambda (out)
          (put-string out "_Z")
          (mangle-ctor-nested-name out subs class-name namespaces ctor-kind)
          (mangle-param-types out subs params)))))

  ;; Mangle a destructor
  ;; dtor-kind: 'complete (D1), 'base (D2), or 'deleting (D0)
  ;; D0 = deleting destructor (calls operator delete)
  ;; D1 = complete object destructor
  ;; D2 = base object destructor (for inheritance)
  (define (mangle-destructor class-name namespaces dtor-kind)
    (let ([subs (make-substitution-table)])
      (call-with-string-output-port
        (lambda (out)
          (put-string out "_Z")
          (mangle-dtor-nested-name out subs class-name namespaces dtor-kind)
          (put-char out #\v)))))  ;; destructors always take void

  ;; Mangle constructor nested name with C1/C2/C3
  (define (mangle-ctor-nested-name out subs class-name namespaces ctor-kind)
    (let ([ctor-code (case ctor-kind
                       [(complete) "C1"]
                       [(base) "C2"]
                       [(allocating) "C3"]
                       [else "C1"])])  ;; default to complete
      (put-char out #\N)
      ;; Mangle namespace path
      (let loop ([ns namespaces] [prefix ""])
        (unless (null? ns)
          (let ([ns-name (car ns)])
            (cond
              [(eq? ns-name 'std)
               (put-string out "St")
               (loop (cdr ns) "St")]
              [else
               (let* ([ns-str (mangle-source-name-to-string ns-name)]
                      [new-prefix (string-append prefix ns-str)]
                      [sub-idx (substitution-table-lookup subs new-prefix)])
                 (if sub-idx
                     (put-string out (encode-substitution-index sub-idx))
                     (begin
                       (mangle-source-name out ns-name)
                       (substitution-table-add! subs new-prefix)))
                 (loop (cdr ns) new-prefix))]))))
      ;; Mangle class name
      (mangle-source-name out class-name)
      ;; Constructor code
      (put-string out ctor-code)
      (put-char out #\E)))

  ;; Mangle destructor nested name with D0/D1/D2
  (define (mangle-dtor-nested-name out subs class-name namespaces dtor-kind)
    (let ([dtor-code (case dtor-kind
                       [(deleting) "D0"]
                       [(complete) "D1"]
                       [(base) "D2"]
                       [else "D1"])])  ;; default to complete
      (put-char out #\N)
      ;; Mangle namespace path
      (let loop ([ns namespaces] [prefix ""])
        (unless (null? ns)
          (let ([ns-name (car ns)])
            (cond
              [(eq? ns-name 'std)
               (put-string out "St")
               (loop (cdr ns) "St")]
              [else
               (let* ([ns-str (mangle-source-name-to-string ns-name)]
                      [new-prefix (string-append prefix ns-str)]
                      [sub-idx (substitution-table-lookup subs new-prefix)])
                 (if sub-idx
                     (put-string out (encode-substitution-index sub-idx))
                     (begin
                       (mangle-source-name out ns-name)
                       (substitution-table-add! subs new-prefix)))
                 (loop (cdr ns) new-prefix))]))))
      ;; Mangle class name
      (mangle-source-name out class-name)
      ;; Destructor code
      (put-string out dtor-code)
      (put-char out #\E)))

  ;; Mangle parameter types (for constructors)
  (define (mangle-param-types out subs params)
    (if (null? params)
        (put-char out #\v)  ;; void
        (for-each (lambda (param)
                    (mangle-type out subs (param-type param)))
                  params)))

  ;; Mangle a global variable
  (define (mangle-variable name namespaces)
    (let ([subs (make-substitution-table)])
      (call-with-string-output-port
        (lambda (out)
          (put-string out "_Z")
          (if (null? namespaces)
              (mangle-source-name out name)
              (mangle-nested-name out subs name namespaces #f))))))

  ;; Standard library abbreviations per Itanium ABI
  ;; St = std::
  ;; Sa = std::allocator
  ;; Sb = std::basic_string
  ;; Ss = std::string (basic_string<char, char_traits<char>, allocator<char>>)
  ;; Si = std::istream (basic_istream<char, char_traits<char>>)
  ;; So = std::ostream (basic_ostream<char, char_traits<char>>)
  ;; Sd = std::iostream (basic_iostream<char, char_traits<char>>)
  
  (define (std-abbreviation name)
    ;; Returns abbreviation string if name is a standard library name, #f otherwise
    (case name
      [(std) "St"]
      [(allocator) "Sa"]
      [(basic_string) "Sb"]
      [(string) "Ss"]
      [(istream basic_istream) "Si"]
      [(ostream basic_ostream) "So"]
      [(iostream basic_iostream) "Sd"]
      [else #f]))

  ;; Mangle nested name (N...E format)
  ;; Tracks namespace prefixes as substitutable components
  ;; Special case: std:: alone uses St prefix without N...E wrapper
  (define (mangle-nested-name out subs name namespaces const?)
    (cond
      ;; No namespaces - just emit the name
      [(null? namespaces)
       (mangle-source-name out name)]
      ;; Special case: std:: alone (no deeper nesting) uses St prefix
      [(and (= (length namespaces) 1)
            (eq? (car namespaces) 'std)
            (not const?))
       (put-string out "St")
       (mangle-source-name out name)]
      ;; General nested name
      [else
       (put-char out #\N)
       (when const?
         (put-char out #\K))
       ;; Mangle each namespace, tracking prefixes as substitutions
       (let loop ([ns namespaces] [prefix ""] [in-std? #f])
         (unless (null? ns)
           (let ([ns-name (car ns)])
             (cond
               ;; std:: namespace - use St abbreviation
               [(eq? ns-name 'std)
                (put-string out "St")
                (loop (cdr ns) "St" #t)]
               ;; Check if this prefix is already substituted
               [else
                (let* ([ns-str (mangle-source-name-to-string ns-name)]
                       [new-prefix (string-append prefix ns-str)]
                       [sub-idx (substitution-table-lookup subs new-prefix)])
                  (if sub-idx
                      ;; Use substitution
                      (put-string out (encode-substitution-index sub-idx))
                      ;; Emit and register
                      (begin
                        (mangle-source-name out ns-name)
                        (substitution-table-add! subs new-prefix)))
                  (loop (cdr ns) new-prefix in-std?))]))))
       (mangle-source-name out name)
       (put-char out #\E)]))

  ;; Mangle source name (length-prefixed) or operator name
  (define (mangle-source-name out name)
    (let ([str (symbol->string name)])
      (cond
        ;; Check for operator names
        [(operator-encoding str)
         => (lambda (enc) (put-string out enc))]
        ;; Regular name
        [else
         (let ([len (string-length str)])
           (put-string out (number->string len))
           (put-string out str))])))

  ;; Get operator encoding for Itanium ABI, or #f if not an operator
  (define (operator-encoding name)
    (cond
      [(string=? name "operator+") "pl"]
      [(string=? name "operator-") "mi"]
      [(string=? name "operator*") "ml"]
      [(string=? name "operator/") "dv"]
      [(string=? name "operator%") "rm"]
      [(string=? name "operator^") "eo"]
      [(string=? name "operator&") "an"]
      [(string=? name "operator|") "or"]
      [(string=? name "operator~") "co"]
      [(string=? name "operator!") "nt"]
      [(string=? name "operator=") "aS"]
      [(string=? name "operator<") "lt"]
      [(string=? name "operator>") "gt"]
      [(string=? name "operator+=") "pL"]
      [(string=? name "operator-=") "mI"]
      [(string=? name "operator*=") "mL"]
      [(string=? name "operator/=") "dV"]
      [(string=? name "operator%=") "rM"]
      [(string=? name "operator^=") "eO"]
      [(string=? name "operator&=") "aN"]
      [(string=? name "operator|=") "oR"]
      [(string=? name "operator<<") "ls"]
      [(string=? name "operator>>") "rs"]
      [(string=? name "operator<<=") "lS"]
      [(string=? name "operator>>=") "rS"]
      [(string=? name "operator==") "eq"]
      [(string=? name "operator!=") "ne"]
      [(string=? name "operator<=") "le"]
      [(string=? name "operator>=") "ge"]
      [(string=? name "operator<=>") "ss"]  ;; C++20 spaceship
      [(string=? name "operator&&") "aa"]
      [(string=? name "operator||") "oo"]
      [(string=? name "operator++") "pp"]
      [(string=? name "operator--") "mm"]
      [(string=? name "operator,") "cm"]
      [(string=? name "operator->*") "pm"]
      [(string=? name "operator->") "pt"]
      [(string=? name "operator()") "cl"]
      [(string=? name "operator-index") "ix"]  ;; our internal name for operator[]
      [(string=? name "operator[]") "ix"]
      [(string=? name "operator new") "nw"]
      [(string=? name "operator new[]") "na"]
      [(string=? name "operator delete") "dl"]
      [(string=? name "operator delete[]") "da"]
      [else #f]))
  
  ;; Mangle source name to string (for substitution tracking)
  (define (mangle-source-name-to-string name)
    (let ([str (symbol->string name)])
      (string-append (number->string (string-length str)) str)))

  ;; Mangle function parameters
  (define (mangle-function-params out subs params)
    (if (null? params)
        (put-char out #\v)  ;; void
        (for-each (lambda (param)
                    (mangle-type out subs (param-type param)))
                  params)))

  ;; Mangle a type
  ;; Tracks compound types as substitutable
  (define (mangle-type out subs type)
    ;; Check for existing substitution first (only for substitutable types)
    (define (substitutable? t)
      (or (pointer-type? t)
          (reference-type? t)
          (qualified-type? t)
          (named-type? t)
          (template-type? t)
          (array-type? t)
          (function-type? t)))
    
    ;; Get type key for substitution lookup
    (define (type-key t)
      (call-with-string-output-port
        (lambda (p) (mangle-type-no-sub p t))))
    
    (if (substitutable? type)
        (let ([key (type-key type)])
          (let ([sub-idx (substitution-table-lookup subs key)])
            (if sub-idx
                ;; Use existing substitution
                (put-string out (encode-substitution-index sub-idx))
                ;; Emit and register
                (begin
                  (mangle-type-impl out subs type)
                  (substitution-table-add! subs key)))))
        ;; Non-substitutable (basic types) - just emit
        (mangle-type-impl out subs type)))
  
  ;; Mangle type without substitution (for key generation)
  (define (mangle-type-no-sub out type)
    (cond
      [(basic-type? type)
       (mangle-basic-type out (basic-type-name type))]
      [(pointer-type? type)
       (put-char out #\P)
       (mangle-type-no-sub out (pointer-type-pointee type))]
      [(reference-type? type)
       (put-char out #\R)
       (mangle-type-no-sub out (reference-type-referent type))]
      [(qualified-type? type)
       (let ([quals (qualified-type-qualifiers type)]
             [inner (qualified-type-type type)])
         (when (memq 'const quals) (put-char out #\K))
         (when (memq 'volatile quals) (put-char out #\V))
         (mangle-type-no-sub out inner))]
      [(named-type? type)
       (mangle-named-type-no-sub out type)]
      [(template-type? type)
       (mangle-template-type-no-sub out type)]
      [(array-type? type)
       (put-char out #\A)
       (when (array-type-size type)
         (put-string out (number->string (array-type-size type))))
       (put-char out #\_)
       (mangle-type-no-sub out (array-type-element type))]
      [(function-type? type)
       (put-char out #\F)
       (mangle-type-no-sub out (function-type-return type))
       (if (null? (function-type-params type))
           (put-char out #\v)
           (for-each (lambda (t) (mangle-type-no-sub out t))
                     (function-type-params type)))
       (put-char out #\E)]
      [else (put-char out #\?)]))
  
  (define (mangle-named-type-no-sub out type)
    (let ([name (named-type-name type)])
      (cond
        [(qualified-name? name)
         (put-char out #\N)
         (for-each (lambda (scope)
                     (if (template-type? scope)
                         (mangle-template-type-no-sub out scope)
                         (mangle-source-name out scope)))
                   (qualified-name-scopes name))
         (let ([final (qualified-name-name name)])
           (if (template-type? final)
               (mangle-template-type-no-sub out final)
               (mangle-source-name out final)))
         (put-char out #\E)]
        [(template-type? name)
         (mangle-template-type-no-sub out name)]
        [else
         (mangle-source-name out name)])))
  
  (define (mangle-template-type-no-sub out type)
    (mangle-source-name out (template-type-name type))
    (put-char out #\I)
    (for-each (lambda (arg)
                (mangle-template-arg-no-sub out arg))
              (template-type-args type))
    (put-char out #\E))
  
  (define (mangle-template-arg-no-sub out arg)
    (cond
      [(or (basic-type? arg) (pointer-type? arg) (reference-type? arg)
           (qualified-type? arg) (named-type? arg) (template-type? arg)
           (array-type? arg) (function-type? arg))
       (mangle-type-no-sub out arg)]
      [(integer? arg)
       (put-char out #\L)
       (put-char out #\i)
       (if (< arg 0)
           (begin (put-char out #\n) (put-string out (number->string (- arg))))
           (put-string out (number->string arg)))
       (put-char out #\E)]
      [(symbol? arg)
       (mangle-source-name out arg)]
      [else (put-char out #\?)]))
  
  ;; Mangle type implementation (actual output)
  (define (mangle-type-impl out subs type)
    (cond
      ;; Basic types
      [(basic-type? type)
       (mangle-basic-type out (basic-type-name type))]

      ;; Pointer
      [(pointer-type? type)
       (put-char out #\P)
       (mangle-type out subs (pointer-type-pointee type))]

      ;; Reference
      [(reference-type? type)
       (put-char out #\R)
       (mangle-type out subs (reference-type-referent type))]

      ;; Qualified type (const, volatile)
      [(qualified-type? type)
       (let ([quals (qualified-type-qualifiers type)]
             [inner (qualified-type-type type)])
         ;; Qualifiers come before the type they modify
         (when (memq 'const quals)
           (put-char out #\K))
         (when (memq 'volatile quals)
           (put-char out #\V))
         (mangle-type out subs inner))]

      ;; Named type (struct, class, typedef)
      [(named-type? type)
       (let ([kind (named-type-kind type)]
             [name (named-type-name type)])
         (cond
           ;; Qualified name
           [(qualified-name? name)
            (put-char out #\N)
            (for-each (lambda (scope)
                        (if (template-type? scope)
                            (mangle-template-type-with-subs out subs scope)
                            (mangle-source-name out scope)))
                      (qualified-name-scopes name))
            (let ([final (qualified-name-name name)])
              (if (template-type? final)
                  (mangle-template-type-with-subs out subs final)
                  (mangle-source-name out final)))
            (put-char out #\E)]
           ;; Template type
           [(template-type? name)
            (mangle-template-type-with-subs out subs name)]
           ;; Simple name
           [else
            (mangle-source-name out name)]))]

      ;; Template type
      [(template-type? type)
       (mangle-template-type-with-subs out subs type)]

      ;; Array type
      [(array-type? type)
       (put-char out #\A)
       (when (array-type-size type)
         (put-string out (number->string (array-type-size type))))
       (put-char out #\_)
       (mangle-type out subs (array-type-element type))]

      ;; Function type
      [(function-type? type)
       (put-char out #\F)
       (mangle-type out subs (function-type-return type))
       (if (null? (function-type-params type))
           (put-char out #\v)
           (for-each (lambda (t) (mangle-type out subs t))
                     (function-type-params type)))
       (put-char out #\E)]

      [else
       (put-char out #\?)]))  ;; Unknown

  ;; Mangle basic type
  (define (mangle-basic-type out name)
    (put-string out
      (case name
        [(void) "v"]
        [(bool) "b"]
        [(char) "c"]
        [(signed-char) "a"]
        [(unsigned-char) "h"]
        [(short) "s"]
        [(unsigned-short) "t"]
        [(int) "i"]
        [(unsigned) "j"]
        [(long) "l"]
        [(unsigned-long) "m"]
        [(long-long) "x"]
        [(unsigned-long-long) "y"]
        [(float) "f"]
        [(double) "d"]
        [(long-double) "e"]
        ;; C++11 types
        [(char16_t) "Ds"]
        [(char32_t) "Di"]
        [(wchar_t) "w"]
        ;; Extensions
        [(auto) "Da"]
        [else "?"])))

  ;; Mangle template type (name<args>) with substitution support
  (define (mangle-template-type-with-subs out subs type)
    (mangle-source-name out (template-type-name type))
    (put-char out #\I)
    (for-each (lambda (arg)
                (mangle-template-arg-with-subs out subs arg))
              (template-type-args type))
    (put-char out #\E))

  ;; Mangle template argument with substitution support
  (define (mangle-template-arg-with-subs out subs arg)
    (cond
      ;; Type argument
      [(or (basic-type? arg)
           (pointer-type? arg)
           (reference-type? arg)
           (qualified-type? arg)
           (named-type? arg)
           (template-type? arg)
           (array-type? arg)
           (function-type? arg))
       (mangle-type out subs arg)]
      ;; Integer value (for non-type parameters)
      [(integer? arg)
       (put-char out #\L)
       (put-char out #\i)  ;; int
       (if (< arg 0)
           (begin
             (put-char out #\n)
             (put-string out (number->string (- arg))))
           (put-string out (number->string arg)))
       (put-char out #\E)]
      ;; Symbol (template parameter reference)
      [(symbol? arg)
       (mangle-source-name out arg)]
      [else
       (put-char out #\?)]))

  ;; Demangle a mangled name (basic implementation)
  ;; Returns a human-readable string
  (define (demangle mangled)
    (if (and (>= (string-length mangled) 2)
             (char=? (string-ref mangled 0) #\_)
             (char=? (string-ref mangled 1) #\Z))
        (demangle-mangled (substring mangled 2 (string-length mangled)))
        mangled))  ;; Not mangled

  ;; Internal demangling with substitution table
  (define (demangle-mangled str)
    (let ([port (open-string-input-port str)]
          [subs (list '())])  ;; mutable substitution table
      (call-with-string-output-port
        (lambda (out)
          (demangle-name port out subs)
          (put-char out #\()
          (demangle-params port out subs)
          (put-char out #\))))))
  
  ;; Add to substitution table during demangling
  (define (demangle-subs-add! subs str)
    (set-car! subs (append (car subs) (list str))))
  
  ;; Get substitution by index
  (define (demangle-subs-get subs idx)
    (let loop ([lst (car subs)] [i 0])
      (cond
        [(null? lst) "?"]
        [(= i idx) (car lst)]
        [else (loop (cdr lst) (+ i 1))])))

  ;; Demangle name portion
  (define (demangle-name port out subs)
    (let ([c (lookahead-char port)])
      (cond
        [(eof-object? c) #f]
        [(char=? c #\N)
         ;; Nested name
         (get-char port)  ;; consume N
         (demangle-nested-name port out subs)]
        [(char=? c #\S)
         ;; Check for St (std:: prefix without N...E)
         (get-char port)  ;; consume S
         (let ([c2 (lookahead-char port)])
           (cond
             [(char=? c2 #\t)
              ;; St = std::name format
              (get-char port)  ;; consume t
              (put-string out "std::")
              ;; Now read the actual name
              (let ([name (demangle-source-name-to-string port)])
                (put-string out name))]
             [else
              ;; Other substitution - put S back by handling inline
              (cond
                [(char=? c2 #\_)
                 (get-char port)
                 (put-string out (demangle-subs-get subs 0))]
                [(or (char-numeric? c2)
                     (and (char? c2) (char>=? c2 #\A) (char<=? c2 #\Z)))
                 (let ([idx (+ 1 (decode-base36 port))])
                   (put-string out (demangle-subs-get subs idx)))]
                [else (put-char out #\?)])]))]
        [(char-numeric? c)
         ;; Simple source name
         (let ([name (demangle-source-name-to-string port)])
           (put-string out name)
           (demangle-subs-add! subs name))]
        [else
         (put-char out #\?)])))

  ;; Demangle nested name
  (define (demangle-nested-name port out subs)
    (let ([first? #t]
          [prefix ""])
      (let loop ()
        (let ([c (lookahead-char port)])
          (cond
            [(eof-object? c) #f]
            [(char=? c #\E)
             (get-char port)]  ;; consume E, done
            [(char=? c #\K)
             (get-char port)  ;; const qualifier
             (loop)]
            [(char=? c #\S)
             ;; Substitution within nested name
             (unless first?
               (put-string out "::"))
             (set! first? #f)
             (let ([sub-str (demangle-substitution-to-string port subs)])
               (put-string out sub-str)
               (set! prefix (string-append prefix (if (string=? prefix "") "" "::") sub-str)))
             (loop)]
            [(char-numeric? c)
             (unless first?
               (put-string out "::"))
             (set! first? #f)
             (let ([name (demangle-source-name-to-string port)])
               (put-string out name)
               (let ([new-prefix (string-append prefix (if (string=? prefix "") "" "::") name)])
                 (demangle-subs-add! subs new-prefix)
                 (set! prefix new-prefix)))
             (loop)]
            [else
             (get-char port)
             (loop)])))))

  ;; Demangle source name, returning as string
  (define (demangle-source-name-to-string port)
    (let ([len (read-decimal port)])
      (call-with-string-output-port
        (lambda (out)
          (let loop ([i 0])
            (when (< i len)
              (let ([c (get-char port)])
                (when (char? c)
                  (put-char out c)))
              (loop (+ i 1))))))))

  ;; Demangle source name (backwards compat)
  (define (demangle-source-name port out)
    (put-string out (demangle-source-name-to-string port)))

  ;; Read decimal number from port
  (define (read-decimal port)
    (let loop ([n 0])
      (let ([c (lookahead-char port)])
        (if (and (char? c) (char-numeric? c))
            (begin
              (get-char port)
              (loop (+ (* n 10)
                       (- (char->integer c) (char->integer #\0)))))
            n))))
  
  ;; Decode base36 index from substitution
  (define (decode-base36 port)
    (let loop ([n 0])
      (let ([c (lookahead-char port)])
        (cond
          [(eof-object? c) n]
          [(char=? c #\_)
           (get-char port)
           n]
          [(char-numeric? c)
           (get-char port)
           (loop (+ (* n 36) (- (char->integer c) (char->integer #\0))))]
          [(and (char>=? c #\A) (char<=? c #\Z))
           (get-char port)
           (loop (+ (* n 36) (+ 10 (- (char->integer c) (char->integer #\A)))))]
          [else n]))))

  ;; Demangle substitution sequence, output to port
  (define (demangle-substitution port out subs)
    (put-string out (demangle-substitution-to-string port subs)))
  
  ;; Demangle substitution and return as string
  (define (demangle-substitution-to-string port subs)
    (get-char port)  ;; consume S
    (let ([c (lookahead-char port)])
      (cond
        ;; Standard library abbreviations
        [(char=? c #\t)  ;; St = std::
         (get-char port)
         "std"]
        [(char=? c #\a)  ;; Sa = std::allocator
         (get-char port)
         "std::allocator"]
        [(char=? c #\b)  ;; Sb = std::basic_string
         (get-char port)
         "std::basic_string"]
        [(char=? c #\s)  ;; Ss = std::string
         (get-char port)
         "std::string"]
        [(char=? c #\i)  ;; Si = std::istream
         (get-char port)
         "std::istream"]
        [(char=? c #\o)  ;; So = std::ostream
         (get-char port)
         "std::ostream"]
        [(char=? c #\d)  ;; Sd = std::iostream
         (get-char port)
         "std::iostream"]
        [(char=? c #\_)
         ;; S_ = first substitution (index 0)
         (get-char port)
         (demangle-subs-get subs 0)]
        [(or (char-numeric? c)
             (and (char? c) (char>=? c #\A) (char<=? c #\Z)))
         ;; S<seq>_ = index seq+1
         (let ([idx (+ 1 (decode-base36 port))])
           (demangle-subs-get subs idx))]
        [else "?"])))

  ;; Demangle parameter list
  (define (demangle-params port out subs)
    (let ([first? #t])
      (let loop ()
        (let ([c (lookahead-char port)])
          (cond
            [(eof-object? c) #f]
            [(char=? c #\v)
             (get-char port)]  ;; void, no params
            [else
             (unless first?
               (put-string out ", "))
             (set! first? #f)
             (demangle-type port out subs)
             (loop)])))))

  ;; Demangle template arguments
  ;; Forward declared before demangle-type which calls it
  (define demangle-template-args)

  ;; Demangle type
  (define (demangle-type port out subs)
    (let ([c (get-char port)])
      (cond
        [(eof-object? c) #f]
        ;; Basic types
        [(char=? c #\v) (put-string out "void")]
        [(char=? c #\b) (put-string out "bool")]
        [(char=? c #\c) (put-string out "char")]
        [(char=? c #\a) (put-string out "signed char")]
        [(char=? c #\h) (put-string out "unsigned char")]
        [(char=? c #\s) (put-string out "short")]
        [(char=? c #\t) (put-string out "unsigned short")]
        [(char=? c #\i) (put-string out "int")]
        [(char=? c #\j) (put-string out "unsigned int")]
        [(char=? c #\l) (put-string out "long")]
        [(char=? c #\m) (put-string out "unsigned long")]
        [(char=? c #\x) (put-string out "long long")]
        [(char=? c #\y) (put-string out "unsigned long long")]
        [(char=? c #\f) (put-string out "float")]
        [(char=? c #\d) (put-string out "double")]
        [(char=? c #\e) (put-string out "long double")]
        [(char=? c #\w) (put-string out "wchar_t")]
        ;; Pointer
        [(char=? c #\P)
         (demangle-type port out subs)
         (put-char out #\*)]
        ;; Reference
        [(char=? c #\R)
         (demangle-type port out subs)
         (put-char out #\&)]
        ;; Const
        [(char=? c #\K)
         (put-string out "const ")
         (demangle-type port out subs)]
        ;; Volatile
        [(char=? c #\V)
         (put-string out "volatile ")
         (demangle-type port out subs)]
        ;; Substitution
        [(char=? c #\S)
         (let ([c2 (lookahead-char port)])
           (cond
             [(char=? c2 #\_)
              (get-char port)
              (put-string out (demangle-subs-get subs 0))]
             [(or (char-numeric? c2)
                  (and (char? c2) (char>=? c2 #\A) (char<=? c2 #\Z)))
              (let ([idx (+ 1 (decode-base36 port))])
                (put-string out (demangle-subs-get subs idx)))]
             [else (put-char out #\?)]))]
        ;; Named type
        [(char-numeric? c)
         ;; Read full length
         (let ([full-len (let loop ([n (- (char->integer c) (char->integer #\0))])
                           (let ([c2 (lookahead-char port)])
                             (if (and (char? c2) (char-numeric? c2))
                                 (begin
                                   (get-char port)
                                   (loop (+ (* n 10)
                                            (- (char->integer c2) (char->integer #\0)))))
                                 n)))])
           (let ([name (call-with-string-output-port
                         (lambda (p)
                           (let loop ([i 0])
                             (when (< i full-len)
                               (let ([ch (get-char port)])
                                 (when (char? ch)
                                   (put-char p ch)))
                               (loop (+ i 1))))))])
             (put-string out name)
             (demangle-subs-add! subs name)))]
        ;; Nested name
        [(char=? c #\N)
         (demangle-nested-name port out subs)]
        ;; Template
        [(char=? c #\I)
         (put-char out #\<)
         (demangle-template-args port out subs)
         (put-char out #\>)]
        [else
         (put-char out #\?)])))

  ;; Demangle template arguments implementation
  (set! demangle-template-args
    (lambda (port out subs)
      (let ([first? #t])
        (let loop ()
          (let ([c (lookahead-char port)])
            (cond
              [(eof-object? c) #f]
              [(char=? c #\E)
               (get-char port)]  ;; done
              [else
               (unless first?
                 (put-string out ", "))
               (set! first? #f)
               (demangle-type port out subs)
               (loop)])))))))
