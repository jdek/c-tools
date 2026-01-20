;; SPDX-License-Identifier: WTFPL
;; C++ FFI Code Generator - AST to Chez Scheme FFI
;; Generates foreign-procedure and foreign-struct definitions
;; Handles C++ namespaces, classes, and name mangling

(library (c-tools codegen chez cpp-ffi)
  (export generate-cpp-ffi-code
          cpp-declaration->ffi-form)
  (import (rnrs base)
          (rnrs control)
          (rnrs io ports)
          (rnrs lists)
          (c-tools ast c)
          (c-tools ast cpp)
          (c-tools codegen chez ffi)
          (c-tools codegen mangle)
          (only (chezscheme) format set-car! set-cdr!))

  ;; Generate FFI code from C++ declarations
  ;; lib-name: shared library name (e.g., "libfoo.so")
  ;; declarations: list of parsed C++ declarations
  (define (generate-cpp-ffi-code declarations lib-name module-name)
    (let ([forms (collect-ffi-forms declarations '())])
      `(library (ffi ,module-name)
         (export ,@(extract-exports forms))
         (import (chezscheme))

         ;; Load the shared library
         (define lib (load-shared-object ,lib-name))

         ,@forms)))

  ;; Collect FFI forms from declarations, tracking namespace context
  (define (collect-ffi-forms declarations namespaces)
    (let loop ([decls declarations] [forms '()])
      (if (null? decls)
          (reverse forms)
          (let ([decl (car decls)])
            (cond
              ;; Namespace - recurse with updated context
              [(namespace-decl? decl)
               (let ([ns-forms (collect-ffi-forms
                                 (namespace-decl-decls decl)
                                 (append namespaces
                                         (list (namespace-decl-name decl))))])
                 (loop (cdr decls) (append (reverse ns-forms) forms)))]

              ;; Class - generate class FFI forms
              [(class-decl? decl)
               (let ([class-forms (class->ffi-forms decl namespaces)])
                 (loop (cdr decls) (append (reverse class-forms) forms)))]

              ;; Template - check if it's a specialization
              [(template-decl? decl)
               (if (null? (template-decl-params decl))
                   ;; Empty params = full specialization (template<> class Foo<int>)
                   ;; Treat as concrete type and generate FFI
                   (let* ([inner-pair (template-decl-decl decl)]
                          [inner-decl (if (pair? inner-pair) (cdr inner-pair) inner-pair)])
                     (cond
                       [(class-decl? inner-decl)
                        (let ([class-forms (class->ffi-forms inner-decl namespaces)])
                          (loop (cdr decls) (append (reverse class-forms) forms)))]
                       [(function-decl? inner-decl)
                        (let ([form (function->ffi-form inner-decl namespaces #f)])
                          (loop (cdr decls) (cons form forms)))]
                       [else
                        ;; Other specializations - skip for now
                        (loop (cdr decls)
                              (cons `(comment ,(format "Skipping specialized: ~a"
                                                       (template-summary decl)))
                                    forms))]))
                   ;; Non-empty params = generic template, skip
                   (loop (cdr decls)
                         (cons `(comment ,(format "Skipping template: ~a"
                                                  (template-summary decl)))
                               forms)))]

              ;; Function - generate with mangled name
              [(function-decl? decl)
               (let ([form (function->ffi-form decl namespaces #f)])
                 (loop (cdr decls) (cons form forms)))]

              ;; Other declarations - use base FFI generator
              [(or (typedef? decl) (struct-decl? decl)
                   (union-decl? decl) (enum-decl? decl))
               (let ([form (declaration->ffi-form decl "lib")])
                 (loop (cdr decls) (cons form forms)))]

              ;; Unknown
              [else
               (loop (cdr decls) forms)])))))

  ;; Check if a function declaration is a constructor
  (define (constructor? decl class-name)
    (eq? (function-decl-name decl) class-name))

  ;; Check if a function declaration is a destructor
  (define (destructor? decl class-name)
    (let ([name (function-decl-name decl)])
      (and (symbol? name)
           (let ([str (symbol->string name)])
             (and (> (string-length str) 1)
                  (char=? (string-ref str 0) #\~))))))

  ;; Generate FFI forms for a class
  (define (class->ffi-forms decl namespaces)
    (let* ([class-name (class-decl-name decl)]
           [members (class-decl-members decl)]
           [ftype-name (make-class-ftype-name class-name namespaces)]
           ;; Keep full member-decl to access specifiers
           [public-fn-members (filter
                                (lambda (m)
                                  (and (eq? (member-decl-access m) 'public)
                                       (function-decl? (member-decl-decl m))))
                                members)]
           [ctors (filter (lambda (m) (constructor? (member-decl-decl m) class-name))
                          public-fn-members)]
           [dtors (filter (lambda (m) (destructor? (member-decl-decl m) class-name))
                          public-fn-members)]
           [methods (filter (lambda (m)
                              (let ([fn (member-decl-decl m)])
                                (and (not (constructor? fn class-name))
                                     (not (destructor? fn class-name)))))
                            public-fn-members)])

      (append
        (list `(define-ftype ,ftype-name void*))
        (map (lambda (m) (constructor->ffi-form (member-decl-decl m) class-name namespaces))
             ctors)
        (map (lambda (m) (destructor->ffi-form (member-decl-decl m) class-name namespaces))
             dtors)
        (filter-map (lambda (m) (method->ffi-form-with-specifiers m class-name namespaces))
                    methods))))

  ;; Generate FFI form for a constructor
  ;; Generates raw constructor binding only - allocation is caller's responsibility
  ;; For full object creation, use companion make-* function or extern "C" factory
  (define (constructor->ffi-form decl class-name namespaces)
    (let* ([params (function-decl-params decl)]
           [variadic? (function-decl-variadic? decl)]
           [mangled (mangle-constructor class-name namespaces params 'complete)]
           [scheme-name (make-constructor-scheme-name class-name namespaces params)]
           ;; Constructor takes 'this' pointer + params, returns void
           [ffi-params (cons 'void*
                             (map (lambda (p)
                                    (cpp-type->ffi-type (param-type p)))
                                  params))])

      (if variadic?
          `(comment ,(format "Skipping variadic constructor: ~a" class-name))
          `(define ,scheme-name
             (foreign-procedure lib ,mangled ,ffi-params void)))))

  ;; Generate FFI form for a destructor
  ;; Generates raw destructor binding only - deallocation is caller's responsibility
  (define (destructor->ffi-form decl class-name namespaces)
    (let* ([mangled (mangle-destructor class-name namespaces 'complete)]
           [scheme-name (make-destructor-scheme-name class-name namespaces)])

      `(define ,scheme-name
         (foreign-procedure lib ,mangled (void*) void))))

  ;; Generate FFI form for a method with specifiers
  ;; member is a member-decl containing both the function-decl and specifiers
  (define (method->ffi-form-with-specifiers member class-name namespaces)
    (let* ([decl (member-decl-decl member)]
           [specifiers (member-decl-specifiers member)]
           [static? (memq 'static specifiers)]
           [const? (memq 'const specifiers)]
           [name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [variadic? (function-decl-variadic? decl)]
           [mangled (mangle-method decl class-name namespaces const?)]
           [scheme-name (make-method-scheme-name name class-name namespaces)]
           [ffi-return (cpp-type->ffi-type return-type)]
           ;; Static methods don't have 'this' pointer
           [ffi-params (if static?
                           (map (lambda (p)
                                  (cpp-type->ffi-type (param-type p)))
                                params)
                           (cons 'void*  ;; this pointer for non-static
                                 (map (lambda (p)
                                        (cpp-type->ffi-type (param-type p)))
                                      params)))])

      (if variadic?
          `(comment ,(format "Skipping variadic method: ~a::~a" class-name name))
          `(define ,scheme-name
             (foreign-procedure lib ,mangled ,ffi-params ,ffi-return)))))

  ;; Generate FFI form for a method (legacy, without specifiers)
  (define (method->ffi-form decl class-name namespaces)
    (let* ([name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [variadic? (function-decl-variadic? decl)]
           [mangled (mangle-method decl class-name namespaces #f)]
           [scheme-name (make-method-scheme-name name class-name namespaces)]
           [ffi-return (cpp-type->ffi-type return-type)]
           ;; Add implicit 'this' pointer as first parameter
           [ffi-params (cons 'void*  ;; this pointer
                             (map (lambda (p)
                                    (cpp-type->ffi-type (param-type p)))
                                  params))])

      (if variadic?
          `(comment ,(format "Skipping variadic method: ~a::~a" class-name name))
          `(define ,scheme-name
             (foreign-procedure lib ,mangled ,ffi-params ,ffi-return)))))

  ;; Generate FFI form for a free function
  (define (function->ffi-form decl namespaces extern-c?)
    (let* ([name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [variadic? (function-decl-variadic? decl)]
           ;; Use mangled name unless extern "C"
           [foreign-name (if extern-c?
                             (symbol->string name)
                             (mangle-function decl namespaces))]
           [scheme-name (make-function-scheme-name name namespaces)]
           [ffi-return (cpp-type->ffi-type return-type)]
           [ffi-params (map (lambda (p)
                              (cpp-type->ffi-type (param-type p)))
                            params)])

      (if variadic?
          `(comment ,(format "Skipping variadic function: ~a" name))
          `(define ,scheme-name
             (foreign-procedure lib ,foreign-name ,ffi-params ,ffi-return)))))

  ;; Convert C++ type to FFI type
  (define (cpp-type->ffi-type type)
    (cond
      ;; Qualified types (const, volatile) - unwrap and process inner type
      [(qualified-type? type)
       (cpp-type->ffi-type (qualified-type-type type))]

      ;; Reference types become pointers
      [(reference-type? type)
       (let ([referent (unwrap-qualifiers (reference-type-referent type))])
         ;; If referent is a class/struct type, use void*
         ;; Otherwise, use pointer to the underlying type
         (if (class-like-type? referent)
             'void*  ;; Class/struct reference -> void*
             (list '* (cpp-type->ffi-type referent))))]

      ;; Pointer types - check if pointing to a class
      [(pointer-type? type)
       (let ([pointee (unwrap-qualifiers (pointer-type-pointee type))])
         (cond
           ;; void* stays void*
           [(and (basic-type? pointee)
                 (eq? (basic-type-name pointee) 'void))
            'void*]
           ;; char* -> string
           [(and (basic-type? pointee)
                 (eq? (basic-type-name pointee) 'char))
            'string]
           ;; Class pointer -> void*
           [(class-like-type? pointee)
            'void*]
           ;; Other pointers
           [else
            (list '* (cpp-type->ffi-type pointee))]))]

      ;; Template types - use void* for now
      [(template-type? type)
       'void*]

      ;; Qualified names - use void* for complex types
      [(and (named-type? type)
            (qualified-name? (named-type-name type)))
       'void*]

      ;; Named types that are class-like -> void*
      [(class-like-type? type)
       'void*]

      ;; Use base FFI type conversion
      [else
       (ast->ffi-type type)]))

  ;; Unwrap qualified-type wrappers to get the underlying type
  (define (unwrap-qualifiers type)
    (if (qualified-type? type)
        (unwrap-qualifiers (qualified-type-type type))
        type))

  ;; Check if a type is class-like (not a basic type or typedef to basic)
  (define (class-like-type? type)
    (and (named-type? type)
         (let ([name (named-type-name type)])
           (and (symbol? name)
                (not (basic-type-name? name))))))

  ;; Check if a name is a basic type
  (define (basic-type-name? name)
    (memq name '(void char short int long float double
                 signed unsigned bool wchar_t
                 int8_t int16_t int32_t int64_t
                 uint8_t uint16_t uint32_t uint64_t
                 size_t ssize_t ptrdiff_t)))

  ;; Generate Scheme name for a class ftype
  (define (make-class-ftype-name class-name namespaces)
    (string->symbol
      (call-with-string-output-port
        (lambda (out)
          (for-each (lambda (ns)
                      (put-string out (symbol->string ns))
                      (put-char out #\-))
                    namespaces)
          (put-string out (symbol->string class-name))
          (put-string out "-ptr")))))

  ;; Generate Scheme name for a method
  ;; Converts C++ operators to friendly Scheme names
  (define (make-method-scheme-name name class-name namespaces)
    (string->symbol
      (call-with-string-output-port
        (lambda (out)
          (for-each (lambda (ns)
                      (put-string out (symbol->string ns))
                      (put-char out #\-))
                    namespaces)
          (put-string out (symbol->string class-name))
          (put-char out #\-)
          (put-string out (operator->scheme-name (symbol->string name)))))))

  ;; Convert C++ operator name to Scheme-friendly name
  ;; e.g., operator+ -> add, operator== -> equal?, operator[] -> ref
  (define (operator->scheme-name name)
    (cond
      [(string=? name "operator+") "add"]
      [(string=? name "operator-") "sub"]
      [(string=? name "operator*") "mul"]
      [(string=? name "operator/") "div"]
      [(string=? name "operator%") "mod"]
      [(string=? name "operator^") "xor"]
      [(string=? name "operator&") "bitand"]
      [(string=? name "operator|") "bitor"]
      [(string=? name "operator~") "bitnot"]
      [(string=? name "operator!") "not"]
      [(string=? name "operator=") "set!"]
      [(string=? name "operator<") "lt?"]
      [(string=? name "operator>") "gt?"]
      [(string=? name "operator<=") "le?"]
      [(string=? name "operator>=") "ge?"]
      [(string=? name "operator==") "equal?"]
      [(string=? name "operator!=") "not-equal?"]
      [(string=? name "operator+=") "add!"]
      [(string=? name "operator-=") "sub!"]
      [(string=? name "operator*=") "mul!"]
      [(string=? name "operator/=") "div!"]
      [(string=? name "operator%=") "mod!"]
      [(string=? name "operator<<") "shl"]
      [(string=? name "operator>>") "shr"]
      [(string=? name "operator&&") "and"]
      [(string=? name "operator||") "or"]
      [(string=? name "operator++") "inc!"]
      [(string=? name "operator--") "dec!"]
      [(string=? name "operator->") "deref"]
      [(string=? name "operator->*") "deref-member"]
      [(string=? name "operator()") "call"]
      [(string=? name "operator[]") "ref"]
      [(string=? name "operator-index") "ref"]  ;; our internal name
      [(string=? name "operator<=>") "compare"]
      [else name]))

  ;; Generate Scheme name for a free function
  (define (make-function-scheme-name name namespaces)
    (string->symbol
      (call-with-string-output-port
        (lambda (out)
          (for-each (lambda (ns)
                      (put-string out (symbol->string ns))
                      (put-char out #\-))
                    namespaces)
          (put-string out (symbol->string name))))))

  ;; Generate Scheme name for a constructor
  ;; For overloaded constructors, append type signature
  (define (make-constructor-scheme-name class-name namespaces params)
    (string->symbol
      (call-with-string-output-port
        (lambda (out)
          (put-string out "make-")
          (for-each (lambda (ns)
                      (put-string out (symbol->string ns))
                      (put-char out #\-))
                    namespaces)
          (put-string out (symbol->string class-name))
          ;; Add type suffix for overloaded constructors
          (unless (null? params)
            (put-char out #\/)
            (put-string out (params->suffix params)))))))

  ;; Generate Scheme name for a destructor
  (define (make-destructor-scheme-name class-name namespaces)
    (string->symbol
      (call-with-string-output-port
        (lambda (out)
          (for-each (lambda (ns)
                      (put-string out (symbol->string ns))
                      (put-char out #\-))
                    namespaces)
          (put-string out (symbol->string class-name))
          (put-string out "-destroy")))))

  ;; Generate type suffix from parameters (for overload disambiguation)
  (define (params->suffix params)
    (call-with-string-output-port
      (lambda (out)
        (let ([first? #t])
          (for-each
            (lambda (p)
              (unless first?
                (put-char out #\-))
              (set! first? #f)
              (put-string out (type->suffix (param-type p))))
            params)))))

  ;; Convert type to short suffix string
  (define (type->suffix type)
    (cond
      [(basic-type? type)
       (symbol->string (basic-type-name type))]
      [(pointer-type? type)
       (string-append (type->suffix (pointer-type-pointee type)) "*")]
      [(reference-type? type)
       (string-append (type->suffix (reference-type-referent type)) "&")]
      [(named-type? type)
       (let ([name (named-type-name type)])
         (if (symbol? name)
             (symbol->string name)
             "obj"))]
      [else "?"]))

  ;; Get template summary for comments
  (define (template-summary decl)
    (let ([inner (template-decl-decl decl)])
      (cond
        [(function-decl? inner) (function-decl-name inner)]
        [(class-decl? inner) (class-decl-name inner)]
        [else "unknown"])))

  ;; Extract exports from generated forms
  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              [(and (pair? form) (eq? (car form) 'define))
               (loop (cdr forms) (cons (cadr form) exports))]
              [(and (pair? form) (eq? (car form) 'define-ftype))
               (loop (cdr forms) (cons (cadr form) exports))]
              [(and (pair? form) (eq? (car form) 'begin))
               (loop (append (cdr form) (cdr forms)) exports)]
              [else
               (loop (cdr forms) exports)])))))

  ;; Helper: filter-map
  (define (filter-map proc lst)
    (let loop ([lst lst] [result '()])
      (if (null? lst)
          (reverse result)
          (let ([val (proc (car lst))])
            (if val
                (loop (cdr lst) (cons val result))
                (loop (cdr lst) result))))))

  ;; Single declaration conversion (for testing)
  (define (cpp-declaration->ffi-form decl namespaces)
    (cond
      [(namespace-decl? decl)
       `(begin ,@(collect-ffi-forms
                   (namespace-decl-decls decl)
                   (append namespaces (list (namespace-decl-name decl)))))]
      [(class-decl? decl)
       `(begin ,@(class->ffi-forms decl namespaces))]
      [(function-decl? decl)
       (function->ffi-form decl namespaces #f)]
      [else
       (declaration->ffi-form decl "lib")])))
