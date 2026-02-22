;; SPDX-License-Identifier: WTFPL
;; FFI Code Generator - AST to Chez Scheme FFI
;; Generates foreign-procedure and foreign-struct definitions

(library (c-tools codegen chez ffi)
  (export generate-ffi-code
          ast->ffi-type
          declaration->ffi-form)
  (import (rnrs base)
          (rnrs control)
          (rnrs hashtables)
          (rnrs io simple)
          (rnrs lists)
          (c-tools ast c)
          (only (c-tools utility) format symbol-append ormap))

  ;;=======================================================================
  ;; Type Mapping: C types to Chez Scheme FFI types

  ;; Global typedef resolution table
  (define *typedef-table* (make-eq-hashtable))

  ;; Build typedef resolution table from declarations
  (define (build-typedef-table! declarations)
    (for-each (lambda (decl)
               (when (typedef? decl)
                 (hashtable-set! *typedef-table*
                                (typedef-name decl)
                                (typedef-type decl))))
             declarations))

  ;; Resolve typedef name to underlying type
  (define (resolve-typedef name)
    (hashtable-ref *typedef-table* name #f))

  ;; Convert C basic type to Chez FFI type
  (define (basic-type->ffi type-name)
    (case type-name
      [(void) 'void]
      [(char) 'char]
      [(signed-char) 'char]
      [(unsigned-char) 'unsigned-8]
      [(short) 'short]
      [(unsigned-short) 'unsigned-short]
      [(int) 'int]
      [(unsigned) 'unsigned]
      [(long) 'long]
      [(unsigned-long) 'unsigned-long]
      [(long-long) 'long-long]
      [(unsigned-long-long) 'unsigned-long-long]
      [(float) 'float]
      [(double) 'double]
      ;; Boolean types (from stdbool.h)
      [(bool _Bool) 'boolean]
      ;; stdint.h types
      [(int8_t) 'integer-8]
      [(uint8_t) 'unsigned-8]
      [(int16_t) 'integer-16]
      [(uint16_t) 'unsigned-16]
      [(int32_t) 'integer-32]
      [(uint32_t) 'unsigned-32]
      [(int64_t) 'integer-64]
      [(uint64_t) 'unsigned-64]
      ;; Common platform-specific types
      [(size_t) 'size_t]
      [(ssize_t) 'ssize_t]
      [(ptrdiff_t) 'ptrdiff_t]
      [(intptr_t) 'iptr]
      [(uintptr_t) 'uptr]
      [else
       ;; Unknown basic type - treat as int
       'int]))

  ;; Convert AST type to Chez FFI type specification
  ;; context: 'function (default) or 'struct
  ;;   In function context, char* maps to 'string
  ;;   In struct context, char* maps to 'u8*
  (define ast->ffi-type
    (case-lambda
      [(type) (ast->ffi-type type 'function)]
      [(type context)
       (cond
         ;; Basic type
         [(basic-type? type)
          (basic-type->ffi (basic-type-name type))]

         ;; Pointer type
         [(pointer-type? type)
          (let ([pointee (pointer-type-pointee type)])
            ;; Unwrap qualified types (const, volatile)
            (let ([unwrapped (if (qualified-type? pointee)
                                (qualified-type-type pointee)
                                pointee)])
              (cond
                ;; char* -> string in function context, (* unsigned-8) in struct context
                [(and (basic-type? unwrapped)
                      (eq? (basic-type-name unwrapped) 'char))
                 (if (eq? context 'function) 'string '(* unsigned-8))]
                ;; void* -> void* in both contexts
                [(and (basic-type? unwrapped)
                      (eq? (basic-type-name unwrapped) 'void))
                 'void*]
                ;; Pointer to pointer -> void* (nested * not supported)
                [(pointer-type? unwrapped)
                 'void*]
                ;; struct foo* -> (* struct-foo)
                [(named-type? unwrapped)
                 (case (named-type-kind unwrapped)
                   [(struct)
                    (list '* (symbol-append 'struct- (named-type-name unwrapped)))]
                   [(union)
                    (list '* (symbol-append 'union- (named-type-name unwrapped)))]
                   [else
                    (list '* (ast->ffi-type pointee context))])]
                ;; Other pointers: (* T)
                [else
                 (list '* (ast->ffi-type pointee context))])))]

         ;; Named type (struct, union, enum, typedef)
         [(named-type? type)
          (case (named-type-kind type)
            [(struct)
             ;; Reference to struct type by name
             (symbol-append 'struct- (named-type-name type))]
            [(union)
             ;; Reference to union type by name
             (symbol-append 'union- (named-type-name type))]
            [(enum)
             ;; Reference to enum type by name
             (symbol-append 'enum- (named-type-name type))]
            [(typedef)
             ;; Check if it's a stdint type or other known typedef - map directly to Chez type
             (let ([name (named-type-name type)])
               (case name
                 ;; stdbool.h types
                 [(bool) 'boolean]
                 ;; stdint.h types
                 [(int8_t) 'integer-8]
                 [(uint8_t) 'unsigned-8]
                 [(int16_t) 'integer-16]
                 [(uint16_t) 'unsigned-16]
                 [(int32_t) 'integer-32]
                 [(uint32_t) 'unsigned-32]
                 [(int64_t) 'integer-64]
                 [(uint64_t) 'unsigned-64]
                 [(size_t) 'size_t]
                 [(ssize_t) 'ssize_t]
                 [(ptrdiff_t) 'ptrdiff_t]
                 [(intptr_t) 'iptr]
                 [(uintptr_t) 'uptr]
                 ;; Other typedefs - resolve in struct context to avoid circular deps
                 [else
                  (if (eq? context 'struct)
                      ;; In struct context, resolve typedef to underlying type
                      (let ([resolved (resolve-typedef name)])
                        (if resolved
                            (ast->ffi-type resolved context)
                            name))
                      ;; In function context, use typedef name directly
                      name)]))])]

         ;; Qualified type (const, volatile)
         [(qualified-type? type)
          ;; Ignore qualifiers in FFI
          (ast->ffi-type (qualified-type-type type) context)]

         ;; Array type
         [(array-type? type)
          ;; Arrays decay to pointers in FFI
          (list '* (ast->ffi-type (array-type-element type) context))]

         ;; Function type (function pointers)
         [(function-type? type)
          ;; Function pointers are just void* for now
          'void*]

         ;; Unknown type
         [else 'void*])]))

  ;;; Declaration to FFI Form Generation

  ;; Check if a type is a struct/union passed by value
  (define (struct-by-value? type)
    (cond
      [(named-type? type)
       (case (named-type-kind type)
         [(struct union) #t]
         [(typedef)
          ;; Resolve typedef and check
          (let ([underlying (resolve-typedef (named-type-name type))])
            (and underlying (struct-by-value? underlying)))]
         [else #f])]
      [(qualified-type? type)
       (struct-by-value? (qualified-type-type type))]
      [else #f]))

  ;; Generate foreign-procedure form from function-decl
  (define (function-decl->ffi-form decl lib-name)
    (let* ([name (function-decl-name decl)]
           [return-type (function-decl-return-type decl)]
           [params (function-decl-params decl)]
           [variadic? (function-decl-variadic? decl)]
           [has-struct-by-value? (or (struct-by-value? return-type)
                                     (ormap (lambda (p) (struct-by-value? (param-type p)))
                                            params))]
           [ffi-return (ast->ffi-type return-type)]
           [ffi-params (map (lambda (p) (ast->ffi-type (param-type p)))
                           params)]
           [scheme-name (symbol-append 'c- name)])

      (cond
        [variadic?
         ;; Variadic functions need special handling - skip for now
         `(comment ,(format "Skipping variadic function: ~a" name))]
        [has-struct-by-value?
         ;; Chez FFI cannot pass structs by value
         `(comment ,(format "Skipping function with struct/union by value: ~a" name))]
        [else
         ;; Regular function
         `(define ,scheme-name
            (foreign-procedure ,(symbol->string name)
                             ,ffi-params ,ffi-return))])))

  ;; Check if a struct/union is defined
  (define (is-type-defined? name declarations)
    (let loop ([decls declarations])
      (cond
        [(null? decls) #f]
        [(and (struct-decl? (car decls))
              (eq? (struct-decl-name (car decls)) name)
              (pair? (struct-decl-fields (car decls))))  ;; Has fields = defined
         #t]
        [(and (union-decl? (car decls))
              (eq? (union-decl-name (car decls)) name)
              (pair? (union-decl-fields (car decls))))
         #t]
        [else (loop (cdr decls))])))

  ;; Global declaration list (set during code generation)
  (define *all-declarations* '())

  ;; Convert type for typedef
  ;; Uses struct context since typedefs can appear in struct field positions
  (define (typedef-type->ffi type)
    (ast->ffi-type type 'struct))

  ;; Generate typedef form
  (define (typedef->ffi-form decl)
    (let ([name (typedef-name decl)]
          [type (typedef-type decl)])
      `(define-ftype ,name ,(typedef-type->ffi type))))

  ;; Generate struct definition form using define-ftype
  (define (struct-decl->ffi-form decl)
    (let ([name (struct-decl-name decl)]
          [fields (struct-decl-fields decl)])
      (let ([ftype-name (symbol-append 'struct- name)])
        (if (null? fields)
            ;; Opaque/incomplete type - define as empty struct
            `(define-ftype ,ftype-name (struct))
            ;; Complete type with fields
            (let ([field-specs (map (lambda (field)
                                     (list (field-name field)
                                           (ast->ffi-type (field-type field) 'struct)))
                                   fields)])
              `(define-ftype ,ftype-name
                 (struct ,@field-specs)))))))

  ;; Generate union definition form using define-ftype
  (define (union-decl->ffi-form decl)
    (let ([name (union-decl-name decl)]
          [fields (union-decl-fields decl)])
      (let ([ftype-name (symbol-append 'union- name)])
        (if (null? fields)
            ;; Opaque/incomplete type - define as empty struct
            `(define-ftype ,ftype-name (struct))
            ;; Complete type with fields
            (let ([field-specs (map (lambda (field)
                                     (list (field-name field)
                                           (ast->ffi-type (field-type field) 'struct)))
                                   fields)])
              `(define-ftype ,ftype-name
                 (union ,@field-specs)))))))

  ;; Generate enum constant definitions (ftype is handled separately)
  (define (enum-decl->ffi-form decl)
    (let ([enumerators (enum-decl-enumerators decl)])
      ;; Generate constant definitions
      (if (null? enumerators)
          #f
          (cons 'begin
                (map (lambda (e)
                       `(define ,(enumerator-name e) ,(enumerator-value e)))
                     enumerators)))))

  ;; Convert a single declaration to FFI form
  ;; Returns #f if no form should be generated (e.g., typedefs)
  (define (declaration->ffi-form decl lib-name)
    (cond
      [(function-decl? decl)
       (function-decl->ffi-form decl lib-name)]
      [(typedef? decl)
       (typedef->ffi-form decl)]
      [(struct-decl? decl)
       (struct-decl->ffi-form decl)]
      [(union-decl? decl)
       (union-decl->ffi-form decl)]
      [(enum-decl? decl)
       (enum-decl->ffi-form decl)]
      [else
       #f]))

  ;;=======================================================================
  ;; Topological Sorting

  ;; Extract type names that a declaration depends on
  ;; Returns list of declaration names (using prefixed names for structs/unions)
  (define (declaration-dependencies decl)
    (cond
      [(function-decl? decl)
       (let* ([ret-type (function-decl-return-type decl)]
              [params (function-decl-params decl)]
              [ret-deps (type-dependencies ret-type)]
              [param-deps (apply append (map (lambda (p)
                                              (type-dependencies (param-type p)))
                                            params))])
         (append ret-deps param-deps))]

      [(typedef? decl)
       (type-dependencies (typedef-type decl))]

      [(struct-decl? decl)
       (if (null? (struct-decl-fields decl))
           '()  ;; Opaque struct has no dependencies
           (apply append (map (lambda (f)
                               (type-dependencies (field-type f)))
                             (struct-decl-fields decl))))]

      [(union-decl? decl)
       (apply append (map (lambda (f)
                           (type-dependencies (field-type f)))
                         (union-decl-fields decl)))]

      [(enum-decl? decl)
       '()]  ;; Enums have no type dependencies

      [else '()]))

  ;; Extract type names from a type  ;; Returns declaration names (with struct-/union-/enum- prefix where applicable)
  (define (type-dependencies type)
    (cond
      [(basic-type? type) '()]

      [(pointer-type? type)
       (type-dependencies (pointer-type-pointee type))]

      [(named-type? type)
       (case (named-type-kind type)
         [(struct) (list (symbol-append 'struct- (named-type-name type)))]
         [(union) (list (symbol-append 'union- (named-type-name type)))]
         [(enum) (list (symbol-append 'enum- (named-type-name type)))]
         [(typedef)
          ;; Resolve typedef to get actual dependencies
          (let ([resolved (resolve-typedef (named-type-name type))])
            (if resolved
                (type-dependencies resolved)
                (list (named-type-name type))))]
         [else (list (named-type-name type))])]

      [(qualified-type? type)
       (type-dependencies (qualified-type-type type))]

      [(array-type? type)
       (type-dependencies (array-type-element type))]

      [(function-type? type)
       '()]  ;; Function pointers simplified to void*

      [else '()]))

  ;; Get name defined by a declaration
  ;; Use prefixed names for structs/unions/enums to avoid collision with typedefs
  (define (declaration-name decl)
    (cond
      [(function-decl? decl) (function-decl-name decl)]
      [(typedef? decl) (typedef-name decl)]
      [(struct-decl? decl)
       (let ([name (struct-decl-name decl)])
         (and name (symbol-append 'struct- name)))]
      [(union-decl? decl)
       (let ([name (union-decl-name decl)])
         (and name (symbol-append 'union- name)))]
      [(enum-decl? decl)
       (let ([name (enum-decl-name decl)])
         (and name (symbol-append 'enum- name)))]
      [else #f]))

  ;; Topologically sort declarations
  ;; Returns declarations in dependency order (dependencies first)
  (define (topological-sort declarations)
    (let* ([decl-map (make-decl-map declarations)]
           [visited (make-eq-hashtable)]
           [result '()])

      (define (visit decl)
        (let ([name (declaration-name decl)])
          (when name
            (unless (hashtable-ref visited name #f)
              (hashtable-set! visited name #t)

              ;; Visit dependencies first
              (let ([deps (declaration-dependencies decl)])
                (for-each (lambda (dep-name)
                           (let ([dep-decl (hashtable-ref decl-map dep-name #f)])
                             (when dep-decl
                               (visit dep-decl))))
                         deps))

              ;; Add this declaration to result
              (set! result (cons decl result))))))

      ;; Visit all declarations
      (for-each visit declarations)

      ;; Return in reverse order (dependencies first)
      (reverse result)))

  ;; Build map from type name to declaration
  (define (make-decl-map declarations)
    (let ([map (make-eq-hashtable)])
      (for-each (lambda (decl)
                 (let ([name (declaration-name decl)])
                   (when name
                     (hashtable-set! map name decl))))
               declarations)
      map))

  ;;=======================================================================
  ;; Top-level API

  ;; Collect all struct/union names referenced in types
  (define (collect-referenced-types declarations)
    (let ([refs (make-eq-hashtable)])
      (define (collect-from-type type)
        (cond
          [(pointer-type? type)
           (collect-from-type (pointer-type-pointee type))]
          [(named-type? type)
           (case (named-type-kind type)
             [(struct union)
              (hashtable-set! refs (named-type-name type) #t)]
             [(typedef)
              (let ([underlying (resolve-typedef (named-type-name type))])
                (when underlying
                  (collect-from-type underlying)))])]
          [(qualified-type? type)
           (collect-from-type (qualified-type-type type))]
          [(array-type? type)
           (collect-from-type (array-type-element type))]))

      (for-each (lambda (decl)
                 (cond
                   [(function-decl? decl)
                    (collect-from-type (function-decl-return-type decl))
                    (for-each (lambda (p)
                               (collect-from-type (param-type p)))
                             (function-decl-params decl))]
                   [(typedef? decl)
                    (collect-from-type (typedef-type decl))]
                   [(struct-decl? decl)
                    (for-each (lambda (f)
                               (collect-from-type (field-type f)))
                             (struct-decl-fields decl))]
                   [(union-decl? decl)
                    (for-each (lambda (f)
                               (collect-from-type (field-type f)))
                             (union-decl-fields decl))]))
               declarations)
      refs))

  ;; Find struct/union names that are referenced but not defined
  (define (find-undefined-types declarations)
    (let ([referenced (collect-referenced-types declarations)]
          [defined (make-eq-hashtable)])
      ;; Collect defined struct/union names
      (for-each (lambda (decl)
                 (cond
                   [(struct-decl? decl)
                    (hashtable-set! defined (struct-decl-name decl) 'struct)]
                   [(union-decl? decl)
                    (hashtable-set! defined (union-decl-name decl) 'union)]))
               declarations)
      ;; Find referenced but not defined
      (let ([undefined '()])
        (vector-for-each
          (lambda (name)
            (unless (hashtable-ref defined name #f)
              (set! undefined (cons name undefined))))
          (hashtable-keys referenced))
        undefined)))

  ;; Generate opaque type definitions
  (define (generate-opaque-types undefined-names)
    (map (lambda (name)
          `(define-ftype ,(symbol-append 'struct- name) (struct)))
        undefined-names))

  ;; Generate struct/union ftype bindings for multi-def form
  (define (struct-decl->ftype-binding decl)
    (let ([name (struct-decl-name decl)]
          [fields (struct-decl-fields decl)])
      (let ([ftype-name (symbol-append 'struct- name)])
        (if (null? fields)
            ;; Opaque - empty struct
            (list ftype-name '(struct))
            ;; Complete struct
            (let ([field-specs (map (lambda (field)
                                     (list (field-name field)
                                           (ast->ffi-type (field-type field) 'struct)))
                                   fields)])
              (list ftype-name (cons 'struct field-specs)))))))

  (define (union-decl->ftype-binding decl)
    (let ([name (union-decl-name decl)]
          [fields (union-decl-fields decl)])
      (let ([ftype-name (symbol-append 'union- name)])
        (if (null? fields)
            ;; Opaque - empty struct
            (list ftype-name '(struct))
            ;; Complete union
            (let ([field-specs (map (lambda (field)
                                     (list (field-name field)
                                           (ast->ffi-type (field-type field) 'struct)))
                                   fields)])
              (list ftype-name (cons 'union field-specs)))))))

  (define (enum-decl->ftype-binding decl)
    (let ([name (enum-decl-name decl)])
      (if name
          (list (symbol-append 'enum- name) 'int)
          #f)))

  ;; Sort struct/union bindings topologically
  ;; Forward references are only allowed in pointer fields
  (define (sort-ftype-bindings bindings)
    (let ([binding-map (make-eq-hashtable)]
          [visited (make-eq-hashtable)]
          [result '()])

      ;; Extract non-pointer dependencies from a field spec
      (define (field-deps spec)
        (cond
          [(symbol? spec) (list spec)]
          [(and (pair? spec) (eq? (car spec) '*))
           '()]  ;; Pointers don't create ordering deps
          [(pair? spec)
           (apply append (map field-deps spec))]
          [else '()]))

      ;; Visit binding in topological order
      (define (visit binding)
        (let ([name (car binding)])
          (unless (hashtable-ref visited name #f)
            (hashtable-set! visited name #t)
            (let* ([type-spec (cadr binding)]
                   [fields (if (and (pair? type-spec)
                                   (memq (car type-spec) '(struct union)))
                              (cdr type-spec)
                              '())]
                   [deps (apply append (map (lambda (field)
                                             (field-deps (cadr field)))
                                           fields))])
              ;; Visit dependencies first
              (for-each (lambda (dep)
                         (let ([dep-binding (hashtable-ref binding-map dep #f)])
                           (when dep-binding
                             (visit dep-binding))))
                       deps)
              ;; Add this binding
              (set! result (cons binding result))))))

      ;; Build map from name to binding
      (for-each (lambda (binding)
                 (hashtable-set! binding-map (car binding) binding))
               bindings)

      ;; Visit all bindings
      (for-each visit bindings)
      (reverse result)))

  ;; Generate complete FFI code from list of declarations
  (define (generate-ffi-code declarations lib-name)
    ;;   Generate FFI bindings for a list of C declarations
    ;; Set global declaration list for opaque type checking
    (set! *all-declarations* declarations)
    ;; Build typedef resolution table
    (build-typedef-table! declarations)
    (let* ([undefined (find-undefined-types declarations)]
           ;; Group declarations by type
           [struct-decls (filter struct-decl? declarations)]
           [union-decls (filter union-decl? declarations)]
           [enum-decls (filter enum-decl? declarations)]
           [other-decls (filter (lambda (d)
                                 (not (or (struct-decl? d)
                                         (union-decl? d)
                                         (enum-decl? d))))
                               declarations)]
           ;; Generate opaque types for undefined references
           [opaque-bindings (map (lambda (name)
                                  (list (symbol-append 'struct- name) '(struct)))
                                undefined)]
           ;; Generate struct/union/enum bindings for multi-def form
           [struct-bindings (map struct-decl->ftype-binding struct-decls)]
           [union-bindings (map union-decl->ftype-binding union-decls)]
           [enum-bindings (filter (lambda (b) b)
                                 (map enum-decl->ftype-binding enum-decls))]
           [all-type-bindings (sort-ftype-bindings
                                (append opaque-bindings enum-bindings
                                       struct-bindings union-bindings))]
           ;; Generate multi-def ftype form if we have types
           [types-form (if (null? all-type-bindings)
                          '()
                          (list (cons 'define-ftype all-type-bindings)))]
           ;; Generate enum constant definitions and other declarations
           [enum-forms (map (lambda (decl) (enum-decl->ffi-form decl)) enum-decls)]
           [sorted-other (topological-sort other-decls)]
           [other-forms (map (lambda (decl) (declaration->ffi-form decl lib-name))
                            sorted-other)]
           ;; Filter out #f and comment forms
           [forms (filter (lambda (f)
                           (and f
                                (not (and (pair? f) (eq? (car f) 'comment)))))
                         (append enum-forms other-forms))]
           [all-forms (append types-form forms)])
      ;; Wrap in a library form
      `(library (ffi ,(string->symbol lib-name))
         (export ,@(extract-exports all-forms))
         (import (chezscheme))

         ,@all-forms)))

  ;; Extract exported identifiers from generated forms
  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              ;; define form - export the name
              [(and (pair? form) (eq? (car form) 'define))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; define-ftype form with single type
              [(and (pair? form) (eq? (car form) 'define-ftype)
                    (symbol? (cadr form)))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; define-ftype form with multiple bindings
              [(and (pair? form) (eq? (car form) 'define-ftype)
                    (pair? (cadr form)))
               ;; Extract all type names from bindings
               (let ([type-names (map car (cdr form))])
                 (loop (cdr forms) (append (reverse type-names) exports)))]
              ;; begin form - recurse
              [(and (pair? form) (eq? (car form) 'begin))
               (loop (append (cdr form) (cdr forms)) exports)]
              ;; Other forms - skip
              [else
               (loop (cdr forms) exports)])))))

)
