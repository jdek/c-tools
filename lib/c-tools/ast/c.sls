;; C AST - Abstract Syntax Tree for C Declarations
;; Represents parsed C declarations for FFI generation

(library (c-tools ast c)
  (export ;; Type constructors
          make-basic-type basic-type? basic-type-name
          make-pointer-type pointer-type? pointer-type-pointee
          make-array-type array-type? array-type-element array-type-size
          make-function-type function-type? function-type-return function-type-params function-type-variadic?
          make-named-type named-type? named-type-kind named-type-name
          make-qualified-type qualified-type? qualified-type-qualifiers qualified-type-type

          ;; Declaration constructors
          make-typedef typedef? typedef-name typedef-type
          make-struct-decl struct-decl? struct-decl-name struct-decl-fields
          make-union-decl union-decl? union-decl-name union-decl-fields
          make-field field? field-name field-type
          make-function-decl function-decl? function-decl-name function-decl-return-type
                             function-decl-params function-decl-variadic?
          make-param param? param-name param-type
          make-enum-decl enum-decl? enum-decl-name enum-decl-enumerators
          make-enumerator enumerator? enumerator-name enumerator-value)

  (import (rnrs base)
          (rnrs records syntactic))

  ;;; Types

  ;; Basic type: int, char, float, double, void, etc.
  (define-record-type basic-type
    (fields name))  ;; symbol: 'int, 'char, 'float, 'double, 'void, etc.

  ;; Pointer type: T*
  (define-record-type pointer-type
    (fields pointee))  ;; type

  ;; Array type: T[n]
  (define-record-type array-type
    (fields element    ;; type
            size))     ;; #f for unsized, number for sized

  ;; Function type: T(params)
  (define-record-type function-type
    (fields return     ;; type
            params     ;; list of types
            variadic?)) ;; boolean

  ;; Named type: struct foo, union bar, enum baz, or typedef name
  (define-record-type named-type
    (fields kind       ;; symbol: 'struct, 'union, 'enum, 'typedef
            name))     ;; symbol

  ;; Qualified type: const T, volatile T
  (define-record-type qualified-type
    (fields qualifiers ;; list of symbols: '(const), '(volatile), '(const volatile)
            type))     ;; type

  ;;; Declarations

  ;; Typedef: typedef <type> <name>;
  (define-record-type typedef
    (fields name       ;; symbol
            type))     ;; type

  ;; Struct declaration: struct <name> { <fields> }
  (define-record-type struct-decl
    (fields name       ;; symbol or #f for anonymous
            fields))   ;; list of field

  ;; Union declaration: union <name> { <fields> }
  (define-record-type union-decl
    (fields name       ;; symbol or #f for anonymous
            fields))   ;; list of field

  ;; Field in struct/union
  (define-record-type field
    (fields name       ;; symbol
            type))     ;; type

  ;; Function declaration: <return-type> <name>(<params>)
  (define-record-type function-decl
    (fields name           ;; symbol
            return-type    ;; type
            params         ;; list of param
            variadic?))    ;; boolean

  ;; Function parameter
  (define-record-type param
    (fields name       ;; symbol or #f for unnamed
            type))     ;; type

  ;; Enum declaration: enum <name> { <enumerators> }
  (define-record-type enum-decl
    (fields name           ;; symbol or #f for anonymous
            enumerators))  ;; list of enumerator

  ;; Enum enumerator
  (define-record-type enumerator
    (fields name       ;; symbol
            value)))   ;; number or #f for auto
