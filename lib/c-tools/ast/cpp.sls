;; SPDX-License-Identifier: WTFPL
(library (c-tools ast cpp)
  (export namespace-decl namespace-decl? make-namespace-decl
          namespace-decl-name namespace-decl-decls

          qualified-name qualified-name? make-qualified-name
          qualified-name-scopes qualified-name-name

          class-decl class-decl? make-class-decl
          class-decl-kind class-decl-name class-decl-bases class-decl-members

          member-decl member-decl? make-member-decl
          member-decl-access member-decl-kind member-decl-decl member-decl-specifiers

          template-decl template-decl? make-template-decl
          template-decl-params template-decl-decl

          template-type template-type? make-template-type
          template-type-name template-type-args

          reference-type reference-type? make-reference-type
          reference-type-referent)
  (import (rnrs base)
          (rnrs records syntactic))

  ;; Namespace declaration
  ;; namespace foo { int x; void bar(); }
  (define-record-type namespace-decl
    (fields name      ;; Symbol: namespace name
            decls))   ;; List of declarations inside namespace

  ;; Qualified name
  ;; std::vector, foo::bar::baz
  (define-record-type qualified-name
    (fields scopes    ;; List of scope names (symbols or template-types)
            name))    ;; Final name (symbol or template-type)

  ;; Class/struct declaration
  ;; class Foo : public Bar { ... };
  (define-record-type class-decl
    (fields kind      ;; Symbol: 'class or 'struct
            name      ;; Symbol: class name
            bases     ;; List of (access . type) for base classes
            members)) ;; List of member-decl

  ;; Member declaration (field or method)
  (define-record-type member-decl
    (fields access      ;; Symbol: 'public, 'private, or 'protected
            kind        ;; Symbol: 'method, 'field, 'constructor, 'destructor
            decl        ;; Actual declaration (function-decl or variable-decl)
            specifiers  ;; List of symbols: 'static, 'virtual, 'const, 'override, etc.
            ))

  ;; Template declaration
  ;; template<typename T> class Foo { ... };
  (define-record-type template-decl
    (fields params    ;; List of template parameters
            decl))    ;; Templated declaration

  ;; Template instantiation type
  ;; vector<int>, map<string, double>
  (define-record-type template-type
    (fields name      ;; Symbol: template name
            args))    ;; List of type arguments

  ;; Reference type (C++ references)
  ;; int&, const Foo&
  (define-record-type reference-type
    (fields referent)) ;; Type being referenced
)
