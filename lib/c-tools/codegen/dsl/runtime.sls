;; SPDX-License-Identifier: WTFPL
;; DSL Runtime - FFI code generation using backend specs

(library (c-tools codegen dsl runtime)
  (export generate-ffi-code
          ast->ffi-type
          declaration->ffi-form)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs io ports)
          (c-tools ast c)
          (c-tools codegen dsl template)
          (c-tools codegen dsl patterns)
          (only (c-tools utility) symbol-append))

  ;;=======================================================================
  ;; Utilities

  ;; filter-map : (a => (or b #f)) list-of-a => list-of-b
  ;;   Maps function over list, filtering out #f results.
  (define (filter-map f lst)
    (let loop ([lst lst] [acc '()])
      (cond
        [(null? lst) (reverse acc)]
        [else
         (let ([result (f (car lst))])
           (if result
               (loop (cdr lst) (cons result acc))
               (loop (cdr lst) acc)))])))

  ;;=======================================================================
  ;; Main FFI Generation

  ;; generate-ffi-code : backend-spec list-of-decl string => s-expression
  ;;   Generates complete FFI module from declarations using backend spec.
  (define (generate-ffi-code backend-spec decls lib-name)
    (let* ([type-map-raw (get-field backend-spec 'type-map)]
           [type-map (compile-type-map type-map-raw)]
           [decl-specs (get-field backend-spec 'declarations)]
           [custom-handlers (get-field backend-spec 'custom-handlers)]
           [ctx (make-context backend-spec type-map decl-specs custom-handlers lib-name)]
           [forms (filter-map
                    (lambda (decl)
                      (declaration->ffi-form ctx decl))
                    decls)]
           [exports (extract-exports forms)])
      ;; Return library form
      `(library (ffi ,(string->symbol lib-name))
         (export ,@exports)
         (import (chezscheme))
         ,@forms)))

  ;; make-context : backend-spec type-map decl-specs custom-handlers string => context
  ;;   Creates generation context.
  (define (make-context backend-spec type-map decl-specs custom-handlers lib-name)
    (list (cons 'backend-spec backend-spec)
          (cons 'type-map type-map)
          (cons 'decl-specs decl-specs)
          (cons 'custom-handlers custom-handlers)
          (cons 'lib-name lib-name)))

  ;; get-field : alist symbol => value
  (define (get-field alist key)
    (let ([entry (assoc key alist)])
      (if entry
          (cdr entry)
          #f)))

  ;;=======================================================================
  ;; Type Mapping

  ;; ast->ffi-type : context type => ffi-type
  ;;   Converts C AST type to target FFI type using pattern matching.
  (define (ast->ffi-type ctx type)
    (let ([type-map (get-field ctx 'type-map)])
      (let ([result (match-type type-map type)])
        (if result
            result
            ;; Fallback: treat as opaque pointer
            'void*))))

  ;;=======================================================================
  ;; Declaration Conversion

  ;; declaration->ffi-form : context decl => (or form #f)
  ;;   Converts declaration to FFI form using declaration specs.
  (define (declaration->ffi-form ctx decl)
    (let ([custom-handlers (get-field ctx 'custom-handlers)]
          [decl-specs (get-field ctx 'decl-specs)])
      (cond
        ;; Check custom handlers first
        [(try-custom-handlers custom-handlers decl ctx)]

        ;; Function declaration
        [(function-decl? decl)
         (function->ffi-form ctx decl decl-specs)]

        ;; Struct declaration
        [(struct-decl? decl)
         (struct->ffi-form ctx decl decl-specs)]

        ;; Union declaration
        [(union-decl? decl)
         (union->ffi-form ctx decl decl-specs)]

        ;; Enum declaration
        [(enum-decl? decl)
         (enum->ffi-form ctx decl decl-specs)]

        ;; Typedef
        [(typedef? decl)
         (typedef->ffi-form ctx decl decl-specs)]

        ;; Unknown - skip
        [else #f])))

  ;; try-custom-handlers : list-of-handlers decl context => (or form #f)
  ;;   Tries custom handlers in order.
  (define (try-custom-handlers handlers decl ctx)
    (if (null? handlers)
        #f
        (let* ([handler (car handlers)]
               [pred? (car handler)]
               [handler-proc (cadr handler)])
          (if (pred? decl)
              (handler-proc decl ctx)
              (try-custom-handlers (cdr handlers) decl ctx)))))

  ;;-----------------------------------------------------------------------
  ;; Function FFI

  ;; function->ffi-form : context function-decl list-of-specs => form
  (define (function->ffi-form ctx decl decl-specs)
    (let ([spec (find-decl-spec 'function decl-specs decl)])
      (if (and spec (not (get-field spec 'skip)))
          (let* ([c-name (function-decl-name decl)]
                 [return-type (function-decl-return-type decl)]
                 [params (function-decl-params decl)]
                 [ffi-return (ast->ffi-type ctx return-type)]
                 [ffi-params (map (lambda (p)
                                   (ast->ffi-type ctx (param-type p)))
                                 params)]
                 [param-names (map (lambda (p)
                                    (or (param-name p) 'unnamed))
                                  params)]
                 [scheme-name (compute-scheme-name spec c-name)]
                 [env (make-function-env c-name scheme-name ffi-return
                                        ffi-params param-names)])
            (expand-template-spec spec env))
          #f)))

  ;; make-function-env : symbol symbol type list list => alist
  (define (make-function-env c-name scheme-name return params param-names)
    (list (cons 'lib-name "lib")
          (cons 'c-name (symbol->string c-name))
          (cons 'scheme-name scheme-name)
          (cons 'return-type return)
          (cons 'param-types params)
          (cons 'param-names param-names)))

  ;;-----------------------------------------------------------------------
  ;; Struct FFI

  ;; struct->ffi-form : context struct-decl list-of-specs => form
  (define (struct->ffi-form ctx decl decl-specs)
    (let ([spec (find-decl-spec 'struct decl-specs decl)])
      (if (and spec (not (get-field spec 'skip)))
          (let* ([name (struct-decl-name decl)]
                 [fields (struct-decl-fields decl)]
                 [struct-name (if name (symbol-append 'struct- name) #f)]
                 [struct-ptr-name (if name
                                     (string->symbol
                                       (string-append "_"
                                                     (symbol->string name)
                                                     "-pointer"))
                                     #f)]
                 [ffi-fields (map (lambda (f)
                                   (list (cons 'name (field-name f))
                                         (cons 'type (ast->ffi-type ctx (field-type f)))))
                                 fields)]
                 [env (make-struct-env name struct-name struct-ptr-name ffi-fields)])
            (expand-template-spec spec env))
          #f)))

  ;; make-struct-env : symbol symbol symbol list => alist
  (define (make-struct-env name struct-name struct-ptr-name fields)
    (list (cons 'name name)
          (cons 'struct-name struct-name)
          (cons 'struct-ptr-name struct-ptr-name)
          (cons 'union-name struct-name)  ;; For union template
          (cons 'fields fields)))

  ;;-----------------------------------------------------------------------
  ;; Union FFI

  ;; union->ffi-form : context union-decl list-of-specs => form
  (define (union->ffi-form ctx decl decl-specs)
    (let ([spec (find-decl-spec 'union decl-specs decl)])
      (if (and spec (not (get-field spec 'skip)))
          (let* ([name (union-decl-name decl)]
                 [union-ptr-name (if name
                                    (string->symbol
                                      (string-append "_"
                                                    (symbol->string name)
                                                    "-pointer"))
                                    #f)]
                 [env (list (cons 'name name)
                           (cons 'union-ptr-name union-ptr-name))])
            (expand-template-spec spec env))
          #f)))

  ;;-----------------------------------------------------------------------
  ;; Enum FFI

  ;; enum->ffi-form : context enum-decl list-of-specs => form
  (define (enum->ffi-form ctx decl decl-specs)
    (let ([spec (find-decl-spec 'enum decl-specs decl)])
      (if (and spec (not (get-field spec 'skip)))
          (let* ([name (enum-decl-name decl)]
                 [enumerators (enum-decl-enumerators decl)]
                 [enum-items (map (lambda (e)
                                   (list (cons 'name (enumerator-name e))
                                         (cons 'value (or (enumerator-value e) 0))))
                                 enumerators)]
                 [env (list (cons 'enum-name name)
                           (cons 'enumerators enum-items))])
            (expand-template-spec spec env))
          #f)))

  ;;-----------------------------------------------------------------------
  ;; Typedef FFI

  ;; typedef->ffi-form : context typedef list-of-specs => form
  (define (typedef->ffi-form ctx decl decl-specs)
    (let ([spec (find-decl-spec 'typedef decl-specs decl)])
      (if (and spec (not (get-field spec 'skip)))
          (let* ([name (typedef-name decl)]
                 [type (typedef-type decl)]
                 [ffi-type (ast->ffi-type ctx type)]
                 [env (list (cons 'name name)
                           (cons 'type ffi-type))])
            (expand-template-spec spec env))
          #f)))

  ;;=======================================================================
  ;; Declaration Spec Utilities

  ;; find-decl-spec : symbol list decl => (or spec #f)
  ;;   Finds matching declaration spec, checking conditions.
  (define (find-decl-spec kind specs decl)
    (let loop ([specs specs])
      (if (null? specs)
          #f
          (let ([spec (car specs)])
            (if (and (pair? spec) (eq? (car spec) kind))
                (let ([condition (get-field (cdr spec) 'condition)])
                  (if (or (not condition) (condition decl))
                      (cdr spec)
                      (loop (cdr specs))))
                (loop (cdr specs)))))))

  ;; expand-template-spec : spec env => form
  ;;   Expands template from spec with environment.
  (define (expand-template-spec spec env)
    (let ([template (get-field spec 'template)])
      (if template
          (if (procedure? template)
              ;; Lambda template - call with environment
              (template env)
              ;; S-expression template - expand
              (expand-template template env))
          #f)))

  ;; compute-scheme-name : spec symbol => symbol
  ;;   Computes Scheme name using scheme-name transformer.
  (define (compute-scheme-name spec c-name)
    (let ([transformer (get-field spec 'scheme-name)])
      (if transformer
          (transformer c-name)
          c-name)))

  ;;=======================================================================
  ;; Export Extraction

  ;; extract-exports : list-of-forms => list-of-symbols
  ;;   Extracts exported symbols from s-expression forms.
  (define (extract-exports forms)
    (let loop ([forms forms] [exports '()])
      (if (null? forms)
          (reverse exports)
          (let ([form (car forms)])
            (cond
              ;; define form - export the name
              [(and (pair? form) (eq? (car form) 'define))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; define-ftype form - export the type name
              [(and (pair? form) (eq? (car form) 'define-ftype))
               (loop (cdr forms) (cons (cadr form) exports))]
              ;; begin form - recurse
              [(and (pair? form) (eq? (car form) 'begin))
               (loop (append (cdr form) (cdr forms)) exports)]
              ;; Other forms - skip
              [else
               (loop (cdr forms) exports)])))))

) ;; end library
