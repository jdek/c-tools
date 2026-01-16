;; C++ Template Context Effect
;; Tracks template parameter context during parsing
;; Critical for disambiguating < as less-than vs template argument list

(library (c-tools effects cpp-lang templates)
  (export push-template-params!
          pop-template-params!
          current-template-params!
          in-template?!
          register-template-name!
          is-template-name?!
          with-cpp-templates)
  (import (rnrs base)
          (rnrs lists)
          (rnrs control)
          (only (chezscheme) void)
          (c-tools effects core))

  ;; Push template parameters onto the stack
  ;; params: list of (kind . name) where kind is 'typename, 'class, or 'value
  (define (push-template-params! params)
    (perform (make-effect 'cpp-push-template-params params)))

  ;; Pop template parameters
  (define (pop-template-params!)
    (perform (make-effect 'cpp-pop-template-params #f)))

  ;; Get current template parameters or #f
  (define (current-template-params!)
    (perform (make-effect 'cpp-current-template-params #f)))

  ;; Check if currently inside a template declaration
  (define (in-template?!)
    (perform (make-effect 'cpp-in-template #f)))

  ;; Register a name as a template (for disambiguation)
  (define (register-template-name! name)
    (perform (make-effect 'cpp-register-template-name name)))

  ;; Check if name is a known template
  (define (is-template-name?! name)
    (perform (make-effect 'cpp-is-template-name name)))

  ;; Handler that provides template context
  (define (with-cpp-templates thunk)
    (let ([template-stack '()]
          [template-names '()])  ;; Known template names for disambiguation

      (with-handler 'cpp-push-template-params
        (lambda (params k loop)
          (set! template-stack (cons params template-stack))
          (k (void)))

        (with-handler 'cpp-pop-template-params
          (lambda (_ k loop)
            (if (null? template-stack)
                (error 'pop-template-params! "Template stack underflow")
                (begin
                  (set! template-stack (cdr template-stack))
                  (k (void)))))

          (with-handler 'cpp-current-template-params
            (lambda (_ k loop)
              (if (null? template-stack)
                  (k #f)
                  (k (car template-stack))))

            (with-handler 'cpp-in-template
              (lambda (_ k loop)
                (k (not (null? template-stack))))

              (with-handler 'cpp-register-template-name
                (lambda (name k loop)
                  (unless (memq name template-names)
                    (set! template-names (cons name template-names)))
                  (k (void)))

                (with-handler 'cpp-is-template-name
                  (lambda (name k loop)
                    ;; A name is a template if:
                    ;; 1. It's registered as a template name, OR
                    ;; 2. It's a template parameter of kind 'typename or 'class
                    (let ([registered? (memq name template-names)]
                          [param? (and (not (null? template-stack))
                                       (exists
                                         (lambda (param)
                                           (and (pair? param)
                                                (eq? (cdr param) name)
                                                (memq (car param) '(typename class))))
                                         (car template-stack)))])
                      (k (or registered? param?))))

                  (thunk))))))))))
