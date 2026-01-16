;; C++ Class Context Effect
;; Tracks current class/struct scope during parsing

(library (c-tools effects cpp-lang class)
  (export push-class!
          pop-class!
          current-class!
          current-access!
          set-access!
          in-class?!
          with-cpp-class)
  (import (rnrs base)
          (rnrs lists)
          (only (chezscheme) void)
          (c-tools effects core))

  ;; Push a new class context onto the stack
  ;; kind: 'class or 'struct
  ;; name: symbol
  (define (push-class! kind name)
    (perform (make-effect 'cpp-push-class (cons kind name))))

  ;; Pop the current class context
  (define (pop-class!)
    (perform (make-effect 'cpp-pop-class #f)))

  ;; Get current class info: (kind . name) or #f if not in class
  (define (current-class!)
    (perform (make-effect 'cpp-current-class #f)))

  ;; Get current access specifier: 'public, 'private, 'protected
  (define (current-access!)
    (perform (make-effect 'cpp-current-access #f)))

  ;; Set access specifier for current class
  (define (set-access! access)
    (perform (make-effect 'cpp-set-access access)))

  ;; Check if currently inside a class
  (define (in-class?!)
    (perform (make-effect 'cpp-in-class #f)))

  ;; Handler that provides class context
  ;; Stack entry: (kind name access) where access is current access level
  (define (with-cpp-class thunk)
    (let ([class-stack '()])

      ;; Get default access for class kind
      (define (default-access kind)
        (if (eq? kind 'struct) 'public 'private))

      (with-handler 'cpp-push-class
        (lambda (data k loop)
          (let ([kind (car data)]
                [name (cdr data)])
            (set! class-stack
                  (cons (list kind name (default-access kind))
                        class-stack))
            (k (void))))

        (with-handler 'cpp-pop-class
          (lambda (_ k loop)
            (if (null? class-stack)
                (error 'pop-class! "Class stack underflow")
                (begin
                  (set! class-stack (cdr class-stack))
                  (k (void)))))

          (with-handler 'cpp-current-class
            (lambda (_ k loop)
              (if (null? class-stack)
                  (k #f)
                  (let ([entry (car class-stack)])
                    (k (cons (car entry) (cadr entry))))))

            (with-handler 'cpp-current-access
              (lambda (_ k loop)
                (if (null? class-stack)
                    (k #f)
                    (let ([entry (car class-stack)])
                      (k (caddr entry)))))

              (with-handler 'cpp-set-access
                (lambda (access k loop)
                  (if (null? class-stack)
                      (error 'set-access! "Not in class context")
                      (let ([entry (car class-stack)])
                        (set! class-stack
                              (cons (list (car entry) (cadr entry) access)
                                    (cdr class-stack)))
                        (k (void)))))

                (with-handler 'cpp-in-class
                  (lambda (_ k loop)
                    (k (not (null? class-stack))))

                  (thunk))))))))))
