;; SPDX-License-Identifier: WTFPL
(library (c-tools effects cpp-lang namespace)
  (export push-namespace!
          pop-namespace!
          resolve-qualified-name!
          current-namespace!
          with-cpp-namespace)
  (import (rnrs base)
          (rnrs lists)
          (only (c-tools utility) void)
          (c-tools effects core))

  (define (push-namespace! name)
    (perform (make-effect 'cpp-push-namespace name)))

  (define (pop-namespace!)
    (perform (make-effect 'cpp-pop-namespace #f)))

  (define (resolve-qualified-name! parts)
    (perform (make-effect 'cpp-resolve-name parts)))

  (define (current-namespace!)
    (perform (make-effect 'cpp-current-namespace #f)))

  (define (with-cpp-namespace thunk)
    (let ([namespace-stack '()])

      (with-handler 'cpp-push-namespace
        (lambda (name k loop)
          (set! namespace-stack (cons name namespace-stack))
          (k (void)))

        (with-handler 'cpp-pop-namespace
          (lambda (_ k loop)
            (if (null? namespace-stack)
                (error 'pop-namespace! "Namespace stack underflow")
                (begin
                  (set! namespace-stack (cdr namespace-stack))
                  (k (void)))))

          (with-handler 'cpp-current-namespace
            (lambda (_ k loop)
              (k (reverse namespace-stack)))

            (with-handler 'cpp-resolve-name
              (lambda (parts k loop)
                (k (append (reverse namespace-stack) parts)))

              (thunk))))))))
