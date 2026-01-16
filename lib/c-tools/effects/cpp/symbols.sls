;; C Preprocessor Symbol Table Handler
;; Tracks defined symbols for #ifdef/#ifndef by delegating to macro handler

(library (c-tools effects cpp symbols)
  (export with-cpp-symbols)
  (import (rnrs base)
          (c-tools effects core)
          (c-tools effects registry))

  ;; Symbol table handler
  ;; Delegates cpp-symbol queries to cpp-expand with no args
  ;; If macro is defined (even as empty), cpp-expand won't return 'undefined
  (define (with-cpp-symbols thunk)
    ;; Simply handle cpp-symbol by trying to expand the macro
    ;; If it's defined, expansion will succeed (may return empty list)
    ;; If undefined, expansion returns 'undefined
    (with-handler 'cpp-symbol
      (lambda (data k loop)
        ;; Try to expand as object-like macro (args=#f)
        (loop (lambda ()
          (let ([result (perform (make-effect 'cpp-expand (cons data #f)))])
            (k (not (eq? result 'undefined)))))))
      (thunk)))

  ;;=========================================================================
  ;; Registration

  (register-effect! 'cpp-symbols
    (lambda (spec thunk)
      (with-cpp-symbols thunk)))
)
