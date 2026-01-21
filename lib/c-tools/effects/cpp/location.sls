;; SPDX-License-Identifier: WTFPL
;; C Preprocessor Source Location Tracking Handler
;; Tracks current source location for error reporting

(library (c-tools effects cpp location)
  (export with-cpp-location)
  (import (rnrs base)
          (c-tools effects core)
          (c-tools effects registry)
          (only (c-tools utility) box set-box! unbox))

  ;; Location tracking handler
  ;; Maintains current file and line for error reporting
  ;; Supports #line directive to update location
  (define (with-cpp-location thunk)
    (let ([current-file (box "<unknown>")]
          [current-line (box 1)])

      (with-handler 'cpp-location-set
        (lambda (data k loop)
          (set-box! current-line (car data))
          (set-box! current-file (cdr data))
          (k #f))

        (with-handler 'cpp-location-get
          (lambda (data k loop)
            (k (cons (unbox current-line) (unbox current-file))))

          (thunk)))))

  ;;=========================================================================
  ;; Registration

  (register-effect! 'cpp-location
    (lambda (spec thunk)
      (with-cpp-location thunk)))
)
