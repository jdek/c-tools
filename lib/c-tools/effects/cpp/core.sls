;; SPDX-License-Identifier: WTFPL
;; C Preprocessor Effect Operations
;; Core effect operations for C preprocessing
;; These are the "perform" side - handlers are in separate files

(library (c-tools effects cpp core)
  (export ;; Macro expansion effects
          expand-macro!
          define-macro!
          undefine-macro!

          ;; Include directive effects
          resolve-include!

          ;; Symbol table effects
          symbol-defined?

          ;; Conditional compilation effects
          eval-conditional!

          ;; Diagnostic collection effects
          cpp-warn!
          cpp-error!

          ;; Source location tracking effects
          set-source-location!
          get-source-location!)
  (import (rnrs base)
          (c-tools effects core))

  ;;; Macro Expansion Effects

  (define (expand-macro! name args)
    ;;   Request macro expansion for NAME with ARGS (or #f for object-like macro)
    (perform (make-effect 'cpp-expand (cons name args))))

  (define (define-macro! name params body)
    ;;   Define a macro NAME with PARAMS (or #f for object-like) and BODY tokens
    (perform (make-effect 'cpp-define (list name params body))))

  (define (undefine-macro! name)
    ;;   Undefine a macro NAME (#undef)
    (perform (make-effect 'cpp-undef name)))

  ;;; Include Directive Effects

  ;; resolve-include! : string boolean => (cons string bytevector) | #f
  ;;   effects: io/read
  ;;   Resolve an include directive for PATH
  ;;   ANGLE-BRACKETS? is #t for <...>, #f for "..."
  ;;   Returns (resolved-path . content) or #f if not found
  (define (resolve-include! path angle-brackets?)
    (perform (make-effect 'cpp-include (cons path angle-brackets?))))

  ;;; Symbol Table Effects

  ;; symbol-defined? : symbol => boolean
  ;;   Check if symbol NAME is defined as a macro
  ;;   Used for #ifdef/#ifndef evaluation
  (define (symbol-defined? name)
    (perform (make-effect 'cpp-symbol name)))

  ;;; Conditional Compilation Effects

  ;; eval-conditional! : symbol value => boolean
  ;;   Evaluate a conditional directive
  ;;   KIND is one of: 'ifdef, 'ifndef, 'if, 'elif
  ;;   CONDITION is macro name (for ifdef/ifndef) or expression tokens (for if/elif)
  ;;   Returns #t if condition is true, #f otherwise
  (define (eval-conditional! kind condition)
    (perform (make-effect 'cpp-conditional (cons kind condition))))

  ;;; Diagnostic Collection Effects

  ;; cpp-warn! : location string => void
  ;;   effects: diagnostic
  ;;   Emit a preprocessor warning at LOCATION with MESSAGE
  ;;   Warnings are collected but do not stop preprocessing
  (define (cpp-warn! location message)
    (perform (make-effect 'cpp-warning (cons location message))))

  ;; cpp-error! : location string => void
  ;;   effects: diagnostic
  ;;   Emit a preprocessor error at LOCATION with MESSAGE
  ;;   Errors are collected but may not stop preprocessing (#error directive)
  (define (cpp-error! location message)
    (perform (make-effect 'cpp-error (cons location message))))

  ;;; Source Location Tracking Effects

  (define (set-source-location! line filename)
    ;;   Set current source location for error reporting (#line directive)
    (perform (make-effect 'cpp-location-set (cons line filename))))

  ;; get-source-location! : => (cons fixnum string)
  ;;   Get current source location for error reporting
  ;;   Returns (line . filename)
  (define (get-source-location!)
    (perform (make-effect 'cpp-location-get #f))))
