;; SPDX-License-Identifier: WTFPL
;; Chez Scheme FFI Backend - DSL Specification

(library (c-tools codegen backends chez)
  (export chez-backend)
  (import (rnrs base)
          (rnrs lists)
          (only (c-tools utility) symbol-append)
          (c-tools codegen dsl template))

  ;; Chez Scheme FFI Backend Specification
  (define chez-backend
    (list
      (cons 'name 'chez)

      ;; Type mapping rules (pattern => result)
      (cons 'type-map
        (list
          ;; Basic types
          (list 'void 'void)
          (list 'char 'char)
          (list 'signed-char 'char)
          (list 'unsigned-char 'unsigned-char)
          (list 'short 'short)
          (list 'unsigned-short 'unsigned-short)
          (list 'int 'int)
          (list 'unsigned 'unsigned)
          (list 'long 'long)
          (list 'unsigned-long 'unsigned-long)
          (list 'long-long 'long-long)
          (list 'unsigned-long-long 'unsigned-long-long)
          (list 'float 'float)
          (list 'double 'double)
          ;; stdint.h types
          (list 'int8_t 'integer-8)
          (list 'uint8_t 'unsigned-8)
          (list 'int16_t 'integer-16)
          (list 'uint16_t 'unsigned-16)
          (list 'int32_t 'integer-32)
          (list 'uint32_t 'unsigned-32)
          (list 'int64_t 'integer-64)
          (list 'uint64_t 'unsigned-64)
          ;; Platform-specific types
          (list 'size_t 'size_t)
          (list 'ssize_t 'ssize_t)
          (list 'ptrdiff_t 'ptrdiff_t)
          (list 'intptr_t 'iptr)
          (list 'uintptr_t 'uptr)

          ;; Pointer types (most specific first)
          (list (list 'pointer 'char) 'string)
          (list (list 'pointer 'void) 'void*)
          (list (list 'pointer (list 'struct '?name))
                (lambda (name) (list '* (symbol-append 'struct- name))))
          (list (list 'pointer (list 'union '?name))
                (lambda (name) (list '* (symbol-append 'union- name))))
          (list (list 'pointer '?T)
                (lambda (T) (list '* T)))

          ;; Named types
          (list (list 'struct '?name)
                (lambda (name) (symbol-append 'struct- name)))
          (list (list 'union '?name)
                (lambda (name) (symbol-append 'union- name)))
          (list 'enum 'int)

          ;; Arrays decay to pointers
          (list (list 'array '?T '?size)
                (lambda (T size) (list '* T)))

          ;; Function pointers
          (list 'function 'void*)))

      ;; Declaration templates (s-expressions with (@ var) markers)
      (cons 'declarations
        (list
          ;; Function declaration
          (list 'function
                (cons 'scheme-name (lambda (c-name) (symbol-append 'c- c-name)))
                (cons 'template
                  (template-lambda (scheme-name lib-name c-name param-types return-type)
                    `(define ,scheme-name
                       (foreign-procedure ,lib-name ,c-name ,param-types ,return-type)))))

          ;; Struct declaration
          (list 'struct
                (cons 'template
                  (template-lambda (struct-name fields)
                    `(define-ftype ,struct-name
                       (struct ,@(map (lambda (f)
                                       (list (cdr (assoc 'name f))
                                             (cdr (assoc 'type f))))
                                     fields))))))

          ;; Union declaration
          (list 'union
                (cons 'template
                  (template-lambda (union-name fields)
                    `(define-ftype ,union-name
                       (union ,@(map (lambda (f)
                                      (list (cdr (assoc 'name f))
                                            (cdr (assoc 'type f))))
                                    fields))))))

          ;; Enum declaration - generate defines
          (list 'enum
                (cons 'template
                  (template-lambda (enumerators)
                    `(begin
                       ,@(map (lambda (e)
                               `(define ,(cdr (assoc 'name e))
                                  ,(cdr (assoc 'value e))))
                             enumerators)))))

          ;; Typedef - skip for now
          (list 'typedef
                (cons 'skip #t))))

      ;; Custom handlers
      (cons 'custom-handlers '())))

) ;; end library
