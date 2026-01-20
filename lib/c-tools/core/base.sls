;; SPDX-License-Identifier: WTFPL
;; C/C++ Shared Base
;; Character predicates and operator tables shared between C and C++ lexers

(library (c-tools core base)
  (export ;; Character predicates
          c-whitespace?
          c-identifier-start?
          c-identifier-char?
          c-digit?
          c-hex-digit?
          c-octal-digit?

          ;; Shared operators
          c-two-char-operators
          c-three-char-operators)

  (import (rnrs base)
          (rnrs unicode))

  ;; c-whitespace? : char => boolean
  ;;   Returns true if character is C whitespace (space, tab, newline, CR).
  (define (c-whitespace? c)
    (and (char? c)
         (or (char=? c #\space)
             (char=? c #\tab)
             (char=? c #\newline)
             (char=? c #\return))))

  ;; c-identifier-start? : char => boolean
  ;;   Returns true if character can start a C identifier (letter or underscore).
  (define (c-identifier-start? c)
    (and (char? c)
         (or (char-alphabetic? c)
             (char=? c #\_))))

  ;; c-identifier-char? : char => boolean
  ;;   Returns true if character can appear in a C identifier.
  (define (c-identifier-char? c)
    (and (char? c)
         (or (char-alphabetic? c)
             (char-numeric? c)
             (char=? c #\_))))

  ;; c-digit? : char => boolean
  ;;   Returns true if character is a decimal digit (0-9).
  (define (c-digit? c)
    (and (char? c)
         (char-numeric? c)))

  ;; c-hex-digit? : char => boolean
  ;;   Returns true if character is a hexadecimal digit (0-9, a-f, A-F).
  (define (c-hex-digit? c)
    (and (char? c)
         (or (char-numeric? c)
             (and (char>=? c #\a) (char<=? c #\f))
             (and (char>=? c #\A) (char<=? c #\F)))))

  ;; c-octal-digit? : char => boolean
  ;;   Returns true if character is an octal digit (0-7).
  (define (c-octal-digit? c)
    (and (char? c)
         (char>=? c #\0)
         (char<=? c #\7)))

  ;; Two-character operators shared by C and C++
  (define c-two-char-operators
    '("==" "!=" "<=" ">=" "&&" "||"
      "++" "--" "<<" ">>"
      "+=" "-=" "*=" "/=" "%="
      "&=" "|=" "^="
      "->" "##"))

  ;; Three-character operators
  (define c-three-char-operators
    '("..." "<<=" ">>=")))
