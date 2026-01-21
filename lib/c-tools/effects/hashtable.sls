;; SPDX-License-Identifier: WTFPL
;; Thread-Safe Hashtable Wrapper
;; Provides mutex-protected access to hashtables for safe concurrent use
;;
;; Following Chez Scheme manual best practices:
;; - Use with-mutex for exception-safe locking
;; - Encapsulate synchronization to prevent forgetting to lock
;; - All hashtable access goes through protected interface

(library (c-tools effects hashtable)
  (export make-protected-hashtable
          protected-hashtable?
          protected-hashtable-ref
          protected-hashtable-set!
          protected-hashtable-delete!
          protected-hashtable-contains?
          protected-hashtable-update!
          protected-hashtable-size
          protected-hashtable-entries
          protected-hashtable-keys
          protected-hashtable-values
          protected-hashtable-clear!)
  (import (rnrs base)
          (rnrs hashtables)
          (rnrs records syntactic)
          (rnrs control)
          (only (c-tools utility) make-mutex with-mutex hashtable-values))

  (define-record-type (protected-hashtable %make-protected-hashtable protected-hashtable?)
    (fields
      (immutable table)  ;; Underlying hashtable
      (immutable mutex))) ;; Protects all access

  (define make-protected-hashtable
    (case-lambda
      [(hash-fn equiv-fn)
       (make-protected-hashtable hash-fn equiv-fn 32)]
      [(hash-fn equiv-fn initial-size)
       (let ([table (make-hashtable hash-fn equiv-fn initial-size)]
             [mutex (make-mutex)])
         (%make-protected-hashtable table mutex))]))

  (define (protected-hashtable-ref pht key default)
    ;;   Thread-safe hashtable lookup
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-ref (protected-hashtable-table pht) key default)))

  (define (protected-hashtable-set! pht key value)
    ;;   Thread-safe hashtable insert/update
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-set! (protected-hashtable-table pht) key value)))

  (define (protected-hashtable-delete! pht key)
    ;;   Thread-safe hashtable delete
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-delete! (protected-hashtable-table pht) key)))

  (define (protected-hashtable-contains? pht key)
    ;;   Thread-safe hashtable membership test
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-contains? (protected-hashtable-table pht) key)))

  (define (protected-hashtable-update! pht key proc default)
    ;; Thread-safe atomic update: (proc old-value) -> new-value
    ;;   If key not found, uses default as old-value
    (with-mutex (protected-hashtable-mutex pht)
      (let* ([table (protected-hashtable-table pht)]
             [old-value (hashtable-ref table key default)]
             [new-value (proc old-value)])
        (hashtable-set! table key new-value)
        new-value)))

  (define (protected-hashtable-size pht)
    ;;   Thread-safe size query
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-size (protected-hashtable-table pht))))

  (define (protected-hashtable-entries pht)
    ;; Thread-safe snapshot of all entries
    ;;   Returns two vectors: keys and values
    ;;   WARNING: Caller must not mutate hashtable while iterating
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-entries (protected-hashtable-table pht))))

  (define (protected-hashtable-keys pht)
    ;;   Thread-safe snapshot of all keys
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-keys (protected-hashtable-table pht))))

  (define (protected-hashtable-values pht)
    ;;   Thread-safe snapshot of all values
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-values (protected-hashtable-table pht))))

  (define (protected-hashtable-clear! pht)
    ;;   Thread-safe clear all entries
    (with-mutex (protected-hashtable-mutex pht)
      (hashtable-clear! (protected-hashtable-table pht)))))
