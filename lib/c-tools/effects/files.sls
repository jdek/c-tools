;; SPDX-License-Identifier: WTFPL
;; File Effects
;; Effect-based filesystem operations for transparent memory-fs/disk handling

(library (c-tools effects files)
  (export read-file!
          write-file!
          file-exists!
          with-file-system)
  (import (rnrs base)
          (rnrs bytevectors)
          (rnrs io ports)
          (only (chezscheme)
                file-exists?
                open-file-input-port
                port-length)
          (only (c-tools utility) void)
          (c-tools effects hashtable)
          (c-tools effects core))

  ;;===========================================================================
  ;; File Effects

  ;; Read file content as bytevector
  (define (read-file! path)
    (perform (make-effect 'read-file path)))

  ;; Write content to file
  (define (write-file! path content)
    (perform (make-effect 'write-file (cons path content))))

  ;; Check if file exists
  (define (file-exists! path)
    (perform (make-effect 'file-exists path)))

  ;;===========================================================================
  ;; File Effect Handler

  ;; Handles file operations by checking memory-fs first, then filesystem
  (define (with-file-system memory-fs root-dir thunk)
    (with-handler 'file-exists
      (lambda (path k loop)
        (let ([exists? (or (and memory-fs (protected-hashtable-contains? memory-fs path))
                          (file-exists? (string-append root-dir path)))])
          (k exists?)))
      (with-handler 'read-file
        (lambda (path k loop)
          ;; Check memory-fs first
          (if (and memory-fs (protected-hashtable-contains? memory-fs path))
              ;; Return from memory-fs
              (k (protected-hashtable-ref memory-fs path #f))
              ;; Fall back to filesystem
              (let ([full-path (string-append root-dir path)])
                (if (file-exists? full-path)
                    (let ([buf (call-with-port (open-file-input-port full-path)
                                 (lambda (in)
                                   (let* ([size (port-length in)]
                                          [buf (make-bytevector size)])
                                     (get-bytevector-n! in buf 0 size)
                                     buf)))])
                      (k buf))
                    ;; File doesn't exist
                    (k #f)))))
        (with-handler 'write-file
          (lambda (data k loop)
            (let ([path (car data)]
                  [content (cdr data)])
              ;; Write to memory-fs if available
              (if memory-fs
                  (begin
                    (protected-hashtable-set! memory-fs path content)
                    (k (void)))
                  ;; Otherwise write to disk
                  (error 'write-file! "Disk writing not implemented" path))))
          (thunk)))))
)
