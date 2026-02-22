;; QuickCheck-style property testing for Scheme

(library (c-tools testing quickcheck)
  (export
    ;; Core testing
    check
    check-property

    ;; Generators
    gen-integer
    gen-natural
    gen-byte
    gen-boolean
    gen-char
    gen-string
    gen-list
    gen-vector
    gen-choose
    gen-one-of
    gen-frequency
    gen-map
    gen-bind
    gen-return
    make-generator  ;; For custom generators

    ;; Combinators
    gen-tuple
    gen-pair
    gen-maybe

    ;; Shrinkers
    shrink-integer
    shrink-list
    shrink-string

    ;; Configuration
    make-config
    config-num-tests
    config-max-size
    config-seed

    ;; Random state
    seed-random!
    generate
    random-range  ;; For custom random operations

    ;; Results
    test-result-pass?
    test-result-reason
    test-result-counter-example)

  (import (rnrs base)
          (rnrs arithmetic bitwise)
          (rnrs arithmetic fixnums)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (rnrs io simple)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs unicode)
          (only (chezscheme) current-time time-second make-parameter))

  ;;===========================================================================
  ;; Configuration

  (define-record-type config
    (fields num-tests max-size seed)
    (protocol
      (lambda (new)
        (case-lambda
          [() (new 100 100 (time-second (current-time)))]
          [(num-tests) (new num-tests 100 (time-second (current-time)))]
          [(num-tests max-size) (new num-tests max-size (time-second (current-time)))]
          [(num-tests max-size seed) (new num-tests max-size seed)]))))

  ;;===========================================================================
  ;; Random State

  (define two^32 4294967296) ;; 2^32
  (define mask32 #xffffffff)
  (define mask64 #xffffffffffffffff)

  (define (u64 x) (bitwise-and x mask64))
  (define (u32 x) (bitwise-and x mask32))

  ;; Global RNG state: (cons state inc) where both are uint64, inc is odd.
  (define *random-state* (make-parameter (cons 0 1)))

  (define (splitmix64-step x)
    (let* ([x (u64 (+ x #x9e3779b97f4a7c15))]
           [z (u64 x)]
           [z (u64 (* (bitwise-xor z (bitwise-arithmetic-shift z -30)) #xbf58476d1ce4e5b9))]
           [z (u64 (* (bitwise-xor z (bitwise-arithmetic-shift z -27)) #x94d049bb133111eb))]
           [z (u64 (bitwise-xor z (bitwise-arithmetic-shift z -31)))])
      (values x z)))

  (define (seed-random! seed)
    ;; Derive (state, inc) from a single seed deterministically.
    ;; Ensures inc is odd as required by PCG.
    (let* ([s (u64 seed)])
      (let*-values ([(s1 v1) (splitmix64-step s)]
                    [(s2 v2) (splitmix64-step s1)])
        (let ([inc (u64 (bitwise-ior v1 1))] ;; must be odd
              [st  (u64 v2)])
          (*random-state* (cons st inc))))))

  (define (rotr32 x r)
    ;; rotate-right 32-bit
    (let* ([r (bitwise-and r 31)]
           [x (u32 x)])
      (u32 (bitwise-ior (bitwise-arithmetic-shift x (- r))
                        (bitwise-arithmetic-shift x (- 32 r))))))

  (define (next-u32!)
    ;; PCG32 output function, advances state.
    (let* ([st+inc (*random-state*)]
           [st (car st+inc)]
           [inc (cdr st+inc)]
           ;; advance LCG in uint64
           [newst (u64 (+ (* st 6364136223846793005) inc))])
      (*random-state* (cons newst inc))
      ;; output transform
      (let* ([xorshifted
              (u32 (bitwise-arithmetic-shift
                    (bitwise-xor (bitwise-arithmetic-shift st -18) st)
                    -27))]
             [rot (u32 (bitwise-arithmetic-shift st -59))])
        (rotr32 xorshifted rot))))

  (define (random-range n)
    ;; Unbiased integer in [0, n)
    (when (<= n 0)
      (raise
        (condition
          (make-error)
          (make-message-condition "random-range: n must be positive"))))
    (if (= n 1)
        0
        (let* ([n (exact n)] ;; in case callers pass inexact; safe for exact too
               [limit (- two^32 (mod two^32 n))])
          (let loop ()
            (let ([x (next-u32!)])
              (if (>= x limit)
                  (loop)
                  (mod x n)))))))

  (define (random-integer min max)
    ;; Inclusive range [min, max]
    (when (> min max)
      (raise
        (condition
          (make-error)
          (make-message-condition "random-integer: min > max"))))
    (let* ([range (+ 1 (- max min))])
      (+ min (random-range range))))

  ;;===========================================================================
  ;; Generator Type

  ;; Generator: size -> random-value
  (define-record-type generator
    (fields proc)
    (protocol
      (lambda (new)
        (lambda (proc)
          (new proc)))))

  (define (generate gen size)
    ((generator-proc gen) size))

  ;;===========================================================================
  ;; Basic Generators

  ;; Constant generator
  (define (gen-return value)
    (make-generator (lambda (size) value)))

  ;; Map function over generated values
  (define (gen-map f gen)
    (make-generator
      (lambda (size)
        (f (generate gen size)))))

  ;; Monadic bind for generators
  (define (gen-bind gen f)
    (make-generator
      (lambda (size)
        (let ([value (generate gen size)])
          (generate (f value) size)))))

  ;; Generate arbitrary integers
  (define gen-integer
    (make-generator
      (lambda (size)
        (random-integer (- size) size))))

  ;; Generate non-negative integers
  (define gen-natural
    (make-generator
      (lambda (size)
        (random-integer 0 size))))

  ;; Generate bytes (0-255)
  (define gen-byte
    (make-generator
      (lambda (size)
        (random-integer 0 255))))

  ;; Generate booleans
  (define gen-boolean
    (make-generator
      (lambda (size)
        ;; use a high bit, not modulo 2
        (not (zero? (bitwise-and (next-u32!) #x80000000))))))

  ;; Generate ASCII printable characters
  (define gen-char
    (make-generator
      (lambda (size)
        (integer->char (random-integer 32 126)))))

  ;; Generate integer in range [min, max]
  (define (gen-choose min max)
    (make-generator
      (lambda (size)
        (random-integer min max))))

  ;; Choose one generator randomly
  (define (gen-one-of . generators)
    (make-generator
      (lambda (size)
        (let ([k (length generators)])
          (when (= k 0)
            (raise
              (condition
                (make-error)
                (make-message-condition "gen-one-of: no generators"))))
          (let ([idx (random-range k)])
            (generate (list-ref generators idx) size))))))

  ;; Choose generator with weighted probability
  (define (gen-frequency . weighted-generators)
    (make-generator
      (lambda (size)
        (when (null? weighted-generators)
          (raise
            (condition
              (make-error)
              (make-message-condition "gen-frequency: empty"))))
        (let* ([total (apply + (map car weighted-generators))])
          (when (<= total 0)
            (raise
              (condition
                (make-error)
                (make-message-condition "gen-frequency: non-positive total weight"))))
          (let ([choice (random-range total)])
            (let loop ([wgens weighted-generators] [acc 0])
              (let* ([w (caar wgens)]
                     [g (cadar wgens)]
                     [acc2 (+ acc w)])
                (if (< choice acc2)
                    (generate g size)
                    (loop (cdr wgens) acc2)))))))))

  ;;===========================================================================
  ;; Collection Generators

  ;; Generate list of elements
  (define (gen-list elem-gen)
    (make-generator
      (lambda (size)
        (let ([len (random-integer 0 size)])
          (let loop ([i 0] [acc '()])
            (if (>= i len)
                (reverse acc)
                (loop (+ i 1)
                      (cons (generate elem-gen size) acc))))))))

  ;; Generate vector of elements
  (define (gen-vector elem-gen)
    (gen-map list->vector (gen-list elem-gen)))

  ;; Generate ASCII string
  (define gen-string
    (gen-map list->string (gen-list gen-char)))

  ;; Generate tuple (list) with specific generators for each position
  (define (gen-tuple . generators)
    (make-generator
      (lambda (size)
        (map (lambda (gen) (generate gen size)) generators))))

  ;; Generate pair
  (define (gen-pair gen-a gen-b)
    (make-generator
      (lambda (size)
        (cons (generate gen-a size)
              (generate gen-b size)))))

  ;; Generate optional value (#f or value)
  (define (gen-maybe gen)
    (make-generator
      (lambda (size)
        (if (generate gen-boolean size)
            (generate gen size)
            #f))))

  ;;===========================================================================
  ;; Shrinkers

  ;; Shrink integer towards zero by halving
  (define (shrink-integer n)
    (if (= n 0)
        '(0)
        (let loop ([candidate (div n 2)]
                   [results (list n)])
          (if (= candidate 0)
              (reverse (cons 0 results))
              (loop (div candidate 2)
                    (cons candidate results))))))

  ;; Shrink list by removing elements and shrinking size
  (define (shrink-list lst)
    (if (null? lst)
        '()
        (let ([len (length lst)])
          (append
            ;; Remove chunks
            (let loop ([n (div len 2)] [acc '()])
              (if (<= n 0)
                  acc
                  (let ([chunks (list-chunks lst n)])
                    (loop (div n 2)
                          (append acc chunks)))))
            ;; Remove single elements
            (let loop ([i 0] [acc '()])
              (if (>= i len)
                  acc
                  (loop (+ i 1)
                        (cons (remove-at lst i) acc))))))))

  ;; Split list into chunks of size n, return list with each chunk removed
  (define (list-chunks lst n)
    (if (< (length lst) n)
        '()
        (cons (drop lst n)
              (let ([rest (list-chunks (drop lst n) n)])
                (map (lambda (chunk) (append (take lst n) chunk))
                     rest)))))

  (define (take lst n)
    (if (or (null? lst) (<= n 0))
        '()
        (cons (car lst) (take (cdr lst) (- n 1)))))

  (define (drop lst n)
    (if (or (null? lst) (<= n 0))
        lst
        (drop (cdr lst) (- n 1))))

  (define (remove-at lst idx)
    (if (null? lst)
        '()
        (if (= idx 0)
            (cdr lst)
            (cons (car lst) (remove-at (cdr lst) (- idx 1))))))

  ;; Shrink string
  (define (shrink-string str)
    (map list->string (shrink-list (string->list str))))

  ;;===========================================================================
  ;; Property Testing

  (define-record-type test-result
    (fields pass? reason counter-example))

  (define (make-pass)
    (make-test-result #t #f #f))

  (define (make-fail reason counter-example)
    (make-test-result #f reason counter-example))

  ;; Check property with generator
  (define (check-property prop gen config)
    (seed-random! (config-seed config))
    (let ([num-tests (config-num-tests config)]
          [max-size (config-max-size config)])
      (let test-loop ([n 0])
        (if (>= n num-tests)
            (make-pass)
            (let* ([size (min (+ n 1) max-size)]
                   [value (generate gen size)])
              (if (prop value)
                  (test-loop (+ n 1))
                  (make-fail "Property falsified" value)))))))

  ;; Run property check and print results
  (define (check desc prop)
    (display "Checking: ")
    (display desc)
    (newline)
    (let ([result (prop)])
      (if (test-result-pass? result)
          (begin
            (display "  Passed")
            (newline)
            #t)
          (begin
            (display "Failed:")
            (display (test-result-reason result))
            (newline)
            (display "  Counter-example:")
            (display (test-result-counter-example result))
            (newline)
            #f))))
)
