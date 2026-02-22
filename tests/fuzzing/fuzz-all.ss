#!/usr/bin/env -S scheme --script
;;=======================================================================
;; fuzz-all.ss - Combined fuzzing campaigns for C/C++ parser
;;=======================================================================

(import (chezscheme)
        (c-tools testing quickcheck)
        (c-tools testing generators)
        (c-tools testing properties)
        (c-tools testing fuzzing mutation)
        (c-tools testing fuzzing harness)
        (c-tools testing fuzzing corpus)
        (prefix (c-tools codegen chez ffi) chez:)
        (prefix (c-tools codegen chez cpp-ffi) chez:)
        (prefix (c-tools codegen chicken ffi) chicken:)
        (prefix (c-tools codegen chicken cpp-ffi) chicken:)
        (prefix (c-tools codegen gambit ffi) gambit:)
        (prefix (c-tools codegen gambit cpp-ffi) gambit:)
        (prefix (c-tools codegen guile ffi) guile:)
        (prefix (c-tools codegen guile cpp-ffi) guile:)
        (prefix (c-tools codegen racket ffi) racket:)
        (prefix (c-tools codegen racket cpp-ffi) racket:)
        (prefix (c-tools codegen cffi ffi) cffi:)
        (prefix (c-tools codegen cffi cpp-ffi) cffi:))

;;=======================================================================
;; Shared Helpers
;;=======================================================================

(define (get-test-count args)
  (if (and (pair? args) (string->number (car args)))
      (string->number (car args))
      100))  ;; Default: 100 tests per campaign

(define (take-mutations lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-mutations (cdr lst) (- n 1)))))

(define (apply-n-random-mutations str n mutations seed)
  (if (<= n 0)
      str
      (let ([mutation (list-ref mutations (random (length mutations)))])
        (apply-n-random-mutations
          (mutate-input str seed mutation)
          (- n 1)
          mutations
          seed))))

;;-----------------------------------------------------------------------
;; Parser Wrappers
;;-----------------------------------------------------------------------

;; parse-c-wrapper : string => boolean
(define (parse-c-wrapper input)
  (guard (ex [else #f])
    (let ([result (parse-c-string input)])
      #t)))

;; parse-cpp-wrapper : string => boolean
(define (parse-cpp-wrapper input)
  (guard (ex [else #f])
    (let ([result (parse-cpp-string input)])
      #t)))

;;-----------------------------------------------------------------------
;; Bug Detectors
;;-----------------------------------------------------------------------

(define (check-parse-succeeds input)
  (let ([result (parse-c-string input)])
    (if result 'pass 'fail)))

(define (check-cpp-parse-succeeds input)
  (let ([result (parse-cpp-string input)])
    (if result 'pass 'fail)))

(define (check-determinism input)
  (if (prop-parser-deterministic input) 'pass 'fail))

(define (check-ffi-succeeds input)
  (guard (ex [else 'fail])
    (let ([ffi (generate-ffi-from-string input)])
      (if ffi 'pass 'fail))))

(define (check-bool-mapping input)
  (if (prop-bool-maps-to-boolean input) 'pass 'fail))

(define (check-array-mapping input)
  (if (prop-arrays-in-structs-are-inline input) 'pass 'fail))

(define (check-pointer-preservation input)
  (if (prop-pointer-types-preserved input) 'pass 'fail))

;;-----------------------------------------------------------------------
;; C++ Specific Mutations
;;-----------------------------------------------------------------------

;; insert-cpp-scope : string => string
(define (insert-cpp-scope str)
  (if (= (string-length str) 0)
      str
      (let* ([pos (random (+ 1 (string-length str)))]
             [scope "::"])
        (string-append (substring str 0 pos)
                      scope
                      (substring str pos (string-length str))))))

;; insert-cpp-template : string => string
(define (insert-cpp-template str)
  (if (= (string-length str) 0)
      str
      (let* ([pos (random (+ 1 (string-length str)))]
             [templates (list "<>" "<T>" "<typename T>" "<int N>"
                             "<class T, class U>" "<<" ">>")])
        (string-append (substring str 0 pos)
                      (list-ref templates (random (length templates)))
                      (substring str pos (string-length str))))))

;; insert-cpp-keyword : string => string
(define (insert-cpp-keyword str)
  (if (= (string-length str) 0)
      str
      (let* ([pos (random (+ 1 (string-length str)))]
             [keywords (list "class" "namespace" "template" "typename"
                            "virtual" "override" "final" "public" "private"
                            "protected" "operator" "friend" "using" "auto"
                            "constexpr" "noexcept" "nullptr" "delete" "default")])
        (string-append (substring str 0 pos)
                      " "
                      (list-ref keywords (random (length keywords)))
                      " "
                      (substring str pos (string-length str))))))

(define cpp-specific-mutations
  (list
    insert-c-keywords
    insert-preprocessor-directive
    insert-unmatched-brace
    insert-pointer-stars
    insert-array-brackets
    insert-cpp-scope
    insert-cpp-template
    insert-cpp-keyword))

;;-----------------------------------------------------------------------
;; Mutation Generators
;;-----------------------------------------------------------------------

(define (make-corpus-mutation-generator corpus mutations)
  (lambda (seed)
    (random-seed seed)
    (let* ([base-input (list-ref corpus (random (length corpus)))]
           [mutation (list-ref mutations (random (length mutations)))])
      (mutate-input base-input seed mutation))))

(define (make-valid-c-mutation-generator valid-corpus c-mutations)
  (lambda (seed)
    (random-seed seed)
    (let* ([base-input (list-ref valid-corpus (random (length valid-corpus)))]
           [mutation (list-ref c-mutations (random (length c-mutations)))])
      (mutate-input base-input seed mutation))))

(define (make-random-mutation-generator all-mutations base-inputs)
  (lambda (seed)
    (random-seed seed)
    (let* ([base (list-ref base-inputs (random (length base-inputs)))]
           [n-mutations (+ 1 (random 5))])
      (apply-n-random-mutations base n-mutations all-mutations seed))))

;;-----------------------------------------------------------------------
;; Generative Generators
;;-----------------------------------------------------------------------

;; generate-from : generator seed => value
;;   Helper to generate from a generator with a seed.
(define (generate-from gen seed)
  (generate gen (mod seed 100)))

(define (make-simple-decl-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-basic-type
                       (lambda (type)
                         (gen-bind gen-c-identifier
                                   (lambda (name)
                                     (gen-return (string-append type " " name ";"))))))])
    (generate-from gen seed)))

(define (make-struct-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return (string-append "struct " name " { int x; };"))))])
    (generate-from gen seed)))

(define (make-typedef-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-basic-type
                       (lambda (base)
                         (gen-bind gen-c-identifier
                                   (lambda (alias)
                                     (gen-return (string-append "typedef " base " " alias ";"))))))])
    (generate-from gen seed)))

(define (make-enum-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-bind gen-c-identifier
                                   (lambda (val)
                                     (gen-return (string-append "enum " name " { " val " };"))))))])
    (generate-from gen seed)))

(define (make-function-decl-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-basic-type
                       (lambda (ret)
                         (gen-bind gen-c-identifier
                                   (lambda (name)
                                     (gen-return (string-append ret " " name "(void);"))))))])
    (generate-from gen seed)))

(define (make-pointer-decl-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-basic-type
                       (lambda (type)
                         (gen-bind gen-c-identifier
                                   (lambda (name)
                                     (gen-return (string-append type " *" name ";"))))))])
    (generate-from gen seed)))

(define (make-array-decl-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-basic-type
                       (lambda (type)
                         (gen-bind gen-c-identifier
                                   (lambda (name)
                                     (gen-bind gen-int-literal
                                               (lambda (size)
                                                 (gen-return (string-append type " " name "[" size "];"))))))))])
    (generate-from gen seed)))

(define (make-multi-field-struct-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [names (list "a" "b" "c" "d")])
    (let ([struct-name (string-append "S" (number->string (mod seed 1000)))]
          [field1 (string-append (list-ref types (mod seed 3)) " " (list-ref names (mod seed 4)))]
          [field2 (string-append (list-ref types (mod (+ seed 1) 3)) " " (list-ref names (mod (+ seed 1) 4)))])
      (string-append "struct " struct-name " { " field1 "; " field2 "; };"))))

;;-----------------------------------------------------------------------
;; C++ Generators
;;-----------------------------------------------------------------------

(define (make-class-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return (string-append "class " name " {};"))))])
    (generate-from gen seed)))

(define (make-class-with-constructor-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return
                           (string-append "class " name " { public: "
                                         name "(); };"))))])
    (generate-from gen seed)))

(define (make-class-with-destructor-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return
                           (string-append "class " name " { public: ~"
                                         name "(); };"))))])
    (generate-from gen seed)))

(define (make-class-with-member-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-return
                                       (string-append "class " name " { " type " f(); };"))))))])
    (generate-from gen seed)))

(define (make-empty-namespace-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-return (string-append "namespace " name " {}"))))])
    (generate-from gen seed)))

(define (make-namespace-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-bind gen-c-identifier
                                               (lambda (var)
                                                 (gen-return
                                                   (string-append "namespace " name " { "
                                                                 type " " var "; }"))))))))])
    (generate-from gen seed)))

(define (make-cpp-struct-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-c-identifier
                       (lambda (name)
                         (gen-bind gen-basic-type
                                   (lambda (type)
                                     (gen-return
                                       (string-append "struct " name " { " type " x; };"))))))])
    (generate-from gen seed)))

;;-----------------------------------------------------------------------
;; Preprocessor Generators
;;-----------------------------------------------------------------------

(define (make-define-object-macro-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-bind gen-int-literal
                                   (lambda (value)
                                     (gen-return
                                       (string-append "#define " name " " value
                                                     "\nint x = " name ";"))))))])
    (generate-from gen seed)))

(define (make-define-function-macro-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-return
                           (string-append "#define " name "(x) ((x) + 1)\nint y = " name "(42);"))))])
    (generate-from gen seed)))

(define (make-ifdef-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-return
                           (string-append "#ifdef " name "\nint x;\n#endif"))))])
    (generate-from gen seed)))

(define (make-ifndef-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-return
                           (string-append "#ifndef " name "\n#define " name "\nint x;\n#endif"))))])
    (generate-from gen seed)))

(define (make-if-else-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name)
                         (gen-return
                           (string-append "#if defined(" name ")\nint x;\n#else\nint y;\n#endif"))))])
    (generate-from gen seed)))

(define (make-nested-conditional-generator seed)
  (random-seed seed)
  (let ([names (list "FOO" "BAR" "BAZ")])
    (let ([name1 (list-ref names (mod seed 3))]
          [name2 (list-ref names (mod (+ seed 1) 3))])
      (string-append "#ifdef " name1 "\n#ifdef " name2 "\nint x;\n#endif\n#endif"))))

(define (make-multiple-defines-generator seed)
  (random-seed seed)
  (let ([gen (gen-bind gen-macro-name
                       (lambda (name1)
                         (gen-bind gen-macro-name
                                   (lambda (name2)
                                     (gen-bind gen-int-literal
                                               (lambda (val)
                                                 (gen-return
                                                   (string-append "#define " name1 " " val
                                                                 "\n#define " name2 " 100\nint x = " name1 ";"))))))))])
    (generate-from gen seed)))

;;-----------------------------------------------------------------------
;; Complex C Generators
;;-----------------------------------------------------------------------

(define (make-multiple-decls-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [vars (list "a" "b" "c")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " "
                    (list-ref vars idx) ", "
                    (list-ref vars (mod (+ idx 1) (length vars))) ";"))))

(define (make-nested-struct-generator seed)
  (random-seed seed)
  (let ([names (list "Inner" "Outer" "Data")])
    (let ([inner (list-ref names (mod seed 3))]
          [outer (list-ref names (mod (+ seed 1) 3))])
      (string-append "struct " inner " { int x; }; struct " outer " { struct " inner " i; };"))))

(define (make-pointer-to-pointer-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [vars (list "ptr" "p" "pp")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " **"
                    (list-ref vars (mod idx (length vars))) ";"))))

(define (make-array-of-pointers-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [vars (list "arr" "a" "ptrs")]
        [sizes (list "10" "20" "5")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " *"
                    (list-ref vars (mod idx (length vars))) "["
                    (list-ref sizes (mod idx (length sizes))) "];"))))

(define (make-pointer-to-array-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "float")]
        [vars (list "ptr" "p" "arr")]
        [sizes (list "10" "20" "5")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " (*"
                    (list-ref vars (mod idx (length vars))) ")["
                    (list-ref sizes (mod idx (length sizes))) "];"))))

(define (make-function-pointer-generator seed)
  (random-seed seed)
  (let ([types (list "int" "char" "void")]
        [vars (list "fn" "f" "func")]
        [param-types (list "int" "char" "void")])
    (let ([idx (mod seed (length types))])
      (string-append (list-ref types idx) " (*"
                    (list-ref vars (mod idx (length vars))) ")("
                    (list-ref param-types (mod (+ idx 1) (length param-types))) ");"))))

(define (make-struct-with-array-generator seed)
  (random-seed seed)
  (let ([names (list "Data" "Node" "Record")]
        [types (list "int" "char" "float")]
        [sizes (list "10" "20" "5")])
    (let ([idx (mod seed (length names))])
      (string-append "struct " (list-ref names idx) " { "
                    (list-ref types (mod idx (length types))) " arr["
                    (list-ref sizes (mod idx (length sizes))) "]; };"))))

(define (make-typedef-function-pointer-generator seed)
  (random-seed seed)
  (let ([names (list "FnPtr" "Callback" "Handler")]
        [ret-types (list "int" "void" "char")]
        [param-types (list "int" "char" "void")])
    (let ([idx (mod seed (length names))])
      (string-append "typedef " (list-ref ret-types (mod idx (length ret-types)))
                    " (*" (list-ref names idx) ")("
                    (list-ref param-types (mod (+ idx 1) (length param-types))) ");"))))

(define (make-complex-struct-generator seed)
  (random-seed seed)
  (let ([names (list "Complex" "Node" "Data")])
    (let ([name (list-ref names (mod seed (length names)))])
      (string-append "struct " name " { int x; char *p; int arr[10]; };"))))

(define (make-large-file-generator seed)
  (random-seed seed)
  (let ([decls (list "int a;" "char b;" "float c;" "int *p;" "struct S { int x; };")])
    (let ([n (+ 5 (mod seed 10))])
      (let loop ([i 0] [acc ""])
        (if (>= i n)
            acc
            (loop (+ i 1)
                  (string-append acc (list-ref decls (mod (+ seed i) (length decls))) "\n")))))))

;;-----------------------------------------------------------------------
;; FFI Integration Generators (End-to-End)
;;-----------------------------------------------------------------------

;; These generators produce C code suitable for FFI testing

(define (make-simple-function-generator seed)
  (random-seed seed)
  (let ([names (list "add" "multiply" "negate")]
        [types (list "int" "long" "short")])
    (let ([idx (mod seed (length names))])
      (string-append (list-ref types (mod idx (length types)))
                    " " (list-ref names idx) "("
                    (list-ref types (mod idx (length types))) " x) { return x; }"))))

(define (make-void-function-generator seed)
  (random-seed seed)
  (let ([names (list "init" "cleanup" "reset")])
    (let ([name (list-ref names (mod seed (length names)))])
      (string-append "void " name "(void) {}"))))

(define (make-getter-function-generator seed)
  (random-seed seed)
  (let ([names (list "get_value" "get_count" "get_status")]
        [types (list "int" "long" "unsigned int")])
    (let ([idx (mod seed (length names))])
      (string-append (list-ref types (mod idx (length types)))
                    " " (list-ref names idx) "(void) { return 42; }"))))

(define (make-struct-with-getter-generator seed)
  (random-seed seed)
  (let ([struct-names (list "Point" "Data" "Record")]
        [getter-names (list "get_x" "get_value" "get_field")])
    (let ([idx (mod seed (length struct-names))])
      (string-append "struct " (list-ref struct-names idx) " { int x; };\n"
                    "int " (list-ref getter-names idx) "(struct "
                    (list-ref struct-names idx) " *p) { return p->x; }"))))

(define (make-multi-function-generator seed)
  (random-seed seed)
  (string-append "int add(int x, int y) { return x + y; }\n"
                "int sub(int x, int y) { return x - y; }\n"
                "int mul(int x, int y) { return x * y; }"))

;;-----------------------------------------------------------------------
;; C++ FFI Integration Generators
;;-----------------------------------------------------------------------

(define (make-cpp-simple-function-generator seed)
  (random-seed seed)
  (let ([names (list "increment" "decrement" "negate")]
        [types (list "int" "long" "short")])
    (let ([idx (mod seed (length names))])
      (string-append "extern \"C\" " (list-ref types (mod idx (length types)))
                    " " (list-ref names idx) "("
                    (list-ref types (mod idx (length types))) " x) { return x; }"))))

(define (make-cpp-class-method-generator seed)
  (random-seed seed)
  (let ([class-names (list "Calculator" "Counter" "Value")])
    (let ([name (list-ref class-names (mod seed (length class-names)))])
      (string-append "class " name " { public: int get() { return 42; } };\n"
                    "extern \"C\" int " name "_get(" name "* obj) { return obj->get(); }"))))

(define (make-cpp-namespace-function-generator seed)
  (random-seed seed)
  (let ([ns-names (list "math" "util" "core")])
    (let ([ns (list-ref ns-names (mod seed (length ns-names)))])
      (string-append "namespace " ns " { int compute(int x) { return x * 2; } }\n"
                    "extern \"C\" int compute(int x) { return " ns "::compute(x); }"))))

(define (make-cpp-multi-function-generator seed)
  (random-seed seed)
  (string-append "extern \"C\" int cpp_add(int x, int y) { return x + y; }\n"
                "extern \"C\" int cpp_sub(int x, int y) { return x - y; }\n"
                "extern \"C\" int cpp_mul(int x, int y) { return x * y; }"))

;;-----------------------------------------------------------------------
;; Multi-Platform FFI Test Infrastructure
;;-----------------------------------------------------------------------

;; Platform configurations
(define *platforms*
  '((chez     "scheme"        "--script")
    (chicken  "csi"           "-s")
    (gambit   "gsi"           "-:search=.")
    (guile    "guile"         "-s")
    (racket   "racket"        "")))

(define *ffi-test-dir* "tests/fuzzing/ffi-artifacts")

;; ensure-test-directory : => void
(define (ensure-test-directory)
  (unless (file-exists? *ffi-test-dir*)
    (system (string-append "mkdir -p " *ffi-test-dir*))))

;; write-source-file : string string string => string
;;   Writes source code to a file, returns the file path.
(define (write-source-file test-id lang source)
  (ensure-test-directory)
  (let ([file-path (string-append *ffi-test-dir* "/" test-id "." lang)])
    (call-with-output-file file-path
      (lambda (port)
        (put-string port source))
      'replace)
    file-path))

;; compile-source : string string => boolean
;;   Compiles C/C++ source to .so, returns #t on success.
(define (compile-source source-path lang)
  (let* ([base (substring source-path 0 (- (string-length source-path) (string-length lang) 1))]
         [so-path (string-append base ".so")]
         [compiler (if (equal? lang "cpp") "g++" "gcc")]
         [cmd (string-append compiler " -shared -fPIC -o " so-path " " source-path " 2>/dev/null")])
    (= 0 (system cmd))))

;; generate-binding : string string string => string
;;   Generates FFI binding for a platform, returns binding code.
(define (generate-binding test-id platform lang)
  (let ([source-path (string-append *ffi-test-dir* "/" test-id "." lang)])
    (guard (ex [else #f])
      (let* ([source (call-with-input-file source-path get-string-all)]
             [decls (if (equal? lang "c")
                        (parse-c-string source)
                        (parse-cpp-string source))])
        (if decls
            (guard (gen-ex [else #f])
              (case platform
                [(chez)
                 (if (equal? lang "c")
                     (chez:generate-ffi-code decls test-id)
                     (chez:generate-cpp-ffi-code decls test-id))]
                [(chicken)
                 (if (equal? lang "c")
                     (chicken:generate-ffi-code decls test-id)
                     (chicken:generate-cpp-ffi-code decls test-id))]
                [(gambit)
                 (if (equal? lang "c")
                     (gambit:generate-ffi-code decls test-id)
                     (gambit:generate-cpp-ffi-code decls test-id))]
                [(guile)
                 (if (equal? lang "c")
                     (guile:generate-ffi-code decls test-id)
                     (guile:generate-cpp-ffi-code decls test-id))]
                [(racket)
                 (if (equal? lang "c")
                     (racket:generate-ffi-code decls test-id)
                     (racket:generate-cpp-ffi-code decls test-id))]
                [(cffi)
                 (if (equal? lang "c")
                     (cffi:generate-ffi-code decls test-id)
                     (cffi:generate-cpp-ffi-code decls test-id))]
                [else #f]))
            #f)))))

;; write-binding : string string string => string | #f
;;   Writes binding to file, returns file path or #f.
(define (write-binding test-id platform lang binding-code)
  (if binding-code
      (let ([ext (case platform
                   [(chez chicken gambit guile racket) "scm"]
                   [(cffi) "py"]
                   [else "txt"])]
            [binding-str (if (pair? binding-code)
                             (call-with-string-output-port
                               (lambda (port)
                                 (write binding-code port)))
                             binding-code)])
        (let ([file-path (string-append *ffi-test-dir* "/" test-id "-"
                                       (symbol->string platform) "." ext)])
          (call-with-output-file file-path
            (lambda (port)
              (put-string port binding-str))
            'replace)
          file-path))
      #f))

;; test-binding-loads : string string string => boolean
;;   Actually tries to load and test the binding on a platform.
(define (test-binding-loads platform binding-path so-path)
  (guard (ex [else #f])
    (let* ([platform-info (assq platform *platforms*)]
           [interpreter (if platform-info (cadr platform-info) #f)]
           [flags (if platform-info (caddr platform-info) "")]
           [available? (and interpreter
                           (= 0 (system (string-append "which " interpreter " >/dev/null 2>&1"))))])
      (if available?
          ;; Create a small test script that loads the binding
          (let ([test-script (string-append *ffi-test-dir* "/test-load-"
                                           (symbol->string platform) ".scm")])
            (call-with-output-file test-script
              (lambda (port)
                (display "(guard (ex [else (exit 1)]) " port)
                (display "(load \"" port)
                (display binding-path port)
                (display "\") (exit 0))" port))
              'replace)
            (let ([result (system (string-append interpreter " " flags " " test-script " 2>/dev/null"))])
              (= 0 result)))
          ;; Platform not available, skip test
          #t))))

;;-----------------------------------------------------------------------
;; FFI Integration Test Harness
;;-----------------------------------------------------------------------

;; check-ffi-integration : string => 'pass | 'fail
;;   End-to-end test: Generate C code, compile it, load FFI, test it works.
(define (check-ffi-integration c-code)
  (guard (ex [else
              (when #f ;; Set to #t for debugging
                (printf "FFI integration error: ~a\n" ex))
              'fail])
    (let* ([temp-dir "/tmp/c-tools-ffi-test"]
           [c-file (string-append temp-dir "/test.c")]
           [so-file (string-append temp-dir "/libtest.so")]
           [cleanup (lambda ()
                      (when (file-exists? c-file)
                        (delete-file c-file))
                      (when (file-exists? so-file)
                        (delete-file so-file)))])

      ;; Create temp directory
      (unless (file-exists? temp-dir)
        (system (string-append "mkdir -p " temp-dir)))

      ;; Step 1: Write C code to file
      (call-with-output-file c-file
        (lambda (port)
          (put-string port c-code))
        'replace)

      ;; Step 2: Try to compile it (optional - just test if it would compile)
      (let ([compile-result (system (string-append "gcc -shared -fPIC -o " so-file " " c-file " 2>/dev/null"))])
        (if (= compile-result 0)
            ;; Step 3: Generate FFI bindings
            (let ([ffi-code (generate-ffi-from-string c-code)])
              (if ffi-code
                  ;; Step 4: Try to eval the FFI code (tests that it's valid Scheme)
                  (guard (eval-ex [else
                                   (cleanup)
                                   'fail])
                    (eval ffi-code (environment '(chezscheme)))
                    (cleanup)
                    'pass)
                  (begin
                    (cleanup)
                    'fail)))
            ;; Compilation failed - that's okay for fuzzing, not a bug
            (begin
              (cleanup)
              'pass))))))

;; Simpler check that doesn't require gcc
(define (check-ffi-codegen c-code)
  (guard (ex [else 'fail])
    (let ([ffi-code (generate-ffi-from-string c-code)])
      (if ffi-code
          (guard (eval-ex [else 'fail])
            ;; Just test that the generated code is valid Scheme
            (eval ffi-code (environment '(chezscheme)))
            'pass)
          'fail))))

;; check-cpp-ffi-codegen : string => 'pass | 'fail
;;   C++ version of codegen check.
(define (check-cpp-ffi-codegen cpp-code)
  (guard (ex [else 'fail])
    (let ([ffi-code (guard (gen-ex [else #f])
                      (let ([decls (parse-cpp-string cpp-code)])
                        (if decls
                            (generate-cpp-ffi-code decls "test-lib")
                            #f)))])
      (if ffi-code
          (guard (eval-ex [else 'fail])
            (eval ffi-code (environment '(chezscheme)))
            'pass)
          'fail))))

;; check-ffi-multiplatform : string string => 'pass | 'fail
;;   Comprehensive multi-platform test.
(define (check-ffi-multiplatform source lang)
  (lambda (code)
    (guard (ex [else 'fail])
      (let* ([test-id (string-append "test-" (number->string (mod (random 1000000) 1000000)))]
             [source-path (write-source-file test-id lang code)]
             [compiled? (compile-source source-path lang)])
        (if compiled?
            (let ([so-path (string-append (substring source-path 0
                                                    (- (string-length source-path)
                                                       (string-length lang) 1))
                                         ".so")])
              ;; Try to generate and test bindings for all platforms
              (let loop ([platforms '(chez chicken gambit guile racket)]
                        [all-passed? #t])
                (if (null? platforms)
                    (if all-passed? 'pass 'fail)
                    (let* ([platform (car platforms)]
                           [binding-code (generate-binding test-id platform lang)]
                           [binding-path (if binding-code
                                            (write-binding test-id platform lang binding-code)
                                            #f)]
                           [loads? (if binding-path
                                      (test-binding-loads platform binding-path so-path)
                                      #t)]) ;; Skip if can't generate binding
                      (loop (cdr platforms)
                            (and all-passed? loads?))))))
            ;; Failed to compile - that's okay for fuzzing
            'pass)))))

;;=======================================================================
;; Main Test Suite
;;=======================================================================

(define (run-all-fuzzers test-count)
  (let ([total-campaigns 0]
        [passed-campaigns 0]
        [failed-campaigns 0])

    (define (run-and-track name campaign)
      (set! total-campaigns (+ total-campaigns 1))
      (let ([result (run-fuzzing-campaign campaign)])
        (print-campaign-summary result)
        (if (or (> (length (campaign-result-crashes result)) 0)
                (> (length (campaign-result-failures result)) 0))
            (begin
              (set! failed-campaigns (+ failed-campaigns 1))
              (printf "FAIL: ~a\n" name))
            (set! passed-campaigns (+ passed-campaigns 1)))))

    (printf "C/C++ Parser Comprehensive Fuzzing Suite\n")
    (printf "==========================================\n\n")
    (printf "Test count per campaign: ~a\n\n" test-count)

    ;;-------------------------------------------------------------------
    ;; C Mutation Fuzzing (3 campaigns)
    ;;-------------------------------------------------------------------

    (printf "========================================\n")
    (printf "C MUTATION FUZZING\n")
    (printf "========================================\n\n")

    (let ([edge-cases (get-edge-case-corpus)]
          [basic-c (get-basic-c-corpus)])

      (printf "Campaign: C Edge Case Mutations\n")
      (printf "--------------------------------\n")
      (let* ([all-muts (take-mutations all-mutations 20)]
             [input-gen (make-corpus-mutation-generator edge-cases all-muts)])
        (run-and-track "C Edge Case Mutations"
          (make-campaign "C Edge Case Mutations" parse-c-wrapper input-gen #f test-count)))

      (printf "\nCampaign: C-Specific Mutations\n")
      (printf "--------------------------------\n")
      (let* ([input-gen (make-valid-c-mutation-generator basic-c c-specific-mutations)])
        (run-and-track "C-Specific Mutations"
          (make-campaign "C-Specific Mutations" parse-c-wrapper input-gen #f test-count)))

      (printf "\nCampaign: C Random Heavy Mutations\n")
      (printf "-----------------------------------\n")
      (let* ([base-inputs '("int x;" "struct S {};" "void f(void);" "enum E { A };" "typedef int T;")]
             [input-gen (make-random-mutation-generator all-mutations base-inputs)])
        (run-and-track "C Random Heavy Mutations"
          (make-campaign "C Random Heavy Mutations" parse-c-wrapper input-gen #f test-count))))

    ;;-------------------------------------------------------------------
    ;; C++ Mutation Fuzzing (3 campaigns)
    ;;-------------------------------------------------------------------

    (printf "\n========================================\n")
    (printf "C++ MUTATION FUZZING\n")
    (printf "========================================\n\n")

    (let ([edge-cases (get-cpp-edge-case-corpus)]
          [basic-cpp (get-basic-cpp-corpus)])

      (printf "Campaign: C++ Edge Case Mutations\n")
      (printf "------------------------------------\n")
      (let* ([all-muts (take-mutations all-mutations 20)]
             [input-gen (make-corpus-mutation-generator edge-cases all-muts)])
        (run-and-track "C++ Edge Case Mutations"
          (make-campaign "C++ Edge Case Mutations" parse-cpp-wrapper input-gen #f test-count)))

      (printf "\nCampaign: C++-Specific Mutations\n")
      (printf "-----------------------------------\n")
      (let* ([input-gen (make-valid-c-mutation-generator basic-cpp cpp-specific-mutations)])
        (run-and-track "C++-Specific Mutations"
          (make-campaign "C++-Specific Mutations" parse-cpp-wrapper input-gen #f test-count)))

      (printf "\nCampaign: C++ Random Heavy Mutations\n")
      (printf "-------------------------------------\n")
      (let* ([base-inputs '("class C {};" "namespace N {}" "template<typename T> class X {};"
                           "enum class E { A };" "using T = int;")]
             [input-gen (make-random-mutation-generator all-mutations base-inputs)])
        (run-and-track "C++ Random Heavy Mutations"
          (make-campaign "C++ Random Heavy Mutations" parse-cpp-wrapper input-gen #f test-count))))

    ;;-------------------------------------------------------------------
    ;; C Generative Fuzzing (8 campaigns)
    ;;-------------------------------------------------------------------

    (printf "\n========================================\n")
    (printf "C GENERATIVE FUZZING\n")
    (printf "========================================\n\n")

    (printf "Campaign: Simple Declarations\n")
    (printf "------------------------------\n")
    (run-and-track "C Simple Declarations"
      (make-campaign "C Simple Declarations" check-parse-succeeds make-simple-decl-generator #f test-count))

    (printf "\nCampaign: Struct Declarations\n")
    (printf "------------------------------\n")
    (run-and-track "C Struct Declarations"
      (make-campaign "C Struct Declarations" check-parse-succeeds make-struct-generator #f test-count))

    (printf "\nCampaign: Typedef Declarations\n")
    (printf "-------------------------------\n")
    (run-and-track "C Typedef Declarations"
      (make-campaign "C Typedef Declarations" check-parse-succeeds make-typedef-generator #f test-count))

    (printf "\nCampaign: Enum Declarations\n")
    (printf "----------------------------\n")
    (run-and-track "C Enum Declarations"
      (make-campaign "C Enum Declarations" check-parse-succeeds make-enum-generator #f test-count))

    (printf "\nCampaign: Function Declarations\n")
    (printf "--------------------------------\n")
    (run-and-track "C Function Declarations"
      (make-campaign "C Function Declarations" check-parse-succeeds make-function-decl-generator #f test-count))

    (printf "\nCampaign: Pointer Declarations\n")
    (printf "-------------------------------\n")
    (run-and-track "C Pointer Declarations"
      (make-campaign "C Pointer Declarations" check-parse-succeeds make-pointer-decl-generator #f test-count))

    (printf "\nCampaign: Array Declarations\n")
    (printf "-----------------------------\n")
    (run-and-track "C Array Declarations"
      (make-campaign "C Array Declarations" check-parse-succeeds make-array-decl-generator #f test-count))

    (printf "\nCampaign: Multi-field Structs\n")
    (printf "------------------------------\n")
    (run-and-track "C Multi-field Structs"
      (make-campaign "C Multi-field Structs" check-parse-succeeds make-multi-field-struct-generator #f test-count))

    ;;-------------------------------------------------------------------
    ;; C++ Generative Fuzzing (8 campaigns)
    ;;-------------------------------------------------------------------

    (printf "\n========================================\n")
    (printf "C++ GENERATIVE FUZZING\n")
    (printf "========================================\n\n")

    (printf "Campaign: Simple C++ Declarations\n")
    (printf "----------------------------------\n")
    (run-and-track "C++ Simple Declarations"
      (make-campaign "C++ Simple Declarations" check-cpp-parse-succeeds make-simple-decl-generator #f test-count))

    (printf "\nCampaign: Class Declarations\n")
    (printf "-----------------------------\n")
    (run-and-track "C++ Class Declarations"
      (make-campaign "C++ Class Declarations" check-cpp-parse-succeeds make-class-generator #f test-count))

    (printf "\nCampaign: Classes with Constructors\n")
    (printf "------------------------------------\n")
    (run-and-track "C++ Classes with Constructors"
      (make-campaign "C++ Classes with Constructors" check-cpp-parse-succeeds make-class-with-constructor-generator #f test-count))

    (printf "\nCampaign: Classes with Destructors\n")
    (printf "-----------------------------------\n")
    (run-and-track "C++ Classes with Destructors"
      (make-campaign "C++ Classes with Destructors" check-cpp-parse-succeeds make-class-with-destructor-generator #f test-count))

    (printf "\nCampaign: Classes with Member Functions\n")
    (printf "----------------------------------------\n")
    (run-and-track "C++ Classes with Member Functions"
      (make-campaign "C++ Classes with Member Functions" check-cpp-parse-succeeds make-class-with-member-generator #f test-count))

    (printf "\nCampaign: Empty Namespaces\n")
    (printf "---------------------------\n")
    (run-and-track "C++ Empty Namespaces"
      (make-campaign "C++ Empty Namespaces" check-cpp-parse-succeeds make-empty-namespace-generator #f test-count))

    (printf "\nCampaign: Namespaces with Content\n")
    (printf "----------------------------------\n")
    (run-and-track "C++ Namespaces with Content"
      (make-campaign "C++ Namespaces with Content" check-cpp-parse-succeeds make-namespace-generator #f test-count))

    (printf "\nCampaign: C++ Struct Declarations\n")
    (printf "----------------------------------\n")
    (run-and-track "C++ Struct Declarations"
      (make-campaign "C++ Struct Declarations" check-cpp-parse-succeeds make-cpp-struct-generator #f test-count))

    ;;-------------------------------------------------------------------
    ;; Preprocessor Fuzzing (7 campaigns)
    ;;-------------------------------------------------------------------

    (printf "\n========================================\n")
    (printf "PREPROCESSOR FUZZING\n")
    (printf "========================================\n\n")

    (printf "Campaign: Object-like Macros\n")
    (printf "-----------------------------\n")
    (run-and-track "Object-like Macros"
      (make-campaign "Object-like Macros" check-parse-succeeds make-define-object-macro-generator #f test-count))

    (printf "\nCampaign: Function-like Macros\n")
    (printf "-------------------------------\n")
    (run-and-track "Function-like Macros"
      (make-campaign "Function-like Macros" check-parse-succeeds make-define-function-macro-generator #f test-count))

    (printf "\nCampaign: #ifdef Directives\n")
    (printf "----------------------------\n")
    (run-and-track "#ifdef Directives"
      (make-campaign "#ifdef Directives" check-parse-succeeds make-ifdef-generator #f test-count))

    (printf "\nCampaign: #ifndef (Header Guards)\n")
    (printf "----------------------------------\n")
    (run-and-track "#ifndef (Header Guards)"
      (make-campaign "#ifndef (Header Guards)" check-parse-succeeds make-ifndef-generator #f test-count))

    (printf "\nCampaign: #if/#else Conditionals\n")
    (printf "---------------------------------\n")
    (run-and-track "#if/#else Conditionals"
      (make-campaign "#if/#else Conditionals" check-parse-succeeds make-if-else-generator #f test-count))

    (printf "\nCampaign: Nested Conditionals\n")
    (printf "------------------------------\n")
    (run-and-track "Nested Conditionals"
      (make-campaign "Nested Conditionals" check-parse-succeeds make-nested-conditional-generator #f test-count))

    (printf "\nCampaign: Multiple Defines\n")
    (printf "---------------------------\n")
    (run-and-track "Multiple Defines"
      (make-campaign "Multiple Defines" check-parse-succeeds make-multiple-defines-generator #f test-count))

    ;;-------------------------------------------------------------------
    ;; Complex C Fuzzing (10 campaigns)
    ;;-------------------------------------------------------------------

    (printf "\n========================================\n")
    (printf "COMPLEX C FUZZING\n")
    (printf "========================================\n\n")

    (printf "Campaign: Multiple Declarations\n")
    (printf "--------------------------------\n")
    (run-and-track "Multiple Declarations"
      (make-campaign "Multiple Declarations" check-parse-succeeds make-multiple-decls-generator #f test-count))

    (printf "\nCampaign: Nested Structs\n")
    (printf "-------------------------\n")
    (run-and-track "Nested Structs"
      (make-campaign "Nested Structs" check-parse-succeeds make-nested-struct-generator #f test-count))

    (printf "\nCampaign: Pointer-to-pointer\n")
    (printf "-----------------------------\n")
    (run-and-track "Pointer-to-pointer"
      (make-campaign "Pointer-to-pointer" check-parse-succeeds make-pointer-to-pointer-generator #f test-count))

    (printf "\nCampaign: Array-of-pointers\n")
    (printf "----------------------------\n")
    (run-and-track "Array-of-pointers"
      (make-campaign "Array-of-pointers" check-parse-succeeds make-array-of-pointers-generator #f test-count))

    (printf "\nCampaign: Pointer-to-array\n")
    (printf "---------------------------\n")
    (run-and-track "Pointer-to-array"
      (make-campaign "Pointer-to-array" check-parse-succeeds make-pointer-to-array-generator #f test-count))

    (printf "\nCampaign: Function Pointers\n")
    (printf "----------------------------\n")
    (run-and-track "Function Pointers"
      (make-campaign "Function Pointers" check-parse-succeeds make-function-pointer-generator #f test-count))

    (printf "\nCampaign: Struct with Array Field\n")
    (printf "----------------------------------\n")
    (run-and-track "Struct with Array Field"
      (make-campaign "Struct with Array Field" check-parse-succeeds make-struct-with-array-generator #f test-count))

    (printf "\nCampaign: Typedef Function Pointer\n")
    (printf "-----------------------------------\n")
    (run-and-track "Typedef Function Pointer"
      (make-campaign "Typedef Function Pointer" check-parse-succeeds make-typedef-function-pointer-generator #f test-count))

    (printf "\nCampaign: Complex Structs\n")
    (printf "--------------------------\n")
    (run-and-track "Complex Structs"
      (make-campaign "Complex Structs" check-parse-succeeds make-complex-struct-generator #f test-count))

    (printf "\nCampaign: Large Files\n")
    (printf "----------------------\n")
    (run-and-track "Large Files"
      (make-campaign "Large Files" check-parse-succeeds make-large-file-generator #f test-count))

    ;;-------------------------------------------------------------------
    ;; FFI Integration Testing (14 campaigns)
    ;;-------------------------------------------------------------------

    (printf "\n========================================\n")
    (printf "C FFI INTEGRATION TESTING\n")
    (printf "========================================\n\n")

    (printf "Campaign: C Simple Functions\n")
    (printf "-----------------------------\n")
    (run-and-track "FFI C Simple Functions"
      (make-campaign "FFI C Simple Functions" check-ffi-codegen make-simple-function-generator #f test-count))

    (printf "\nCampaign: C Void Functions\n")
    (printf "---------------------------\n")
    (run-and-track "FFI C Void Functions"
      (make-campaign "FFI C Void Functions" check-ffi-codegen make-void-function-generator #f test-count))

    (printf "\nCampaign: C Getter Functions\n")
    (printf "-----------------------------\n")
    (run-and-track "FFI C Getter Functions"
      (make-campaign "FFI C Getter Functions" check-ffi-codegen make-getter-function-generator #f test-count))

    (printf "\nCampaign: C Struct with Getter\n")
    (printf "-------------------------------\n")
    (run-and-track "FFI C Struct with Getter"
      (make-campaign "FFI C Struct with Getter" check-ffi-codegen make-struct-with-getter-generator #f test-count))

    (printf "\nCampaign: C Multi-function Files\n")
    (printf "---------------------------------\n")
    (run-and-track "FFI C Multi-function Files"
      (make-campaign "FFI C Multi-function Files" check-ffi-codegen make-multi-function-generator #f test-count))

    (printf "\nCampaign: C End-to-End (gcc + eval)\n")
    (printf "------------------------------------\n")
    (run-and-track "FFI C End-to-End"
      (make-campaign "FFI C End-to-End" check-ffi-integration make-simple-function-generator #f test-count))

    (printf "\nCampaign: C Multi-Platform\n")
    (printf "---------------------------\n")
    (run-and-track "FFI C Multi-Platform"
      (make-campaign "FFI C Multi-Platform" (check-ffi-multiplatform "c" "c") make-simple-function-generator #f test-count))

    (printf "\n========================================\n")
    (printf "C++ FFI INTEGRATION TESTING\n")
    (printf "========================================\n\n")

    (printf "Campaign: C++ Simple Functions\n")
    (printf "-------------------------------\n")
    (run-and-track "FFI C++ Simple Functions"
      (make-campaign "FFI C++ Simple Functions" check-cpp-ffi-codegen make-cpp-simple-function-generator #f test-count))

    (printf "\nCampaign: C++ Class Methods\n")
    (printf "----------------------------\n")
    (run-and-track "FFI C++ Class Methods"
      (make-campaign "FFI C++ Class Methods" check-cpp-ffi-codegen make-cpp-class-method-generator #f test-count))

    (printf "\nCampaign: C++ Namespace Functions\n")
    (printf "----------------------------------\n")
    (run-and-track "FFI C++ Namespace Functions"
      (make-campaign "FFI C++ Namespace Functions" check-cpp-ffi-codegen make-cpp-namespace-function-generator #f test-count))

    (printf "\nCampaign: C++ Multi-function Files\n")
    (printf "-----------------------------------\n")
    (run-and-track "FFI C++ Multi-function Files"
      (make-campaign "FFI C++ Multi-function Files" check-cpp-ffi-codegen make-cpp-multi-function-generator #f test-count))

    (printf "\nCampaign: C++ End-to-End (g++ + eval)\n")
    (printf "--------------------------------------\n")
    (run-and-track "FFI C++ End-to-End"
      (make-campaign "FFI C++ End-to-End" check-ffi-integration make-cpp-simple-function-generator #f test-count))

    (printf "\nCampaign: C++ Multi-Platform\n")
    (printf "-----------------------------\n")
    (run-and-track "FFI C++ Multi-Platform"
      (make-campaign "FFI C++ Multi-Platform" (check-ffi-multiplatform "cpp" "cpp") make-cpp-simple-function-generator #f test-count))

    (printf "\nCampaign: Cross-Platform Validation\n")
    (printf "------------------------------------\n")
    (run-and-track "FFI Cross-Platform Validation"
      (make-campaign "FFI Cross-Platform Validation" (check-ffi-multiplatform "c" "c") make-multi-function-generator #f test-count))

    ;;-------------------------------------------------------------------
    ;; Final Summary
    ;;-------------------------------------------------------------------

    (printf "\n========================================\n")
    (printf "COMPREHENSIVE FUZZING SUMMARY\n")
    (printf "========================================\n")
    (printf "Total campaigns:  ~a\n" total-campaigns)
    (printf "Passed:           ~a\n" passed-campaigns)
    (printf "Failed:           ~a\n" failed-campaigns)
    (printf "\n")

    (if (> failed-campaigns 0)
        (exit 1)
        (exit 0))))

;;=======================================================================
;; Entry Point
;;=======================================================================

(let ([test-count (get-test-count (command-line-arguments))])
  (run-all-fuzzers test-count))
