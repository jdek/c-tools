;; Corpus Management - Test case collection and management

(library (c-tools testing fuzzing corpus)
  (export
    ;; Corpus operations
    load-corpus-from-directory
    load-corpus-from-file
    save-corpus-to-directory
    save-test-case

    ;; Corpus statistics
    corpus-size
    corpus-average-length
    corpus-statistics

    ;; Categorization
    categorize-test-case

    ;; Built-in corpora
    get-edge-case-corpus
    get-basic-c-corpus
    get-cpp-edge-case-corpus
    get-basic-cpp-corpus)

  (import (rnrs base)
          (rnrs control)
          (rnrs exceptions)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs lists)
          (only (chezscheme) directory-list file-exists? format))

  ;;=======================================================================
  ;; Helpers

  ;; filter-map : procedure list => list
  ;;   Maps proc over list and filters out #f values.
  (define (filter-map proc lst)
    (if (null? lst)
        '()
        (let ([result (proc (car lst))]
              [rest (filter-map proc (cdr lst))])
          (if result
              (cons result rest)
              rest))))

  ;;=======================================================================
  ;; Corpus Loading

  ;; load-corpus-from-directory : string => list-of-string
  ;;   Loads all .c files from a directory into a corpus.
  (define (load-corpus-from-directory dir-path)
    (guard (ex [else '()])
      (let ([files (directory-list dir-path)])
        (filter-map
          (lambda (filename)
            (if (string-suffix? filename ".c")
                (load-corpus-from-file
                  (string-append dir-path "/" filename))
                #f))
          files))))

  ;; string-suffix? : string string => boolean
  ;;   Checks if string ends with suffix.
  (define (string-suffix? str suffix)
    (let ([slen (string-length str)]
          [suflen (string-length suffix)])
      (and (>= slen suflen)
           (string=? (substring str (- slen suflen) slen) suffix))))

  ;; load-corpus-from-file : string => string | #f
  ;;   Loads a single file into corpus.
  (define (load-corpus-from-file file-path)
    (guard (ex [else #f])
      (call-with-input-file file-path
        (lambda (port)
          (get-string-all port)))))

  ;;=======================================================================
  ;; Corpus Saving

  ;; save-corpus-to-directory : list-of-string string => void
  ;;   Saves corpus to directory as numbered files.
  (define (save-corpus-to-directory corpus dir-path)
    (let loop ([cases corpus] [n 0])
      (unless (null? cases)
        (save-test-case (car cases) dir-path
                       (format "test-~a.c" n))
        (loop (cdr cases) (+ n 1)))))

  ;; save-test-case : string string string => void
  ;;   Saves a single test case to file.
  (define (save-test-case test-case dir-path filename)
    (guard (ex [else (if #f #f)])
      (let ([full-path (string-append dir-path "/" filename)])
        (call-with-output-file full-path
          (lambda (port)
            (put-string port test-case))
          'replace))))

  ;;=======================================================================
  ;; Corpus Statistics

  ;; corpus-size : list => fixnum
  ;;   Returns number of test cases in corpus.
  (define (corpus-size corpus)
    (length corpus))

  ;; corpus-average-length : list-of-string => real
  ;;   Returns average length of test cases.
  (define (corpus-average-length corpus)
    (if (null? corpus)
        0.0
        (let ([total-length (apply + (map string-length corpus))]
              [count (length corpus)])
          (/ total-length count))))

  ;; corpus-statistics : list-of-string => alist
  ;;   Returns statistics about the corpus.
  (define (corpus-statistics corpus)
    (list
      (cons 'size (corpus-size corpus))
      (cons 'average-length (corpus-average-length corpus))
      (cons 'total-bytes (apply + (map string-length corpus)))
      (cons 'min-length (if (null? corpus) 0 (apply min (map string-length corpus))))
      (cons 'max-length (if (null? corpus) 0 (apply max (map string-length corpus))))))

  ;;=======================================================================
  ;; Categorization

  ;; categorize-test-case : string => symbol
  ;;   Categorizes a test case by content.
  (define (categorize-test-case test-case)
    (cond
      [(string-contains? test-case "struct") 'struct]
      [(string-contains? test-case "enum") 'enum]
      [(string-contains? test-case "typedef") 'typedef]
      [(string-contains? test-case "(") 'function]
      [(string-contains? test-case "#include") 'preprocessor]
      [(string-contains? test-case "#define") 'macro]
      [else 'other]))

  ;; string-contains? : string string => boolean
  ;;   Checks if haystack contains needle.
  (define (string-contains? haystack needle)
    (and (string-search haystack needle 0) #t))

  ;; string-search : string string fixnum => fixnum | #f
  ;;   Searches for needle in haystack starting at start.
  (define (string-search haystack needle start)
    (define (string-prefix-at? str prefix pos)
      (let ([prefix-len (string-length prefix)])
        (and (<= (+ pos prefix-len) (string-length str))
             (let loop ([i 0])
               (if (>= i prefix-len)
                   #t
                   (and (char=? (string-ref str (+ pos i))
                               (string-ref prefix i))
                        (loop (+ i 1))))))))
    (let ([needle-len (string-length needle)]
          [hay-len (string-length haystack)])
      (let loop ([pos start])
        (if (> (+ pos needle-len) hay-len)
            #f
            (if (string-prefix-at? haystack needle pos)
                pos
                (loop (+ pos 1)))))))

  ;;=======================================================================
  ;; Built-in Corpora

  ;; get-edge-case-corpus : => list-of-string
  ;;   Returns a corpus of edge cases for C parsing.
  (define (get-edge-case-corpus)
    (list
      ;; Empty input
      ""

      ;; Whitespace only
      "   \n\t  "

      ;; Single tokens
      "int"
      ";"
      "*"
      "{"

      ;; Minimal valid declarations
      "int x;"
      "void f(void);"
      "struct S {};"
      "enum E { A };"
      "typedef int T;"

      ;; Deeply nested pointers
      "int **********p;"

      ;; Long identifier
      (make-string 1000 #\x)

      ;; Zero-length array (GNU extension)
      "struct Flex { int arr[0]; };"

      ;; Flexible array member
      "struct FAM { int count; char data[]; };"

      ;; Empty struct
      "struct Empty {};"

      ;; Function pointers
      "int (*fptr)(void);"
      "void (*signal(int, void (*)(int)))(int);"

      ;; Arrays of arrays
      "int matrix[10][20][30];"

      ;; Static inline function
      "static inline int helper(void) { return 42; }"

      ;; Unmatched braces
      "struct X {"
      "}"

      ;; Missing semicolon
      "int x"

      ;; Multiple pointers and arrays
      "int *(*arr[10])(void);"

      ;; Preprocessor conditionals
      "#ifdef FOO\nint x;\n#endif"

      ;; Complex macro
      "#define MAX(a,b) ((a)>(b)?(a):(b))"

      ;; Struct with bitfields
      "struct Bits { unsigned a:1; unsigned b:7; };"

      ;; Anonymous struct
      "struct { int x; int y; } point;"

      ;; Anonymous union
      "union { int i; float f; } u;"

      ;; Nested structs
      "struct Outer { struct Inner { int x; } in; };"

      ;; Forward declaration
      "struct Forward;"

      ;; Const volatile pointers
      "const volatile int * const * volatile p;"

      ;; Bool type
      "bool flag;"

      ;; Array with macro size
      "#define SIZE 24\nstruct Foo { int arr[SIZE]; };"

      ;; Very long line
      (string-append "int " (make-string 10000 #\x) ";")

      ;; Unicode in comments
      "int x; // \x4E2D;\x6587;"

      ;; Control characters
      (string-append "int" (string #\tab) "x" (string #\nul) ";")))

  ;; get-basic-c-corpus : => list-of-string
  ;;   Returns a corpus of basic valid C declarations.
  (define (get-basic-c-corpus)
    (list
      ;; Basic types
      "int x;"
      "char c;"
      "float f;"
      "double d;"
      "void *p;"

      ;; Unsigned types
      "unsigned int ui;"
      "unsigned char uc;"
      "unsigned long ul;"

      ;; Pointers
      "int *ptr;"
      "char **pptr;"
      "void ***ppptr;"

      ;; Arrays
      "int arr[10];"
      "char str[256];"
      "float matrix[3][3];"

      ;; Structs
      "struct Point { int x; int y; };"
      "struct Color { unsigned char r; unsigned char g; unsigned char b; };"
      "struct Node { int data; struct Node *next; };"

      ;; Enums
      "enum Status { OK, ERROR, PENDING };"
      "enum { RED = 0, GREEN = 1, BLUE = 2 };"

      ;; Typedefs
      "typedef int int32_t;"
      "typedef unsigned char uint8_t;"
      "typedef struct Point Point;"

      ;; Functions
      "int add(int a, int b);"
      "void print_message(const char *msg);"
      "int *get_array(void);"

      ;; Combined declarations
      "struct Rect { int x, y, w, h; };"
      "typedef struct { float x; float y; float z; } Vector3;"

      ;; Const/volatile
      "const int MAX = 100;"
      "volatile int status;"
      "const char * const name;"))

  ;;=======================================================================
  ;; C++ Built-in Corpora

  ;; get-cpp-edge-case-corpus : => list-of-string
  ;;   Returns a corpus of edge cases for C++ parsing.
  (define (get-cpp-edge-case-corpus)
    (list
      ;; Empty input
      ""

      ;; Whitespace only
      "   \n\t  "

      ;; Single tokens
      "class"
      "namespace"
      "template"
      "::"

      ;; Minimal valid declarations
      "class C {};"
      "namespace N {}"
      "template<typename T> class X {};"
      "using namespace std;"

      ;; Nested namespaces
      "namespace A { namespace B { class C {}; } }"
      "namespace A::B { class C {}; }"

      ;; Class with members
      "class Point { public: int x; int y; };"
      "class Base { virtual void f() = 0; };"

      ;; Templates with multiple parameters
      "template<typename T, typename U> class Pair {};"
      "template<int N> struct Array { int data[N]; };"

      ;; Template specialization
      "template<> class X<int> {};"

      ;; Function templates
      "template<typename T> T max(T a, T b);"

      ;; Operator overloading
      "class C { C operator+(const C& other); };"

      ;; Constructors/destructors
      "class C { C(); ~C(); };"
      "class C { C(int x) : x(x) {} int x; };"

      ;; Inheritance
      "class Derived : public Base {};"
      "class D : public A, private B {};"

      ;; Virtual functions
      "class C { virtual void f(); };"
      "class C { virtual void f() override; };"
      "class C { virtual void f() final; };"

      ;; Static members
      "class C { static int count; };"
      "class C { static void init(); };"

      ;; Friend declarations
      "class C { friend class D; };"
      "class C { friend void f(); };"

      ;; Nested classes
      "class Outer { class Inner {}; };"

      ;; Anonymous namespaces
      "namespace { int x; }"

      ;; Using declarations
      "using std::vector;"
      "using T = int;"

      ;; Const methods
      "class C { void f() const; };"

      ;; Reference types
      "void f(int& x);"
      "void f(int&& x);"

      ;; Default parameters
      "void f(int x = 0);"

      ;; Variadic templates
      "template<typename... Args> class Tuple {};"

      ;; Constexpr
      "constexpr int factorial(int n);"

      ;; Nullptr
      "int* ptr = nullptr;"

      ;; Auto keyword
      "auto x = 42;"

      ;; Lambda expressions (might not be supported, edge case)
      "auto f = [](int x) { return x * 2; };"

      ;; Deeply nested templates
      "template<template<typename> class C> class X {};"

      ;; Very long identifier
      (make-string 1000 #\x)

      ;; Unmatched braces
      "class X {"
      "}"

      ;; Missing semicolon
      "class X {}"

      ;; Template with >>
      "template<template<typename> class C> class X {};"
      "std::vector<std::vector<int>> matrix;"

      ;; Preprocessor with C++
      "#ifdef __cplusplus\nextern \"C\" {\n#endif"

      ;; Extern C linkage
      "extern \"C\" void f();"
      "extern \"C\" { void f(); void g(); }"

      ;; Inline namespace
      "inline namespace V1 { class C {}; }"

      ;; Deleted/defaulted functions
      "class C { C() = default; };"
      "class C { C(const C&) = delete; };"

      ;; Enum class
      "enum class Color { RED, GREEN, BLUE };"
      "enum class Status : int { OK = 0, ERROR = 1 };"

      ;; Noexcept
      "void f() noexcept;"
      "void f() noexcept(true);"

      ;; Alignas
      "alignas(16) int x;"
      "class alignas(32) Aligned {};"

      ;; Thread local
      "thread_local int x;"

      ;; Very long line
      (string-append "class " (make-string 10000 #\x) " {};")

      ;; Unicode in comments
      "class C {}; // \x4E2D;\x6587;"

      ;; Control characters
      (string-append "class" (string #\tab) "C" (string #\nul) " {};")))

  ;; get-basic-cpp-corpus : => list-of-string
  ;;   Returns a corpus of basic valid C++ declarations.
  (define (get-basic-cpp-corpus)
    (list
      ;; Simple classes
      "class Point { int x; int y; };"
      "class Empty {};"
      "class WithConstructor { WithConstructor(); };"

      ;; Namespaces
      "namespace math { int add(int a, int b); }"
      "namespace graphics { class Renderer {}; }"

      ;; Templates
      "template<typename T> class Container { T data; };"
      "template<int N> struct FixedArray { int arr[N]; };"
      "template<typename T> T min(T a, T b);"

      ;; Inheritance
      "class Base { virtual void f(); };"
      "class Derived : public Base { void f() override; };"

      ;; Operator overloading
      "class Complex { Complex operator+(const Complex& c); };"
      "class String { char& operator[](int i); };"

      ;; Constructors
      "class Vec2 { Vec2(float x, float y); };"
      "class Singleton { private: Singleton(); };"

      ;; Static members
      "class Counter { static int count; static void reset(); };"

      ;; Const methods
      "class Array { int size() const; };"

      ;; References
      "void swap(int& a, int& b);"
      "int& get_ref();"

      ;; Function overloading
      "void print(int x);"
      "void print(float x);"
      "void print(const char* x);"

      ;; Default arguments
      "void init(int value = 0);"
      "class Window { Window(int w = 800, int h = 600); };"

      ;; Using declarations
      "using std::string;"
      "using Point2D = Point;"

      ;; Enum class
      "enum class Color { RED, GREEN, BLUE };"

      ;; Extern C
      "extern \"C\" void c_function();"

      ;; Inline functions
      "inline int square(int x) { return x * x; };"

      ;; Virtual destructors
      "class Base { virtual ~Base(); };"

      ;; Pure virtual functions
      "class Interface { virtual void method() = 0; };"

      ;; Multiple inheritance
      "class C : public A, public B {};"

      ;; Friend functions
      "class Point { friend bool operator==(const Point& a, const Point& b); };"

      ;; Nested types
      "class Container { typedef int value_type; };"

      ;; Forward declarations
      "class Forward;"
      "namespace N { class C; }"

      ;; Const correctness
      "const int* ptr;"
      "int* const ptr;"
      "const int* const ptr;"

      ;; Noexcept
      "void safe_function() noexcept;"))

) ;; end library
