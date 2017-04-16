MINILISP
========

MINILISP is a TRS-80 Color Computer port of Rui Ueyama
minilisp which can be found on [https://github.com/rui314/minilisp](GitHub).

It supports most of the features of minilisp including the following:

- integers, symbols, cons cells,
- global variables,
- lexically-scoped local variables,
- closures,
- _if_ conditional,
- primitive functions, such as +, \*,  =, <, \>, <=, \>= and _list_,
- user-defined functions,
- a macro system,
- and a copying garbage collector with whopping 2KB buffers


Compile
-------

    $ make

Will create a minilisp.dsk image that can be used with MESS.


Language features
-----------------

MiniLisp is a traditional Lisp interpreter. It reads one expression at a time
from the standard input, evaluates it, and then prints out the return value of
the expression. Here is an example of a valid input.

    (+ 1 2)

The above expression prints "3".

### Literals

MiniLisp supports integer literals, `()`, `T`, symbols, and list literals.

* Integer literals are positive or negative integers.
* `()` is the only false value. It also represents the empty list.
* `T` is a predefined variable evaluated to itself. It's a preferred way to
  represent a true value, while any non-`()` value is considered to be true.
* Symbols are objects with unique name. They are used to represent identifiers.
  Because MiniLisp does not have string type, symbols are sometimes used as a
  substitute for strings too.
* List literals are cons cells. It's either a regular list whose last element's
  cdr is `()` or an dotted list ending with any non-`()` value. A dotted list is
  written as `(a . b)`.

### List operators

`CONS` takes two arguments and returns a new cons cell, making the first
argument the car, and the second the cdr.

    (CONS 'A 'B)   ; -> (A . B)
    (CONS 'A '(B)) ; -> (A B)

`CAR` and `CDR` are accessors for cons cells. `CAR` returns the CAR, and `CDR`
returns the CDR.

    (CAR '(A . B)) ; -> A
    (CDR '(A . B)) ; -> B

`SETCAR` mutates a cons cell. `SETCAR` takes two arguments, assuming the first
argument is a cons cell. It sets the second argument's value to the cons cell's
car.

    (DEFINE CELL (CONS 'A 'B))
    CELL; -> (A . B)
    (SETCAR CELL 'X)
    CELL; -> (X . B)

### Numeric operators

`+` returns the sum of the arguments.

    (+ 1)      ; -> 1
    (+ 1 2)    ; -> 3
    (+ 1 2 3)  ; -> 6

`-` negates the value of the argument if only one argument is given.

    (- 3)      ; -> -3
    (- -5)     ; -> 5

`*` returns the product of the arguments.

    (* 1)      ; -> 1
    (* 1 2)    ; -> 2
    (* 1 2 3)  ; -> 6

If multiple arguments are given, `-` subtracts each argument from the first one.

    (- 5 2)    ; -> 3
    (- 5 2 7)  ; -> -4

`=` takes two arguments and returns `T` if the two are the same integer.

    (= 11 11)  ; -> T
    (= 11 6)   ; -> ()

`<` takes two arguments and returns `T` if the first argument is smaller than
the second.

    (< 2 3)    ; -> T
    (< 3 3)    ; -> ()
    (< 4 3)    ; -> ()

`>` takes two arguments and returns `T` if the first argument is greater than
the second.

    (> 2 3)    ; -> ()
    (> 3 3)    ; -> ()
    (> 4 3)    ; -> T 

`<=` takes two arguments and returns `T` if the first argument is smaller than
or equal to the second.

    (<= 2 3)    ; -> T
    (<= 3 3)    ; -> T
    (<= 4 3)    ; -> ()

`>=` takes two arguments and returns `T` if the first argument is greater than
or equal to the second.

    (>= 2 3)    ; -> ()
    (>= 3 3)    ; -> T
    (>= 4 3)    ; -> T

### Conditionals

`(IF COND THEN ELSE)` is the only conditional in the language. It first
evaluates *COND*. If the result is a true value, *THEN* is evaluated. Otherwise
*ELSE* is evaluated.

### Loops

`(WHILE COND EXPR ...)` executes `EXPR ...` until `COND` is evaluated to
`()`. This is the only loop supported by MiniLisp.

If you are familiar with Scheme, you might be wondering if you could write a
loop by tail recursion in MiniLisp. The answer is no. Tail calls consume stack
space in MiniLisp, so a loop written as recursion will fail with the memory
exhaustion error.

### Equivalence test operators

`EQ` takes two arguments and returns `T` if the objects are the same. What `EQ`
really does is a pointer comparison, so two objects happened to have the same
contents but actually different are considered to not be the same by `EQ`.

### Output operators

`PRINTLN` prints a given object to the standard output.

    (PRINTLN 3)               ; prints "3"
    (PRINTLN '(HELLO WORLD))  ; prints "(HELLO WORLD)"

### Definitions

MiniLisp supports variables and functions. They can be defined using `DEFINE`.

    (DEFINE A (+ 1 2))
    (+ A A)   ; -> 6

There are two ways to define a function. One way is to use a special form
`LAMBDA`. `(LAMBDA(args ...)  expr ...)` returns a function object which
you can assign to a variable using `define`.

    (DEFINE DOUBLE (LAMBDA (X) (+ X X)))
    (DOUBLE 6)                ; -> 12
    ((LAMBDA (X) (+ X X)) 6)  ; do the same thing without assignment

The other way is `DEFUN`. `(DEFUN FN (ARGS ...) EXPR ...)` is short for
`(DEFINE FN (LAMBDA (ARGS ...) EXPR ...)`.

    ;; Define "DOUBLE" using DEBUN
    (DEFUN DOUBLE (X) (+ X X))

You can write a function that takes variable number of arguments. If the
parameter list is a dotted list, the remaining arguments are bound to the last
parameter as a list.

    (DEFUB FN (EXPR . REST) REST)
    (FN 1)     ; -> ()
    (FN 1 2 3) ; -> (2 3)

Variables are lexically scoped and have indefinite extent. References to "outer"
variables remain valid even after the function that created the variables
returns.

    ;; A countup function. We use lambda to introduce local variables because we
    ;; do not have "let" and the like.
    (DEFINE COUNTER 
      ((LAMBDA (COUNT)
         (LAMBDA ()
           (SETQ COUNT (+ COUNT 1))
           COUNT))
       0))

    (COUNTER)  ; -> 1
    (COUNTER)  ; -> 2

    ;; This will not return 12345 but 3. Variable "count" in counter function
    ;; is resolved based on its lexical context rather than dynamic context.
    ((LAMBDA (COUNT) (COUNTER)) 12345)  ; -> 3

`SETQ` sets a new value to an existing variable. It's an error if the variable
is not defined.

    (DEFINE VAL (+ 3 5))
    (SETQ VAL (+ VAL 1))  ; increment "VAL"

### Macros

Macros look similar to functions, but they are different that macros take an
expression as input and returns a new expression as output. `(defmacro
macro-name (args ...) body ...)` defines a macro. Here is an example.

    (DEFMACRO UNLESS (CONDITION EXPR)
      (LIST 'IF CONDITION () EXPR))

The above `DEFMACRO` defines a new macro *UNLESS*. *UNLESS* is a new conditional
which evaluates *EXPR* unless *CONDITION* is a true value. You cannot do the
same thing with a function because all the arguments would be evaluated before
the control is passed to the function.

    (DEFINE X 0)
    (UNLESS (= X 0) '(X IS NOT 0))  ; -> ()
    (UNLESS (= x 1) '(X IS NOT 1))  ; -> (X IS NOT 1)

`MACROEXPAND` is a convenient special form to see the expanded form of a macro.

    (MACROEXPAND (UNLESS (= x 1) '(X IS NOT 1)))
    ;; -> (IF (= x 1) () (quote (X IS NOT 1)))

`GENSYM` creates a new symbol which will never be `eq` to any other symbol other
than itself. Useful for writing a macro that introduces new identifiers.

    (GENSYM)   ; -> a new symbol

### Comments

As in the traditional Lisp syntax, `;` (semicolon) starts a single line comment.
The comment continues to the end of line.


### Loading Programs

Programs can be loaded via the `LOAD` primitive.

    (LOAD 'FILE)

will load the program from disk named "FILE.LSP" as if it were typed in.

