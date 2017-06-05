MINILISP
========

MINILISP is a TRS-80 Color Computer port of Rui Ueyama
minilisp which can be found on [https://github.com/rui314/minilisp](https://github.com/rui314/minilisp).

It supports most of the features of minilisp including the following:

- integers, symbols, cons cells,
- global variables,
- lexically-scoped local variables,
- closures,
- _if_ conditional,
- primitive functions, such as +, \*,  =, <, \>, <=, \>= and _list_,
- user-defined functions,
- a macro system,
- and a copying garbage collector with whopping 15KB buffers


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

`cons` takes two arguments and returns a new cons cell, making the first
argument the car, and the second the cdr.

    (cons 'a 'b)   ; -> (a . b)
    (cons 'a '(b)) ; -> (a b)

`car` and `cdr` are accessors for cons cells. `car` returns the car, and `cdr`
returns the cdr.

    (car '(a . b)) ; -> a
    (cdr '(a . b)) ; -> b

`setcar` mutates a cons cell. `setcar` takes two arguments, assuming the first
argument is a cons cell. It sets the second argument's value to the cons cell's
car.

    (define cell (cons 'a 'b))
    cell; -> (a . b)
    (setcar cell 'x)
    cell; -> (x . b)

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

`(if cond then else)` is the only conditional in the language. It first
evaluates *cond*. If the result is a true value, *then* is evaluated. Otherwise
*else* is evaluated.

### Loops

`(while cond expr ...)` executes `expr ...` until `cond` is evaluated to
`()`. This is the only loop supported by MiniLisp.

If you are familiar with Scheme, you might be wondering if you could write a
loop by tail recursion in MiniLisp. The answer is no. Tail calls consume stack
space in MiniLisp, so a loop written as recursion will fail with the memory
exhaustion error.

### Equivalence test operators

`eq` takes two arguments and returns `T` if the objects are the same. What `eq`
really does is a pointer comparison, so two objects happened to have the same
contents but actually different are considered to not be the same by `eq`.

### Output operators

`println` prints a given object to the standard output.

    (println 3)               ; prints "3"
    (println '(hello world))  ; prints "(hello world)"

### Definitions

MiniLisp supports variables and functions. They can be defined using `define`.

    (define a (+ 1 2))
    (+ a a)   ; -> 6

There are two ways to define a function. One way is to use a special form
`lambda`. `(lambda(args ...)  expr ...)` returns a function object which
you can assign to a variable using `define`.

    (define double (lambda (x) (+ x x)))
    (double 6)                ; -> 12
    ((lambda (x) (+ x x)) 6)  ; do the same thing without assignment

The other way is `defun`. `(defun fn (ARGS ...) expr ...)` is short for
`(define fn (lambda (ARGS ...) expr ...)`.

    ;; Define "double" using defun
    (defun double (x) (+ x x))

You can write a function that takes variable number of arguments. If the
parameter list is a dotted list, the remaining arguments are bound to the last
parameter as a list.

    (defun fn (expr . rest) rest)
    (fn 1)     ; -> ()
    (fn 1 2 3) ; -> (2 3)

Variables are lexically scoped and have indefinite extent. References to "outer"
variables remain valid even after the function that created the variables
returns.

    ;; A countup function. We use lambda to introduce local variables because we
    ;; do not have "let" and the like.
    (define counter 
      ((lambda (count)
         (lambda ()
           (setq count (+ count 1))
           count))
       0))

    (counter)  ; -> 1
    (counter)  ; -> 2

    ;; This will not return 12345 but 3. Variable "count" in counter function
    ;; is resolved based on its lexical context rather than dynamic context.
    ((lambda (count) (counter)) 12345)  ; -> 3

`setq` sets a new value to an existing variable. It's an error if the variable
is not defined.

    (define val (+ 3 5))
    (setq val (+ val 1))  ; increment "val"

### Macros

Macros look similar to functions, but they are different that macros take an
expression as input and returns a new expression as output. `(defmacro
macro-name (args ...) body ...)` defines a macro. Here is an example.

    (defmacro unless (condition expr)
      (list 'if condition () expr))

The above `defmacro` defines a new macro *unless*. *unless* is a new conditional
which evaluates *expr* unless *condition* is a true value. You cannot do the
same thing with a function because all the arguments would be evaluated before
the control is passed to the function.

    (define x 0)
    (unless (= x 0) '(x is not 0))  ; -> ()
    (unless (= x 1) '(x is not 1))  ; -> (x is not 1)

`macroexpand` is a convenient special form to see the expanded form of a macro.

    (macroexpand (unless (= x 1) '(x IS NOT 1)))
    ;; -> (if (= x 1) () (quote (x IS NOT 1)))

`gensym` creates a new symbol which will never be `eq` to any other symbol other
than itself. Useful for writing a macro that introduces new identifiers.

    (gensym)   ; -> a new symbol

### Comments

As in the traditional Lisp syntax, `;` (semicolon) starts a single line comment.
The comment continues to the end of line.


### Loading Programs

Programs can be loaded via the `load` primitive.

    (load 'file)

will load the program from disk named "file.lsp" as if it were typed in.

