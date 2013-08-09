MiniLisp
========

This is an implementation of Lisp in less than 1k lines of C. This language
can't do anything other than a simple math, but should be interesting to those
who want to see how dynamic scoping, macro system, and copying GC can be
implemented in C. The code is heavily commented to help the reader understand
how it works.

Compile
-------

    $ make

Test
----

    $ make test

Language features
-----------------

MiniLisp is a traditional Lisp interpreter. It reads a Lisp expression at a time
from the standard input, evaluate it, and then prints out the return value of
the expression. Here is an example of a valid input.

    (+ 1 2)

The above expression prints "3".

### Literals

MiniLisp supports integer literals, `()`, `t`, and list literals.

* Integer literals are positive or negative integers.
* `()` is the only false value. It also represents the empty list.
* `t` is a predefined variable evaluated to itself. It's a preferred way to
  represent a true value, while any non-`()` value is considered to be true.
* List literals are the cons cells quoted by `quote`. It's either a regular list
  whose last element's cdr is `()` or an dotted list ending with any non-`()`
  value. A dotted list can be written as `(a . b)`.

### Predefined functions

A predefined function `+` returns the sum of the arguments.

    (+ 1)      ; -> 1
    (+ 1 2)    ; -> 3
    (+ 1 2 3)  ; -> 6

The language does not have `-` operator. However, you can decrement a value by
adding a negative number.

    (+ 3 -7)   ; -> -4

`=` takes two arguments and returns `t` if the two are the same integer.

    (= 11 11)  ; -> t
    (= 11 6)   ; -> ()

`list` takes an arbitrary number of arguments and returns a new list consists of
the arguments.

    (list 'a (+ 2 5) 'zzz)  ; -> (a 7 zzz)

### Conditionals

`(if cond then else)` is the only conditional in the language. It first
evaluates *cond*. If the result is a true value, *then* is evaluated. Otherwise
*else* is evaluated.

### Definitions

MiniLisp supports variables and functions. They can be defined using `define`.

    (define a (+ 1 2))
    (+ a a)       ; should return 6

There are two ways to define a function. One way is to use a special form
`lambda`. `(lambda (args ...)  expr ...)` returns a function object which
you can assign to a variable using `define`.

    (define double (lambda (x) (+ x x)))
    (double 6)                ; -> 12
    ((lambda (x) (+ x x)) 6)  ; do the same thing without assignment

The other way is `defun`. `(defun fn (args ...) expr ...)` is short for
`(define fn (lambda (args ...) expr ...)`.

    ;; Define "double" using defun
    (defun double (x) (+ x x))

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
the control is passed to the function. What you can do with macros is extending
the language.

    (define x 0)
    (unless (= x 0) '(x is not 0))  ; -> ()
    (unless (= x 1) '(x is not 1))  ; -> (x is not 1)

`macroexpand` is a convenient special form to see the expanded form of a macro.

    (macroexpand (unless (= x 1) '(x is not 1)))
    ;; -> (if (= x 1) () (quote (x is not 1)))

### Comments

As in the traditional Lisp syntax, `;` (semicolon) starts a single line comment.
The comment continues to the end of line.
