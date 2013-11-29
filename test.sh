#!/bin/bash

function fail() {
  echo -n -e '\e[1;31m[ERROR]\e[0m '
  echo "$1"
  exit 1
}

function do_run() {
  error=$(echo "$3" | ./minilisp 2>&1 > /dev/null)
  if [ -n "$error" ]; then
    echo FAILED
    fail "$error"
  fi

  result=$(echo "$3" | ./minilisp 2> /dev/null | tail -1)
  if [ "$result" != "$2" ]; then
    echo FAILED
    fail "$2 expected, but got $result"
  fi
}

function run() {
  echo -n "Testing $1 ... "
  # Run the tests twice to test the garbage collector with different settings.
  MINILISP_ALWAYS_GC= do_run "$@"
  MINILISP_ALWAYS_GC=1 do_run "$@"
  echo ok
}

# Basic data types
run integer 1 1
run integer -1 -1
run symbol a "'a"
run quote a "(quote a)"
run quote 63 "'63"
run quote '(+ 1 2)' "'(+ 1 2)"

run + 3 '(+ 1 2)'
run + -2 '(+ 1 -3)'

run 'unary -' -3 '(- 3)'
run '-' -2 '(- 3 5)'
run '-' -9 '(- 3 5 7)'

run '<' t '(< 2 3)'
run '<' '()' '(< 3 3)'
run '<' '()' '(< 4 3)'

run 'literal list' '(a b c)' "'(a b c)"
run 'literal list' '(a b . c)' "'(a b . c)"

# List manipulation
run cons "(a . b)" "(cons 'a 'b)"
run cons "(a b c)" "(cons 'a (cons 'b (cons 'c ())))"

run car a "(car '(a b c))"
run cdr "(b c)" "(cdr '(a b c))"

run setcar "(x . b)" "(define obj (cons 'a 'b)) (setcar obj 'x) obj"

# Comments
run comment 5 "
  ; 2
  5 ; 3"

# Global variables
run define 7 '(define x 7) x'
run define 10 '(define x 7) (+ x 3)'
run define 7 '(define + 7) +'
run setq 11 '(define x 7) (setq x 11) x'
run setq 17 '(setq + 17) +'

# Conditionals
run if a "(if 1 'a)"
run if '()' "(if () 'a)"
run if a "(if 1 'a 'b)"
run if a "(if 0 'a 'b)"
run if a "(if 'x 'a 'b)"
run if b "(if () 'a 'b)"
run if c "(if () 'a 'b 'c)"

# Numeric comparisons
run = t '(= 3 3)'
run = '()' '(= 3 2)'

# eq
run eq t "(eq 'foo 'foo)"
run eq t "(eq + +)"
run eq '()' "(eq 'foo 'bar)"
run eq '()' "(eq + 'bar)"

# gensym
run gensym G__0 '(gensym)'
run gensym '()' "(eq (gensym) 'G__0)"
run gensym '()' '(eq (gensym) (gensym))'
run gensym t '((lambda (x) (eq x x)) (gensym))'

# Functions
run lambda '<function>' '(lambda (x) x)'
run lambda t '((lambda () t))'
run lambda 9 '((lambda (x) (+ x x x)) 3)'
run defun 12 '(defun double (x) (+ x x)) (double 6)'

run args 15 '(defun f (x y z) (+ x y z)) (f 3 5 7)'

run restargs '(3 5 7)' '(defun f (x . y) (cons x y)) (f 3 5 7)'
run restargs '(3)'    '(defun f (x . y) (cons x y)) (f 3)'

# Lexical closures
run closure 3 '(defun call (f) ((lambda (var) (f)) 5))
  ((lambda (var) (call (lambda () var))) 3)'

run counter 3 '
  (define counter
    ((lambda (val)
       (lambda () (setq val (+ val 1)) val))
     0))
  (counter)
  (counter)
  (counter)'

# While loop
run while 45 "
  (define i 0)
  (define sum 0)
  (while (< i 10)
    (setq sum (+ sum i))
    (setq i (+ i 1)))
  sum"

# Macros
run macro 42 "
  (defun list (x . y) (cons x y))
  (defmacro if-zero (x then) (list 'if (list '= x 0) then))
  (if-zero 0 42)"

run macro 7 '(defmacro seven () 7) ((lambda () (seven)))'

run macroexpand '(if (= x 0) (print x))' "
  (defun list (x . y) (cons x y))
  (defmacro if-zero (x then) (list 'if (list '= x 0) then))
  (macroexpand (if-zero x (print x)))"


# Sum from 0 to 10
run recursion 55 '(defun f (x) (if (= x 0) 0 (+ (f (+ x -1)) x))) (f 10)'
