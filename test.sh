#!/bin/bash

function run() {
  echo -n "Testing $1 ... "

  error=$(echo "$3" | ./minilisp 2>&1 > /dev/null)
  if [ -n "$error" ]; then
    echo FAILED
    echo ERROR: "$error"
    exit 1
  fi

  result=$(echo "$3" | ./minilisp 2> /dev/null | tail -1)
  if [ "$result" != "$2" ]; then
    echo FAILED
    echo "$2 expected, but got $result"
    exit 1
  fi
  echo ok
}

run integer 1 1
run symbol a "'a"
run quote a "(quote a)"
run quote 63 "'63"
run quote '(+ 1 2)' "'(+ 1 2)"
run '+' 3 '(+ 1 2)'
run list "(a b c)" "(list 'a 'b 'c)"

run define 7 '(define x 7) x'
run define 10 '(define x 7) (+ x 3)'
run define 7 '(define + 7) +'
run setq 11 '(define x 7) (setq x 11) x'
run setq 17 '(setq + 17) +'

run if a "(if 1 'a 'b)"
run if a "(if 0 'a 'b)"
run if a "(if 'x 'a 'b)"
run if b "(if '() 'a 'b)"

run = t '(= 3 3)'
run = '()' '(= 3 2)'

run lambda '<function>' '(lambda (x) x)'
run lambda 9 '((lambda (x) (+ x x x)) 3)'
run defun 12 '(defun double (x) (+ x x)) (double 6)'

run macro 42 "
  (defmacro if-zero (x then) (list 'if (list '= x 0) then))
  (if-zero 0 42)"

run macroexpand '(quote (if-zero x (print x)))' "
  (defmacro if-zero (x then) (list 'if (list '= x 0) then))
  (macroexpand '(if-zero x (print x)))"
