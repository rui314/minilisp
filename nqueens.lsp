;;;
;;; n-queens puzzle solver.
;;;
;;; the n queens puzzle is the problem of placing n chess queens on an n x n
;;; chessboard so that no two queens attack each
;;; other. http://en.wikipedia.org/wiki/eight_queens_puzzle
;;;
;;; this program solves n-queens puzzle by depth-first backtracking.
;;;

;;;
;;; basic macros
;;;
;;; because the language does not have quasiquote, we need to construct an
;;; expanded form using cons and list.
;;;

;; (progn expr ...)
;; => ((lambda () expr ...))
(defmacro progn (expr . rest)
  (list (cons 'lambda (cons () (cons expr rest)))))

(defun list (x . y)
  (cons x y))

(defun not (x)
  (if x () T))

;; (let1 var val body ...)
;; => ((lambda (var) body ...) val)
(defmacro let1 (var val . body)
  (cons (cons 'lambda (cons (list var) body))
	(list val)))

;; (and e1 e2 ...)
;; => (if e1 (and e2 ...))
;; (and e1)
;; => e1
(defmacro and (expr . rest)
  (if rest
      (list 'if expr (cons 'and rest))
    expr))

;; (or e1 e2 ...)
;; => (let1 <tmp> e1
;;      (if <tmp> <tmp> (or e2 ...)))
;; (or e1)
;; => e1
;;
;; the reason to use the temporary variables is to avoid evaluating the
;; arguments more than once.
(defmacro or (expr . rest)
  (if rest
      (let1 var (gensym)
	    (list 'let1 var expr
		  (list 'if var var (cons 'or rest))))
    expr))

;; (when expr body ...)
;; => (if expr (progn body ...))
(defmacro when (expr . body)
  (cons 'if (cons expr (list (cons 'progn body)))))

;; (unless expr body ...)
;; => (if expr () body ...)
(defmacro unless (expr . body)
  (cons 'if (cons expr (cons () body))))

;;;
;;; numeric operators
;;;

(defun <= (e1 e2)
  (or (< e1 e2)
      (= e1 e2)))

;;;
;;; list operators
;;;

;; applies each element of lis to pred. if pred returns a true value, terminate
;; the evaluation and returns pred's return value. if all of them return (),
;; returns ().
(defun any (lis pred)
  (when lis
    (or (pred (car lis))
	(any (cdr lis) pred))))

;;; applies each element of lis to fn, and returns their return values as a list.
(defun map (lis fn)
  (when lis
    (cons (fn (car lis))
	  (map (cdr lis) fn))))

;; returns nth element of lis.
(defun nth (lis n)
  (if (= n 0)
      (car lis)
    (nth (cdr lis) (- n 1))))

;; returns the nth tail of lis.
(defun nth-tail (lis n)
  (if (= n 0)
      lis
    (nth-tail (cdr lis) (- n 1))))

;; returns a list consists of m .. n-1 integers.
(defun %iota (m n)
  (unless (<= n m)
    (cons m (%iota (+ m 1) n))))

;; returns a list consists of 0 ... n-1 integers.
(defun iota (n)
  (%iota 0 n))

;; returns a new list whose length is len and all members are init.
(defun make-list (len init)
  (unless (= len 0)
    (cons init (make-list (- len 1) init))))

;; applies fn to each element of lis.
(defun for-each (lis fn)
  (or (not lis)
      (progn (fn (car lis))
	     (for-each (cdr lis) fn))))

;;;
;;; n-queens solver
;;;

;; creates size x size list filled with symbol "x".
(defun make-board (size)
  (map (iota size)
       (lambda (_)
	 (make-list size 'x))))

;; returns location (x, y)'s element.
(defun get (board x y)
  (nth (nth board x) y))

;; set symbol "@" to location (x, y).
(defun set (board x y)
  (setcar (nth-tail (nth board x) y) '@))

;; set symbol "x" to location (x, y).
(defun clear (board x y)
  (setcar (nth-tail (nth board x) y) 'x))

;; returns true if location (x, y)'s value is "@".
(defun set? (board x y)
  (eq (get board x y) '@))

;; print out the given board.
(defun print (board)
  (if (not board)
      '$
    (println (car board))
    (print (cdr board))))

;; returns true if we cannot place a queen at position (x, y), assuming that
;; queens have already been placed on each row from 0 to x-1.
(defun conflict? (board x y)
  (any (iota x)
       (lambda (n)
	 (or
	  ;; check if there's no conflicting queen upward
	  (set? board n y)
	  ;; upper left
	  (let1 z (+ y (- n x))
		(and (<= 0 z)
		     (set? board n z)))
	  ;; upper right
	  (let1 z (+ y (- x n))
		(and (< z board-size)
		     (set? board n z)))))))

;; find positions where we can place queens at row x, and continue searching for
;; the next row.
(defun %solve (board x)
  (if (= x board-size)
      ;; problem solved
      (progn (print board)
	     (println '$))
    (for-each (iota board-size)
	      (lambda (y)
		(unless (conflict? board x y)
		  (set board x y)
		  (%solve board (+ x 1))
		  (clear board x y))))))

(defun solve (board)
  (println 'start)
  (%solve board 0)
  (println 'done))

;;;
;;; main
;;;

(define board-size 4)
(define board (make-board board-size))
(solve board)
