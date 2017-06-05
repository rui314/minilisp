;;; modified from original version located at:
;;; https://www.cs.sfu.ca/coursecentral/310/pwfong/lisp/1/tutorial1.html
(defun fib(n)
      (if (<= n 1)
              n
              (+ (fib (- n 1)) (fib (- n 2)))))

