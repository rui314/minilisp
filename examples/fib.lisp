(defun fib (n)
  (if (= n 0)
      n
      (if (= n 1)
          n
          (+ (fib (- n 1)) (fib (- n 2))))))

(fib 35)
