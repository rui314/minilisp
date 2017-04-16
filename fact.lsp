(DEFUN FACT(X)
  (IF (<= X 0)
      1
      (* X (FACT (- X 1)))))
