;;; Modified from original version located at:
;;; https://www.cs.sfu.ca/CourseCentral/310/pwfong/Lisp/1/tutorial1.html
(DEFUN FIB(N)
      (IF (<= N 1)
              N
              (+ (FIB (- N 1)) (FIB (- N 2)))))

