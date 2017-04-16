;;;
;;; CONWAY'S GAME OF LIFE
;;;

;; (PROGN EXPR ...)
;; => ((LAMBDA () EXPR ...))
(DEFMACRO PROGN (EXPR . REST)
  (LIST (CONS 'LAMBDA (CONS () (CONS EXPR REST)))))

(DEFUN LIST (X . Y)
  (CONS X Y))

(DEFUN NOT (X)
  (IF X () T))

;; (LET VAR VAL BODY ...)
;; => ((LAMBDA (VAR) BODY ...) VAL)
(DEFMACRO LET (VAR VAL . BODY)
  (CONS (CONS 'LAMBDA (CONS (LIST VAR) BODY))
	(LIST VAL)))

;; (AND E1 E2 ...)
;; => (IF E1 (AND E2 ...))
;; (AND E1)
;; => E1
(DEFMACRO AND (EXPR . REST)
  (IF REST
      (LIST 'IF EXPR (CONS 'AND REST))
    EXPR))

;; (OR E1 E2 ...)
;; => (LET <TMP> E1
;;      (IF <TMP> <TMP> (OR E2 ...)))
;; (OR E1)
;; => E1
;;
;; THE REASON TO USE THE TEMPORARY VARIABLES IS TO AVOID EVALUATING THE
;; ARGUMENTS MORE THAN ONCE.
(DEFMACRO OR (EXPR . REST)
  (IF REST
      (LET VAR (GENSYM)
           (LIST 'LET VAR EXPR
                 (LIST 'IF VAR VAR (CONS 'OR REST))))
    EXPR))

;; (WHEN EXPR BODY ...)
;; => (IF EXPR (PROGN BODY ...))
(DEFMACRO WHEN (EXPR . BODY)
  (CONS 'IF (CONS EXPR (LIST (CONS 'PROGN BODY)))))

;; (UNLESS EXPR BODY ...)
;; => (IF EXPR () BODY ...)
(DEFMACRO UNLESS (EXPR . BODY)
  (CONS 'IF (CONS EXPR (CONS () BODY))))

;;;
;;; NUMERIC OPERATORS
;;;

(DEFUN <= (E1 E2)
  (OR (< E1 E2)
      (= E1 E2)))

;;;
;;; LIST OPERATORS
;;;

;;; APPLIES EACH ELEMENT OF LIS TO FN, AND RETURNS THEIR RETURN VALUES AS A LIST.
(DEFUN MAP (LIS FN)
  (WHEN LIS
    (CONS (FN (CAR LIS))
	  (MAP (CDR LIS) FN))))

;; RETURNS NTH ELEMENT OF LIS.
(DEFUN NTH (LIS N)
  (IF (= N 0)
      (CAR LIS)
    (NTH (CDR LIS) (- N 1))))

;; RETURNS THE NTH TAIL OF LIS.
(DEFUN NTH-TAIL (LIS N)
  (IF (= N 0)
      LIS
    (NTH-TAIL (CDR LIS) (- N 1))))

;; RETURNS A LIST CONSISTS OF M .. N-1 INTEGERS.
(DEFUN %IOTA (M N)
  (UNLESS (<= N M)
    (CONS M (%IOTA (+ M 1) N))))

;; RETURNS A LIST CONSISTS OF 0 ... N-1 INTEGERS.
(DEFUN IOTA (N)
  (%IOTA 0 N))

;;;
;;; MAIN
;;;

(DEFINE WIDTH 5)
(DEFINE HEIGHT 5)

;; RETURNS LOCATION (X, Y)'S ELEMENT.
(DEFUN GET (BOARD X Y)
  (NTH (NTH BOARD Y) X))

;; RETURNS TRUE IF LOCATION (X, Y)'S VALUE IS "@".
(DEFUN ALIVE? (BOARD X Y)
  (AND (<= 0 X)
       (< X HEIGHT)
       (<= 0 Y)
       (< Y WIDTH)
       (EQ (GET BOARD X Y) '@)))

;; PRINT OUT THE GIVEN BOARD.
(DEFUN PRINT (BOARD)
  (IF (NOT BOARD)
      '$
    (PRINTLN (CAR BOARD))
    (PRINT (CDR BOARD))))

(DEFUN COUNT (BOARD X Y)
  (LET AT (LAMBDA (X Y)
            (IF (ALIVE? BOARD X Y) 1 0))
       (+ (AT (- X 1) (- Y 1))
          (AT (- X 1) Y)
          (AT (- X 1) (+ Y 1))
          (AT X (- Y 1))
          (AT X (+ Y 1))
          (AT (+ X 1) (- Y 1))
          (AT (+ X 1) Y)
          (AT (+ X 1) (+ Y 1)))))

(DEFUN NEXT (BOARD X Y)
  (LET C (COUNT BOARD X Y)
       (IF (ALIVE? BOARD X Y)
           (OR (= C 2) (= C 3))
         (= C 3))))

(DEFUN RUN (BOARD)
  (WHILE T
    (PRINT BOARD)
    (PRINTLN '*)
    (LET NEWBOARD (MAP (IOTA HEIGHT)
                       (LAMBDA (Y)
                         (MAP (IOTA WIDTH)
                              (LAMBDA (X)
                                (IF (NEXT BOARD X Y) '@ '_)))))
         (SETQ BOARD NEWBOARD))))

(RUN '((_ _ _ _ _)
       (_ _ _ _ _)
       (_ @ @ @ _)
       (_ _ _ @ _)
       (_ _ @ _ _)))
