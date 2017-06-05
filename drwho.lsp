(defun make-dr(num phrase)
  (lambda (msg)
    (if (eq msg 'num)
      num
      (if (eq msg 'phrase)
        phrase
        'unknown_msg))))


(define hartnell (make-dr 1 'hmmn))
(define troughton (make-dr 2 'i-dont-like-it))
(define pertwee (make-dr 3 'reverse-the-polarity))
(define tom-baker (make-dr 4 'jelly-baby))
(define davison (make-dr 5 'not-to-reverse-the-polarity-of-the-neutron-flow))
(define colin-baker (make-dr 6 'an-alien-spy))
(define mccoy (make-dr 7 'im-ready))
(define mcgann (make-dr 8 'im-ready))
(define eccleston (make-dr 9 'fantastic))
(define tennant (make-dr 10 'allons-y))
(define smith (make-dr 11 'geronimo))
(define capaldi (make-dr 12 'shuttity-up-up-up))

(defun make-drwho-universe()
  (define doctors
    (cons hartnell (cons troughton (cons pertwee
      (cons tom-baker (cons davison (cons colin-baker
      (cons mcgann (cons eccleston (cons tennant
      (cons smith (cons capaldi '()))))))))))))
  (lambda (msg)
    (if (eq msg 'current)
      (car doctors)
      (if (eq msg 'regenerate)
        ((lambda  ()
          (setq doctors (cdr doctors))
          '())) 
        'unknown_msg))))
