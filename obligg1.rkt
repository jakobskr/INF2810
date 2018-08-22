;;TO DO: oppgave 1, 2a, 3d og 3e.
#|

|#
;;2a)

;;2b)
(define signCond
  (lambda (x)
    (cond ((= x 0)  0)
          ((> x 0)  1)
          ((< x 0) -1))))
          
(define signIf
  (lambda (x)
    (if (> x 0)
        1
        (if(= x 0)
        0
        -1))))

;;2c)
(define sign
  (lambda (x)
    (or
     (or
      (and (> x 0) 1) ;;sjekker om den er storre enn 0
      (and (< x 0) -1) ;; sjekker om den er mindre enn 0
     )
    0))) 
        
;;3a)
(define sub1
 (lambda (x)
  (- x 1)))

(define add1
 (lambda (x)
  (+ x 1)))

(add1 (sub1 0))


;;3b)

(define plus
  (lambda (x y)
    (if (> y 0)
        (plus (add1 x) (sub1 y))
         x)))
;;en rask test
(if ( = 52 (plus 49 3))
    "plus: success"
    "plus: failure")


