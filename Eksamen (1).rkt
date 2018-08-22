(define (take n l)
  (if (or (null? l)
          (zero? n))
      '()
      (cons (car l)
            (take (- n 1) (cdr l)))))



(define (take! n l)
  (define (take-it! x g)
    (cond ((= x 1)  (begin (set-cdr! g '()) l))
          (else (take-it! (- x 1) (cdr g)))
          ))
  (take-it! n l))

(define foo '(1 2 3 4))

(define (drop n l)
  (if (null? l)
      '()
      (if (= 0 n)
          l
          (drop (- n 1) (cdr l)) )))

(define (split-every n l)
  (define (sub-split x r)
    (if (null? r) '()
        (cons (take x r) (sub-split x (drop x r)))))
  (sub-split n l))

(define (make-card value price n)
  (let ((val value)
        (p price)
        (x n)
        (coff 0))
    (lambda (msg)
      (define (buycoff cnt)
        (if (= x (+ 1 coff))
            (begin (set! coff 0) (if (= 0 (- cnt 1)) msg (buycoff (- 1 cnt))))
            (begin (set! val (- val (* p msg))) (set! coff (+ coff 1)) (if (= 0 (- cnt 1)) msg (buycoff (- 1 cnt))))))
      (cond ((null? msg) 0)
            ((equal? msg 'value) val)
            ((number? msg) (buycoff msg)))

      )))