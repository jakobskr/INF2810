(define (take n l)
  (define (take-iter l r n i)
    (cond ((null? l) (reverse r))
          ((= n 0) r)
          ((= n i) (reverse r))
          (else (take-iter (cdr l) (cons (car l) r) n (+ i 1)))))
  (take-iter l '() n 0))


(define (take! n l)
  (define (take-it! x g)
  (cond ((= x 1)  (begin (set-cdr! g '()) l))
        (else (take-it! (- x 1) (cdr g)))
    ))
  (take-it! n l))

(define foo '(1 2 3 4))