(define (cons1 x y)
  (lambda (message . new)
    (cond ((eq? message 'car) x)
          ((eq? message 'cdr) y)
          ((eq? message 'set-car) (set! x new))
          ((eq? message 'set-cdr) (set! y new)))))

(define (car1 p)
  (p 'car))

(define (cdr1 p)
  (p 'cdr))

(define (set-car p new)
  (p 'set-car new))

(define (set-cdr p new)
  (p 'set-cdr new))


(define foo (cons1 1 (cons1 2 3)))

(define (take n l)
  (define (take-iter l r n i)
    (cond ((null? l) (reverse r))
          ((= n 0) r)
          ((= n i) (reverse r))
          (else (take-iter (cdr l) (cons (car l) r) n (+ i 1)))))
  (take-iter l '() n 0))

(define make-queue
  (lambda (n)
    (let ((current 0)
         (queue '()))
      (lambda (x)
        (cond ((= current n) (begin (set! queue (cons x (take (- n 1) queue))) queue))
              (else (begin (set! queue (cons x queue)) (set! current (+ current 1)) queue)))))))