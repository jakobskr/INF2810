(define (liste? items)
  (or (null? items)
      (and (pair? items)
           (liste? (cdr items)))))

(define (deep-map proc items)
  (if (null? items) '()
      (if (pair? items)
          (cons (deep-map proc (car items)) (deep-map proc (cdr items)))
          (proc items))))

(define nested '(((1) 2) 3 (4 (5 6))))


;2a)

(define foo '(a b b a))

(define (replaces x y items)
  (define (replaces-iter old new)
    (if (null? old) (reverse new)
        (if (equal? (car old) x)
            (replaces-iter (cdr old) (cons y new))
            (replaces-iter (cdr old) (cons (car items) new)))))
  (replaces-iter items '()))

(replaces 'a 'c foo)


;2b)
(define (replace x y items)
  (if (null? items) '()
      (if (equal? (car items) x)
          (cons y (replace x y (cdr items)))
          (cons (car items) (replace x y (cdr items))))))


;2c)

(define (high-rep x y items)
  (map (lambda (c) (if (eq? c x) y
                       c)) items))

;2)
(define (replace! x y seq)
  (define (iter items)
    (if (null? items) seq
        (if (equal? (car items) x) (begin (set-car! items y) (iter (cdr items)))
    (iter (cdr items)))))
  (iter seq))