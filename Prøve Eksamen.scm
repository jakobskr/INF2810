;(x even? '(2 4 6 8)) -> #t

;3)

(define (avg . args)
  (define (iter sum length args)
    (if (null? args) (/ sum length)
        (iter (+ sum (car args)) (+ length 1) (cdr args))))
  (if (null? args) 0
      (iter 0 0 args)))


(define (transform-if test trans seq)
  (if (null? seq) '()
      (if (not (test (car seq)))
          (cons (car seq) (transform-if test trans (cdr seq)))
          (cons (trans (car seq)) (transform-if test trans (cdr seq))))))

(define foo '(1 2 3 4 5 6))

(define (transform-if! test trans seq)
  (define (iter rest)
    (if (null? rest)
        seq
        (if (test (car rest))
            (begin (set-car! rest (trans (car rest))) (iter (cdr rest)))
            (iter (cdr rest)))))
  (iter seq))

(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree))
         (append (fringe (car tree))
                 (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))


(define (same-fringe? fx fy pred)
  (define (iter x y)
    (cond ((and (null? x) (null? y)) #t)
          ((or (null? x) (null? y)) #f)
          ((pred (car x) (car y)) (iter (cdr x) (cdr y)))
          (else #f)
          ))
  (iter (fringe fx) (fringe fy)))

(define (fringe-stream tree)
  (cond ((null? tree) the-empty-stream)
        ((pair? (car tree))
         (stream-append (fringe-stream (car tree))
                        (fringe-stream (cdr tree))))
        (else (cons-stream (car tree) (fringe-stream (cdr tree))))))

(define (same-fringe-stream s1 s2 pred)
  (define (iter x y)
    (cond ((and (stream-null? x) (stream-null? y)) #t)
          ((or (stream-null? x) (stream-null? y)) #f)
          ((pred (stream-car x) (stream-car y)) (iter (stream-cdr x) (stream-cdr y)))
          (else #f)))
  (iter (fringe-stream s1) (fringe-stream s2)))

(define (monitor proc)
  (let ((count 0))
    (lambda args
      (let ((message (car args)))
    (cond ((eq? message 'zero) (set! count 0))
          ((eq? message 'count) count)
          ((eq? message 'reset) proc)
          (else (set! count (+ count 1)) (apply proc args)))))))

(define original fringe)