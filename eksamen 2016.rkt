

;3a)
(define (take n l)
  (define (iter i in out)
    (cond ((null? in) (reverse out))
          ((zero? i) (reverse out))
          (else (iter (- i 1) (cdr in) (cons (car in) out)))))
  (iter n l '()))

(define foo '(a b c d e))

(define (take n l)
  (cond ((null? l) '())
        ((zero? n) '())
        (else (cons (car l) (take (- n 1) (cdr l))))))

;3b)

(define (take! n l)
  (define (iter rest i)
    (cond ((null? rest) l)
          ((zero? i) (set-cdr! rest '()) l)
          (else (iter (cdr rest) (- i 1)))))
  (if (zero? n)
      '()
      (iter l (- n 1))))

;3d)

(define (drop n l)
  (cond ((null? l) '())
        ((zero? n) l)
        (else (drop (- n 1) (cdr l)))))

(define (split-every n l)
  (if (null? l) '()
      (cons (take n l) (split-every n (drop n l)))))


(define (make-card v p n)
  (let ((val v)
        (price p)
        (count 0)
        (free n))
    (lambda (message)
      (display message)
      (newline)
      (cond ((eq? message 'value) val)
            ((eq? message 'dep) (lambda (x) (set! val (+ val x))))
            ((eq? message 'count) count)
            ((eq? message 'order)
             (lambda (x)
               (let ((grat (quotient (+ count x) free))
                     (to-pay (* price (- x (quotient (+ count x) free)))))
                 (cond ((or (= val to-pay) (> val to-pay))
                        (set! val (- val to-pay))
                        (set! count (remainder (+ count x) free))
                        (- x grat))
                       (else 0)))))))))

(define (card-value card)
  (card 'value))

(define (card-count card)
  (card 'count))

(define (card-deposit c x)
  ((c 'dep) x))

(define (card-order c x)
  ((c 'order) x))

(define oe (make-card 100 25 5))

(define (func pred list)
  (cond ((null? list) '())
        ((pred (car list))
         (cons (car list)
               (func pred (cdr list))))
        (else (func pred (cdr list)))))
#|
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream)
               (stream-cons (stream-car stream)
                            (stream-filter pred (stream-cdr stream))))
         (else (stream-filter pred (stream-cdr stream))))))
|#


(define (stream-range i)
  (stream-cons i (stream-range (+ i 1))))

(define (eratosthenes)
  (define (iter stream)
        (let* ((first (stream-car stream))
               (predicate (lambda (i) (not (divisible? i first)))))
          (stream-cons first (iter (stream-filter predicate (stream-cdr stream))))))
  (iter (stream-range 2)))

(define make-counter
  (let ((global 0))
  (lambda ()
    (let ((local 0))
      (lambda ()
        (set! global (+ global 1))
        (set! local (+ local 1))
        (list global local))))))

(define (reduce fn list init)
  (if (null? list) init
      (fn (car list)
          (reduce fn (cdr list) init))))

(reduce (lambda (x y) (cons (* x x) y))  '(1 2 3 4) '())

(define (filter1 pred list)
    (reduce (lambda (x y) 
                (if (pred x) (cons x y)
                       y))
            list '()))


(define (map1 pros list)
    (reduce (lambda (x y) (cons (pros x) y)) list))

                    