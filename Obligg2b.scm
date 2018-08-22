;;jakobskr; eilifb; sigurson,
;;https://uio.instructure.com/courses/949/files/26092/download?wrap=1 ;for oblig2b_tester.scm

;1a)
(load "oblig2b_tester.scm")
(define count 42)

(define make-counter
  (lambda ()
    (let ((count 0))
      (lambda ()
        (begin
          (set! count (+ count 1))
          count)))))

(define c1 (make-counter))

(define c2 (make-counter))

;1b) TODO

;2a)

(define (make-stack liste)
  (define (pop)
    (if (not (null? liste))
        (set! liste (cdr liste))))
  (define (push inn)
    (if (null? inn)
        ""
        (begin
          (set! liste (append (cons (car inn) '()) liste))
          (push (cdr inn)))))
  (define (stack)
    liste)
  (define (dispatch . args)
    (cond
      ((eq? (car args) 'pop!) (pop))
      ((eq? (car args) 'stack) liste)
      ((eq? (car args) 'push!) (push (cdr args)))
      (else "uvu"))
    )
  dispatch)


(define ss1 (make-stack (list 'foo 'bar)))
(define ss2 (make-stack '()))

;2b)

(define (pop! stakk)
  (stakk 'pop!))

(define (stack stakk)
  (stakk 'stack))

(define (push! . args)
  (apply (car args) (cons 'push! (cdr args))))


;3a) TO DO
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

;3b) TO DO
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))


;3c)
(define (cycle? liste)
  (define (cycle-iter tortoise hare)
    (cond ((null? hare) #f)
          ((null? (cdr hare)) #f)
          ((eq? tortoise hare) #t)
          (else (cycle-iter (cdr tortoise) (cddr hare))))
    )
  (cond ((null? liste) #f)
        ((null? (cdr liste))#f)
        (else (cycle-iter (cdr liste) (cddr liste)))))

;3d)
;;fordi en liste er definert som enten '() eller et par hvor den cdr er en liste, og siden en sirkulær (altså en liste hvor vi kan kyddre tilbake til starten) liste ikke inneholder den tomme listen så kan ikke
;; (list?) rekursivt definere bar.

;3e) TO DO

;3f) TO DO