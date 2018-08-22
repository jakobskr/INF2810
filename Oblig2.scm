#|
1a)
(list 47 11)  -> [|][-]->11
                  v  
                  47
1b)
(list 47)  -> [|][/]
               v  
               47 
1c)
(list 47 11)  -> [|][-]-> [|][/]
                  v        v
                  47       11
1d)
'(47 (11 12)) -> [|][-]-> [|][/]
                  v        v
                  47      [|][-]->[|][/]
                           v       v
                           11      12
1e)
foo -> [|][---------------------]-->[|][/]
        v                            v
       [|][-]->[|][-]->[|][/]       [|][-]->[|][-]->[|][/]
        v       v       v            v       v       v
        1       2       3            1       2       3
|#

(define bar 'bar)
;1f)
(car (cdr '(0 42 #t bar)))

;1g)
(car (cdr (car '((0 42) (#t bar)))))

;1h)
(car (car (cdr '((0) (42 #t) (bar)))))

;1i)
(cons (cons 0 (cons 42 '())) (cons(cons #t (cons bar '())) '()))
(list (list 0 42) (list #t bar))

;2a)
(define (length2 list)
  (define (iter-length2 count list)
    (if (null? list)
        count
        (iter-length2 (+ count 1) (cdr list))))
    (iter-length2 0 list))

(length2 '(1 2 3 4 5))

;2b) 
(define (reduce-reverse proc init items)
  (define (red-iter in out)
     (if (null? in)
        out
        (red-iter (cdr in) (proc (car in) out))))
  (red-iter items init))

(reduce-reverse cons '() '(1 2 3 4))
;denne procedyren gir opphav til en hale rekursivprossess

;2c)
(define (all? pred items)
  (cond ((null? items) #t)
        ((pred (car items)) (all? pred (cdr items)))
          (else #f)))

(all? (lambda (x) (> 10 x)) '(1 3 5 6 9))
(all? (lambda (x) (> 10 x)) '(1 3 5 6 50))

;2d)
(define (nth at items)
  (define (iter-nth index items)
    (if (= index at)
     (car items)
      (iter-nth (+ 1 index) (cdr items))))
  (iter-nth 0 items))

;2e)
(define (where x items)
 (define (iter-where count items)
   (cond ((null? items) #f)
         ((= x (car items)) count)
         (else (iter-where (+ count 1) (cdr items)))))
  (iter-where 0 items))

;2f)
(define (map2 proc y x)
 (if (OR (null? y) (null? x))
    '()
    (cons (proc (car y) (car x))
           (map2 proc (cdr y) (cdr x)))))
;2g)
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))

;2h)
(define (both? pred)
  (lambda (x y)
    (if (or (pred x) (pred y))
        #t
        #f)))

;2i)
(define (self proc)
  (lambda (x) (proc x x)))
