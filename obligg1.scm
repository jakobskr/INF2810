;TO DO: oppgave 1, 2a, 3d og 3e.
#|
1a) 30
1b) Err: not a procedure. Kommer av at 5'ern er inni i sen egen parantes i.e (5)
1c) Err: not a procedure. Kommer av at + tegnet er mellom 4 og 2 og ikke før dem. riktig blir (+ 4 2)
1d) bar 21.
1e) 10
1f) 12

2a) 
|#
;2b)
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
         (if(< y 0)
         (plus (sub1 x) (add1 y))
          x))))

;3c)

|#
Den plussen som jeg skrev i 3b gir opphav til en iterativ prosess, siden vi ikke har ett basistilfelle men en counter som teller ned/opp, slik at metoden blir kalt
på y antall ganger men med forskjellige argumenter hver gang, og når prosessen er ferdig så printer den ut resultatet og backtracer ikke opp det rekursive treet. 

#|
;;den rekursive plus prossessen.
(define rec-plus
  (lambda (x y)
    (if (zero? y)
        x
        (add1 (rec-plus x (sub1 y))))))

;3d)
;; vi kan forenkle utrykket med blokkstrukur ved å fjerne b og n fra definisjonen til power-iter siden b og e er statiske gjennom prossessen.
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
      e
     (power-iter (+ 1 e))))
   (power-iter 1))


;3e)
; kan gjøre den til blokkstruktur, men kan ikke simplifiseres grunnet at a, b, og c
(define (fib n)
  (define (fib-iter a b count)
  (if (= count 0)
    b
   (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))