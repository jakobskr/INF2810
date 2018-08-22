;Jakobskr, Eilifb, Sigurson
(load "prekode3a.scm")

;1a og 1b)

(define (memoize f)
  (let ((table (make-table)))
    (let ((mem-proc (lambda x
                      (let ((prev-comp-result (lookup x table)))
                        (cond ((procedure? prev-comp-result) prev-comp-result)
                              (else (or prev-comp-result
                                        (let ((result (apply f x)))
                                          (insert! x result table)
                                          result))))))))
      (insert! (list mem-proc) f table)
      mem-proc)))
            
(define (mem hva proc)
  (let ((pro proc))
    (cond ((eq? hva 'memoize) (memoize proc))
          ((eq? hva 'unmemoize) (proc proc))
          (else "hva behager"))))

;c)
;Hvis vi bare bruker (define fib (memoize 'memoize fib)) så vil da bare lambda utrykket fib peker på bli memoisert,
;og ikke lambda utrykkene i (memoize) prosedyren. Og da vil den memoiserte fib kalle den originale fib i stedenfor den nye memoiserte fib.
;Men når vi bruker (set! fib (memoize 'memoize fib)) da vil vi også sette fib pekerene inni fib til også å peke på den memoizerte versjonen av fib.


;d)

(define greet
  (lambda args
    (string-append "I said good " (get-values args 'time "day") " " (get-values args 'title "friend"))))
    

(define get-values
  (lambda (list symbol default)
    (if (member symbol list)
        (cadr (member symbol list))
        default)))
  

;2a)

(define (list-to-stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (list-to-stream (cdr list)))))

(define (stream-to-list strøm . n)
  (define (stream-iter stream x)
    (if (eq? the-empty-stream stream)
        '()
        (if (= 0 x)
            '()
            (cons (stream-car stream) (stream-iter (stream-cdr stream) (- x 1))))))
  (if (null? n)
      (stream-iter strøm 15)
      (stream-iter strøm n)))

;2b)

(define (square x)
  (* x x))

(define (stream-map proc . argstream)
  (if (finished argstream)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstream))
       (apply stream-map (cons proc (map stream-cdr argstream))))))

(define (finished list)
  (if (null? list)
      #t
      (if (null? (car list))
          #t
          (if (null? (cdr list))
              #f
              (finished (cdr list))))))
;2c)
;
;Den beholder ikke listens/strømmens originale bevaringskriterium
;(remove-duplicates-str (list-to-stream '(4 1 2 3 3 4 4 5))) -> 1 2 3 4 5 og ikke 4 1 2 3 5 som en kunne forventet
; og hvis vi sender inn en uendelig strøm så vil aldri memq returnere noe siden basistilfelle dens er 'the-empty stream, og en uendelig liste vil aldri ha den konstanten
;derfor vil memq bare rekursere uendelig.

;2d)
(define (remove-duplicates-str lst)
  (cond ((null? lst) '())
        ((not (memqs (stream-car lst) (stream-cdr lst)))
         (cons-stream (stream-car lst) (remove-duplicates-str (stream-cdr lst))))
        (else (remove-duplicates-str (stream-cdr lst)))))

(define (memqs item x)
  (cond ((null? x) #f)
        ((eq? item (stream-car x)) x)
        (else (memqs item (stream-cdr x)))))

(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter (lambda (x) (not (equal? x (stream-car stream)))) (remove-duplicates (stream-cdr stream)))))))

;2e)
#|
(define x
(stream-map show
(stream-interval 0 10))) -> 0
Skriver ut null siden det er det første elementet, og siden vi har brukt stream-map med prosedyren show, så vil
da show bli kalt hver gang vi ønsker å hente ut et nytt element fra strømmen.
(stream-ref x 5) -> "1" "2" "3" "4" "5" 5
her blir 1-5 skrevet ut i repl. siden vi ikke har hentet dem ut før, så er de ikke lagret i cachen.
 og hele uttrykket blir evaluert til 5 siden 5 er på den 5. indexen i strømmen.
(stream-ref x 5) -> "6" "7" 7
her blir bare 6 og 7 skrevet ut siden vi 0-5 allerede er lagret i cachen, mens vi må kalle på show metoden som er
lagret på dem 6 og 7 indexen. og hele utrykket evalures til 7 siden 7 er på den 7. indexen i strømmen.
|#


;f)
(define (mul-streams . argstream)
  (display "mul-stream arg: ")
  (display argstream)
  (newline)
  (apply stream-map * argstream))

;g)

(define factorials
  (cons-stream (stream-car (mul-streams nats nats)) (mul-streams nats factorials)))

