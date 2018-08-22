;-jakobskr, sigurson, eilifb
; https://uio.instructure.com/courses/949/files/21035/download?download_frd=1
;1
;a)
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;b)
(define foo 42)

(define (compareToX x y)
  (if (= x y)
      'same
      'different))

(define (list-me-up-fam bar baz)
  (list baz (list bar baz)))

;c)
(define (infix-eval list)
  ((cadr list) (car list) (caddr list)))

;d) application not a procedure, quote tegnet gjør at '/' tolkes som et symbol og ikke en prossedyre

(load "huffman.scm")

;2
;a)
(define (member? pred ele list)
  (if(null? list)
     #f
     (if(pred ele (car list))
        #t
        (member? pred ele (cdr list))
        )))

;b)
(define (decode-tail bits tree)
  (define (decode-2 bits current-branch output)
    (if (null? bits)
        output
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-2 (cdr bits) tree (append output (cons (symbol-leaf next-branch) '()))
                        )
              (decode-2 (cdr bits) next-branch output)))))
  (decode-2 bits tree '()))

;c) (ninjas fight ninjas by night)


;d)

(define (encode list tree)
  (define (encode-iter tree list output)
    (display "output is now")
    (display output)
    (newline)
    (if (null? list)
        output
        (encode-iter tree (cdr list) (append output (gb tree (car list))))
        ))
  (encode-iter tree (cdr list) (gb tree (car list)))
  )

(define (gb tree sym)
  (if (leaf? tree)
      '()
      (begin       
        (if (member sym (symbols (left-branch tree)))
            (cons 0 (gb (left-branch tree) sym))
            (cons 1 (gb (right-branch tree) sym)))
        )
      ))

;e)
(define (grow-huffman-tree liste)
  (define (grow-iter nodelist)
    (if (= 1(length nodelist))
        (car nodelist)
        (let ((x (car nodelist))
              (y (cadr nodelist)))
          (grow-iter (adjoin-set (list x y (append (get-value x) (get-value y)) (+ (weight x) (weight y))) (cddr nodelist)))))
    )
  (grow-iter (make-leaf-set liste)))

(define (get-value x)
  (if (leaf? x)
      (cons (cadr x) '())
      (caddr x)))
;f)
   
(define weeblist '((samurais 57) (ninjas 20) (fight 45) (night 12) (hide 3) (in 2) (ambush 2)(defeat 1) (the 5) (sword 4) (by 12) (assassin 1) (river 2) (forest 1) (wait 1) (poison 1)))

(define codebook (grow-huffman-tree weeblist))

#|
 '(ninjas fight) -> 01010 5 bits. gjennomsnittlig  ~3 bits. fixed length 8
 '(ninjas fight ninjas) -> 01010010 8. gjennomsnittlig ~3 bits per symbol. fixed length 12
 '(ninjas fight samurais) -> 0101011 7. gjennomsnittlig ~3 bits per symbol. fixed length 12
 '(samurai fight) -> 1110 4 bits. gjenomsittlig 2 bits per symbol. fixed length 8
 '(samurai fight ninjas) 1110010 7 bits. gjennomsnittlig ~3 bits. fixed length 12
 '(ninjas fight by night) 010100111000 12. gjennomsnittlig 4 bits per symbol. fixed length 16
|#

;g
(define (huffman-leaves tree)
  (if (leaf? tree)
      (cons (list (cadr tree) (caddr tree) ) '())
      (append (huffman-leaves (left-branch tree)) (huffman-leaves (right-branch tree)))
      ))

;h

(define (exp-iter huffcode tree)
  (if (leaf? tree)
      (* (weight tree)  (length huffcode))
      (+ (exp-iter (cons 0 huffcode) (car tree)) (exp-iter (cons 1 huffcode) (cadr tree)))))

(define (expected-code-length tree)
  (/ (exp-iter '() tree) (weight tree)))


