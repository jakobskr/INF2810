(load "evaluator.scm")



#|
1a)

input:
(foo 2 square)
retur:
0, siden 2 = 0 så blir 0 returnert

input:
(foo 4 square)
retur:
siden 4 != 0, så blir da square kalt med 4 som argument

input:
(cond ((= cond 2) 0)
    (else (else 4)))
retur:
2, siden 3 ikke er lik 2 så går den til else i utrykket, og else er her definert som (\ x 2), og x blir da 4, altså 4 \ 2 = 2

grunnen til at vi får det vi får er at interpretatoren tolket utrykkene basert på hvordan utrykkene er satt opp.
Hvis man har noe som heter cond men som ikke er satt opp som et cond utrykk, så vil da scheme se etter noe annet som heter
cond og evaluere utrykket med den cond verdien som gir "mening" for interpretatoren basert på utrykket.
|#
;2a)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        (list '1+ (lambda (x) (+ x 1)))
        (list '1- (lambda (x) (- x 1)))
        (list 'append append)
        ;;      La til 1+ og 1- primitivene
        ;;      her kan vi legge til flere primitiver.
        ))
;2b)
(set! the-global-environment (setup-environment))

(define install-primitive!
  (lambda (name func)
    (set! the-global-environment (extend-environment (list name) (list (list 'primitive func)) the-global-environment))))

;3a)

;;la til and? , or? , let? og letu?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t)
        ((or? exp) #t)
        ((let? exp) #t)
        ((letu? exp) #t)
        (else #f)))

;;la til and? , or? , let? og letu?
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((let? exp) (eval-let exp env))
        ((letu? exp) (eval-letu (cdr exp) env))

        ;;add more here
        
        ))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (let? exp) (tagged-list? exp 'let))
(define (letu? exp) (tagged-list? exp 'letu))

(define eval-and
  (lambda (exp env)
    (cond ((null? (cdr exp)) #t)
          ((true? (mc-eval (cadr exp) env)) (eval-and (cdr exp) env))
          (else #f))))

(define eval-or
  (lambda (exp env)
    (cond ((null? (cdr exp)) #f)
          ((true? (mc-eval (cadr exp) env)) #t)
          (else (eval-or (cdr exp) env)))))


;3b)


(define eval-if
  (lambda (exp env)
    (if (true? (mc-eval (cadr exp) env))(cadddr exp)
        (eval-if-help (cddddr exp) env))))

(define eval-if-help
  (lambda (exp env)
    (if (eq? (car exp) 'else) (cadr exp)
        (if (eq? (car exp) 'elseif)
            (if (true? (mc-eval (cadr exp) env))(cadddr exp)
                (eval-if-help (cddddr exp) env))))))

;3c)

(define eval-let
  (lambda (exp env)
    (mc-eval (cons (list 'lambda (get-vars (cadr exp)) (caddr exp)) (get-exp (cadr exp))) env)))
    
(set! the-global-environment (setup-environment))

(define get-vars
  (lambda (list)
    (if (null? list)
        '()
        (cons (caar list) (get-vars (cdr list))))))

(define get-exp
  (lambda (list)
    (if (null? list)
        '()
        (cons (cadar list) (get-exp (cdr list))))))


;3d)
;(let var1 = exp1 and var2 = exp2 in body)

(define eval-letu
  (lambda (exp env)
    (if (eq?  (cadddr exp) 'in)
        (mc-eval (cons (list 'lambda (cons (car exp) '()) (cddddr exp)) (cons (caddr exp) '())) env))))

;(letu x = 2 in (lambda (x) (+ x 1)))

(read-eval-print-loop)