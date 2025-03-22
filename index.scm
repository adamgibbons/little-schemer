#lang racket
(require test-engine/racket-tests)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define lat?
  (lambda (x)
    (cond
    ((null? x) #t)
    ((atom? (car x)) (lat? (cdr x)) #t)
    (else #f))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
                  (rember a
                          (cdr lat)))))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else
       (cons (car (car l))
             (firsts (cdr l)))))))

(check-expect (lat? '()) #t)
(check-expect (lat? '("hi")) #t)
(check-expect (lat? '('())) #f)
(check-expect (rember 2 '(1 2 3 4)) '(1 3 4))
(check-expect (firsts '((1 2)(3 4))) '(1 3))
(test)