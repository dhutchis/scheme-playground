#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define first
  (lambda (pair)
    (car pair)))
(define second
  (lambda (pair)
    (car (cdr pair))))
(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))
(define domset1
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      ((null? (car rel)) (domset (cdr rel)))
      ((or (null? (cdr (car rel))) (eq? (first (car rel)) (second (car rel))))
       (cons (first (car rel)) (domset (rember* (first (car rel)) (cdr rel)))))
      (else (cons (first (car rel)) (cons (second (car rel))
          (domset (rember* (second (car rel)) (rember* (first (car rel)) (cdr rel))))))))))
(define member*?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l)) (member*? a (cdr l))))
      (else (or (member*? a (car l)) (member*? a (cdr l)))))))
(define getuniques
  (lambda (uniql l)
    (cond
      ((null? l) uniql)
      ((atom? (car l)) 
       (cond
         ((member*? (car l) uniql) (getuniques uniql (cdr l)))
         (else (getuniques (cons (car l) uniql) (cdr l)))))
      (else (getuniques (getuniques uniql (car l)) (cdr l))))))
(define domset
  (lambda (rel)
    (getuniques (quote ()) rel)))
(domset '((a a) (a b) (b b) (e b) (c d) (3 3)) )