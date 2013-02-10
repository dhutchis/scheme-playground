#lang Scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define member*?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (eq? a (car l)) (member*? a (cdr l))))
      ((null? (car l)) (member*? a (cdr l)))
      (else (or (member*? a (car l)) (member*? a (cdr l)))) )))
(define intersectall
  (lambda (l-set)
    (cond
      ((null? l-set) (quote ()))
      ((null? (car l-set)) (intersectall (cdr l-set)))
      ((member*? (car (car l-set)) (cons (cdr (car l-set)) (cdr l-set)))
       (intersectall (cons (cdr (car l-set)) (cdr l-set))))
      (else (cons (car (car l-set)) (intersectall (cons (cdr (car l-set)) (cdr l-set))))) )))
(intersectall '((a b c) (c a d e) (e f g h a b)) )
NOPE -- this is wrong