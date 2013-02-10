#lang scheme
(define (fold fun iv lst) 
  (cond
    ((null? lst) iv)
    (else (fun (car lst) (fold fun iv (cdr lst))))
    ))

(define (square x) (* x x))
(define (my-sum-squares lst) (fold + 0 (map square lst)))
; alternative:
;(define (my-sum-squares lst) (apply + (map square lst)))
(my-sum-squares '(1 2 3 4 5)) ; == 55
