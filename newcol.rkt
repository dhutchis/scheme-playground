#lang Scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define flatten
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? l) l)
      ((atom? (car l)) (cons (car l) (flatten (cdr l)))) ;reverse
      (else (cons (flatten (car l)) (flatten (cdr l))))
      )))

(define flatten2
  (lambda (l col) ;col takes 
    (cond
      ((null? l) (col (quote ())))
      ((atom? l) (col l))
      ((atom? (car l)) (flatten2 (cdr l) (lambda (loe)
                                           (col  (cons (car l) loe))
                                           )))
      (else (flatten2 (car l) (lambda (loecar)
                                (flatten2 (cdr l) (lambda (loecdr)
                                                    (col (cons loecar loecdr))
                                                    )))))
      )))

(define fl
  (lambda (loe)
    (cond
      ((null? loe) loe)
      ((atom? (car loe)) (cons (car loe) (fl (cdr loe))))
      (else (cons (fl (car loe)) (cdr loe))))))

(flatten2 '(5 (6)) 
          fl
          )