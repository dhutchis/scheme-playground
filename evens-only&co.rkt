#lang Scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote ()) 1 0))
      ((atom? (car l))
       (cond
         ((even? (car l)) (evens-only*&co (cdr l) 
                                          (lambda (loe p s)
                                            (col (cons (car l) loe) (* (car l) p) s))))
         (else (evens-only*&co (cdr l)
                               (lambda (loe p s)
                                 (col loe p (+ (car l) s)))))))
      (else (evens-only*&co (car l)
                            (lambda (loecar pcar scar)
                              (evens-only*&co (cdr l)
                                              (lambda (loecdr pcdr scdr)
                                                (col (cons loecar loecdr) (* pcar pcdr) (+ scar scdr))))))))))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                (lambda (loe p s)
                  (cons s (cons p loe))))
