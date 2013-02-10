#lang racket

;;; CPS factorial
(define (kfact n k)
  (if (= n 1) 
      (k 1)
      (kfact (- n 1) (lambda (x) (k (* n x))))))

(kfact 4 (lambda (x) (+ x 1)))

(let countdown ((i 3)) ; local variable binding
  (if (= i 0) 'liftoff
      (begin
        (display i)
        (newline)
        (countdown (- i 1)))))

(* 3 (call/cc (lambda (k)  (+ 1 (k 2)))))

(define (display2 x y) (display x) (display y))
(define (display3 x y z) (display2 x y) (display z))
;(display2 "hey" "buddy")

(display3 "outer " (call/cc (lambda (k)
                              (display "indisp1 ") (display "innner ") (display "indisp2 ") "retval ")
                            ) "outer2 ")            ;^ change this to k
(newline)
(let ((a 1))
  (display a)
  (set! a 2)
  (display a)
  )(newline)

   (let* ((yin
         ((lambda (cc) (display "@") cc) (call-with-current-continuation (lambda (c) c))))
       (yang
         ((lambda (cc) (display "*") cc) (call-with-current-continuation (lambda (c) c)))))
    (yin yang))
