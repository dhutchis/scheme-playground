#lang racket
; pick a random element from the list l
; tries to pick each element in the list in succession, increasing the probability of success for each failure
;   this is equivalent to picking each element in l with the same probability
; Example with 4 elements:
; pick #1 with prob. 1/4
; if failure, pick #2 with prob. (1/4)/(1-1/4) = 1/3; Prob. of picking #2 is (3/4)*(1/3) = 1/4
; if failure, pick #3 with prob. (1/3)/(1-1/3) = 1/2; Prob. of picking #3 is (3/4)*(2/3)*(1/2) = 1/4
; if failure, pick #4 with prob. (1/2)/(1-1/2) = 1; Prob. of picking #4 is (3/4)*(2/3)*(1/2)*1 = 1/4
(define (uniform-pick l)
  (let loop ((lp (/ 1 (length l))) (l l))
    (if (<= (random) lp)
        (car l)
        (loop (/ lp (- 1 lp)) (cdr l))
        )))

; a list of functions
(define lodfs (list (lambda () (display "a") (newline)) 
                    (lambda () (display "b") (newline)) 
                    (lambda () (display "c") (newline))) )
;((uniform-pick lodfs)) ;test executing a random function from lodfs

; an example long-running primary-task to use with multitask below
(define (long-task other-task)
  (let loop ((n 3))
    (display "performing task ") (display n) (newline)
    (set! other-task (call/cc other-task))
    (if (> n 0)
        (loop (- n 1))
        'done
        )))

; Multitask: a non-preemptive priority scheduler.
;   primary-task: A function that takes 1 argument, a pointer back to this via call/cc
;   lof: A list of secondary functions, each taking 0 arguments.
; Runs primary-task until it cedes control explicity via by calling its first argument-the call/cc back to this-
;   with a call/cc back to the primary-task at the point it cedes control.  Effectively, primary-task should 
;   do some work, save its context and send a pointer to it back to here.  Then multitask will call a secondary function
;   at random from lof.  When lof completes, multitask restores the context of primary-task so that it can do more work, 
;   until primary-task has had enough and wishes to cede control back to multitask again.
; Note that if primary-task returns without calling cc, primary-task must have finished!  
;   It passes its return value to done-cond, which breaks the infinite loop and become the return value of multitask.
(define (multitask primary-task lof)
  ;(let loop ((secondary-task (uniform-pick lof)))
  (call/cc (lambda (done-cond)
             (let loop ()
               (set! primary-task (call/cc (lambda (cc) (done-cond (primary-task cc)) )))
               ((uniform-pick lof))
               (loop)
               ))))

(multitask long-task lodfs)