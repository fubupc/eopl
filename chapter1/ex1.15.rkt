#lang racket

;; Exercise 1.15 [★] (duple n x) returns a list containing n copies of x.

; duple: Int × Any -> Listof(Any)
; usage: returns a list containing n copies of x.
(define duple
  (lambda (n x)
    (if (= n 0)
        '()
        (cons x (duple (- n 1) x)))))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))