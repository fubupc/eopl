#lang racket

;; Exercise 1.19 [★★] (list-set lst n x) returns a list like lst, except that the n-th element, using
;; zero-based indexing, is x.

; list-set: List × Int × Any -> List
; usage: returns a list like lst, except that the n-th element, using zero-based indexing, is x.
(define list-set
  (lambda (lst n x)
    (if (= n 0)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)