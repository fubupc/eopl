#lang racket

;; Exercise 1.17 [★] (down lst) wraps parentheses around each top-level element of lst.

; down: List -> List
; usage: wraps parentheses around each top-level element of lst.
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (list (car lst))
         (down (cdr lst))))))

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))
