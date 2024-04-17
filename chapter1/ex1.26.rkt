#lang racket

;; Exercise 1.26 [★★] `(up lst)` removes a pair of parentheses from each top-level element of `lst`.
;; If a top-level element is not a list, it is included in the result, as is. The value of
;; `(up (down lst))` is equivalent to `lst`, but `(down (up lst))` is not necessarily `lst`.
;; (See exercise 1.17.)

; up: List -> List
; usage: removes a pair of parentheses from each top-level element of `lst`.
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((head (car lst))
              (tail (up (cdr lst))))
          (if (list? head)
              (append head tail)
              (cons head tail))))))

(up '((1 2) (3 4)))
(up '((x (y)) z))