#lang racket

;; Exercise 1.29 [★★] `(sort loi)` returns a list of the elements of `loi` in ascending order.

; sort: Listof(Int) -> Listof(Int)
; usage: returns a list of the elements of `loi` in ascending order.
(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (insert (car loi) (sort (cdr loi))))))

; insert: Int × Listof(Int) -> Listof(Int)
; usage: inserts an integer `i` at the correct position in `loi` sorted in ascending order.
(define insert
  (lambda (i loi)
    (if (null? loi)
        (list i)
        (let ((head (car loi)))
          (if (<= i head)
              (cons i loi)
              (cons head (insert i (cdr loi))))))))

(sort '(8 2 5 2 3))
