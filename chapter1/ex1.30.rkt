#lang racket

;; Exercise 1.30 [★★] `(sort/predicate pred loi)` returns a list of elements sorted by the predicate.

; sort: Pred × Listof(Int) -> Listof(Int)
; usage: returns a list of elements sorted by the predicate.
(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (insert/predicate pred (car loi) (sort/predicate pred (cdr loi))))))

; insert: Pred × Int × Listof(Int) -> Listof(Int)
; usage: inserts an integer `i` at the correct position in `loi` sorted by the predicate.
(define insert/predicate
  (lambda (pred i loi)
    (if (null? loi)
        (list i)
        (let ((head (car loi)))
          (if (pred i head)
              (cons i loi)
              (cons head (insert/predicate pred i (cdr loi))))))))

(module+ test
  (require rackunit)

  (check-equal? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
  (check-equal? (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2)))
