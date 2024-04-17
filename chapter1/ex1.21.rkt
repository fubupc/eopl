#lang racket

;; Exercise 1.21 [★★] `(product sos1 sos2)`, where `sos1` and `sos2` are each a list of symbols
;; without repetitions, returns a list of 2-lists that represents the Cartesian product of `sos1` and
;; `sos2`. The 2-lists may appear in any order.

; product: List × List -> Listof(2-lists)
; usage: returns a list of 2-lists that represents the Cartesian product of `sos1` and `sos2`.
;        The 2-lists may appear in any order.
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (product-helper (car sos1) sos2)
                (product (cdr sos1) sos2)))))

; product-helper: Sym × List -> Listof(2-lists)
; usage: returns a list of 2-lists that pairs `s` with each element of `sos`.
(define product-helper
  (lambda (s sos)
    (if (null? sos)
        '()
        (cons (list s (car sos))
              (product-helper s (cdr sos))))))

(product '(a b c) '(x y))