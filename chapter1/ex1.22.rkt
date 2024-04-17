#lang racket

;; Exercise 1.22 [★★] `(filter-in pred lst)` returns the list of those elements in `lst` that satisfy
;; the predicate `pred`.

; filter-in: Pred × List -> List
; usage: returns the list of those elements in `lst` that satisfy the predicate `pred`.
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (let ((tail (filter-in pred (cdr lst))))
          (if (pred (car lst))
              (cons (car lst) tail)
              tail)))))

(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))