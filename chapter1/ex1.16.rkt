#lang racket

;; Exercise 1.16 [★] (invert lst), where lst is a list of 2-lists (lists of length two), returns a
;; list with each 2-list reversed.

; invert: Listof(2-list) -> List(2-list)
; usage: returns a list with each 2-list reversed
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((first (car lst)))
          (cons
           (list (cadr first) (car first))
           (invert (cdr lst)))))))


(invert '((a 1) (a 2) (1 b) (2 b)))