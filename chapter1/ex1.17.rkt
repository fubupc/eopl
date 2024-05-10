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

(module+ test
  (require rackunit)

  (check-equal? (down '(1 2 3)) '((1) (2) (3)))
  (check-equal? (down '((a) (fine) (idea)))
                '(((a)) ((fine)) ((idea))))
  (check-equal? (down '(a (more (complicated)) object))
                '((a) ((more (complicated))) (object))))
