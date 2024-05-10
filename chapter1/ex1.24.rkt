#lang racket

;; Exercise 1.24 [★★] `(every? pred lst)` returns `#f` if any element of `lst` fails to satisfy
;; `pred`, and returns `#t` otherwise.

; every?: Pred × List -> Bool
; usage: returns `#f` if any element of `lst` fails to satisfy `pred`, and returns `#t` otherwise.
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (and (pred (car lst))
             (every? pred (cdr lst))))))

(module+ test
  (require rackunit)

  (check-equal? (every? number? '(a b c 3 e)) #f)
  (check-equal? (every? number? '(1 2 3 5 4)) #t))
