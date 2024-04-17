#lang racket

;; Exercise 1.25 [★★] `(exists? pred lst)` returns `#t` if any element of `lst` satisﬁes `pred`, and
;; returns `#f` otherwise.

; exists?: Pred × List -> Bool
; usage: returns `#t` if any element of `lst` satisﬁes to satisfy `pred`, and returns `#f` otherwise.
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst))
            (exists? pred (cdr lst))))))

(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))
