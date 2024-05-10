#lang racket

; Exercise 1.9 [★★] Deﬁne `remove`, which is like `remove-first`, except that it removes all
; occurrences of a given symbol from a list of symbols, not just the ﬁrst.
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? s (car los))
            (remove s (cdr los))
            (cons (car los) (remove s (cdr los)))))))

(module+ test
  (require rackunit)

  (check-equal? (remove 'a '(b a c a d)) '(b c d)))
