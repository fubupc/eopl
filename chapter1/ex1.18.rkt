#lang racket

;; Exercise 1.18 [★] (swapper s1 s2 slist) returns a list the same as slist, but with all occurrences
;; of s1 replaced by s2 and all occurrences of s2 replaced by s1.

; swapper: Sym × Sym × S-list -> S-list
; usage: returns a list the same as slist, but with all occurrences of s1 replaced by s2 and all
;        occurrences of s2 replaced by s1.
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons
         (swapper-in-s-exp s1 s2 (car slist))
         (swapper s1 s2 (cdr slist))))))

; swapper-in-s-exp: Sym × Sym × S-exp -> S-exp
(define swapper-in-s-exp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (cond ((eqv? sexp s1) s2)
              ((eqv? sexp s2) s1)
              (else sexp))
        (swapper s1 s2 sexp))))


(module+ test
  (require rackunit)

  (check-equal? (swapper 'a 'd '(a b c d)) '(d b c a))
  (check-equal? (swapper 'a 'd '(a d () c d)) '(d a () c a))
  (check-equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y)))))
