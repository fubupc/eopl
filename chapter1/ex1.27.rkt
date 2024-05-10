#lang racket

;; Exercise 1.27 [★★] `(flatten slist)` returns a list of the symbols contained in `slist` in the
;; order in which they occur when `slist` is printed. Intuitively, `flatten` removes all the inner
;; parentheses from its argument.

; flatten: S-list -> Listof(Sym)
; usage: returns a list of the symbols contained in `slist`.
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (append (flatten-s-exp (car slist))
                (flatten (cdr slist))))))

; flatten-s-exp: S-exp -> Listof(Sym)
; usage: returns a list (even when `sexp` is a symbol) of the symbols contained in `sexp`.
(define flatten-s-exp
  (lambda (sexp)
    (if (symbol? sexp)
        (list sexp)
        (flatten sexp))))

(module+ test
  (require rackunit)

  (check-equal? (flatten '(a b c)) '(a b c))
  (check-equal? (flatten '((a) () (b ()) () (c))) '(a b c))
  (check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e))
  (check-equal? (flatten '(a b (() (c)))) '(a b c)))
