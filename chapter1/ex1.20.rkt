#lang racket

;; Exercise 1.20 [★] `(count-occurrences s slist)` returns the number of occurrences of `s` in
;; `slist`.

; count-occurrences: Sym × S-list -> Int
; usage: returns the number of occurrences of `s` in `slist`.
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-occurrences-in-s-exp s (car slist))
           (count-occurrences s (cdr slist))))))

; count-occurrences-in-s-exp: Sym × S-exp -> Int
; usage: returns the number of occurrences of s in sexp.
(define count-occurrences-in-s-exp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp) 1 0)
        (count-occurrences s sexp))))

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'w '((f x) y (((x z) x))))