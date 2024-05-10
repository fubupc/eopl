#lang racket

; Exercise 1.13 [★★] In our example, we began by eliminating the Kleene star in the
; grammar for S-list. Write subst following the original grammar by using map.

(define (subst new old slist)
  (map (lambda (sexp) (subst-sexp new old sexp)) slist))

(define (subst-sexp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old)
          new
          sexp)
      (subst new old sexp)))


(module+ test
  (require rackunit)

  (check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d))))
