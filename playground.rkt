#lang racket

; occurs-free?: Sym * LcExp -> Bool
; usage: returns #t if var occurs free in exp, otherwise returns #f.
(define (occurs-free? var exp)
  (cond
    ((symbol? exp) (eqv? var exp))
    ((eqv? 'lambda (car exp))
     (and
      (not (eqv? var (car (car (cdr exp)))))
      (occurs-free? var (car (cdr (cdr exp))))))
    (else
     (or
      (occurs-free? var (car exp))
      (occurs-free? var (car (cdr exp)))
      ))))

(occurs-free? 'x 'x)
(occurs-free? 'x 'y)
(occurs-free? 'x '(lambda (x) (x y)))
(occurs-free? 'x '(lambda (y) (x y)))
(occurs-free? 'x '((lambda (x) x) (x y)))
(occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))


; subst: Sym * Sym * S-list -> S-list
; usage: returns a new S-list with all occurrences of old replaced by new.
(define (subst new old slist)
  (if
   (null? slist) '()
   (cons
    (subst-sexp new old (car slist))
    (subst new old (cdr slist)))))

; s-exp version of subst
(define (subst-sexp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old)
          new
          sexp)
      (subst new old sexp)))

(subst 'a 'b '((b c) (b () d)))
