#lang racket

; Exercise 2.29 [★] Where a Kleene star or plus (page 7) is used in concrete syntax, it is most
; convenient to use a `list` of associated subtrees when constructing an abstract syntax tree. For
; example, if the grammar for lambda-calculus expressions had been
;
; Lc-exp ::= Identiﬁer
;            var-exp (var)
;        ::= (lambda ({Identiﬁer}∗ ) Lc-exp)
;            lambda-exp (bound-vars body)
;        ::= (Lc-exp {Lc-exp}∗ )
;            app-exp (rator rands)
;
; then the predicate for the bound-vars ﬁeld could be `(list-of identifier?)`, and the predicate for
; the rands ﬁeld could be `(list-of lc-exp?)`. Write a define-datatype and a parser for this grammar
; that works in this way.

(require eopl)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-vars (list-of symbol?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))

; parse-expression : SchemeVal → LcExp
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp (cadr datum) (parse-expression (caddr datum)))
           (app-exp (parse-expression (car datum)) (map parse-expression (cdr datum)))))
      (else (eopl:error "invalid lambda-calculus expression: ~a" datum)))))


; Tests
(module+ test
  (require rackunit)

  (check-equal? (parse-expression '(lambda (x y) (z x y))) (lambda-exp '(x y) (app-exp (var-exp 'z) (list (var-exp 'x) (var-exp 'y))))))
