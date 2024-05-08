#lang racket

; Exercise 2.28 [★] Write an unparser that converts the abstract syntax of an `lc-exp` into a string
;  that matches the second grammar in this section (page 52).

; Grammer:
; Lc-exp ::= Identiﬁer
;            var-exp (var)
;        ::= (lambda (Identiﬁer) Lc-exp)
;            lambda-exp (bound-var body)
;        ::= (Lc-exp Lc-exp)
;            app-exp (rator rand)

(require eopl)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

; unparse-lc-exp : LcExp → SchemeVal
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) (~a "var-exp (" var ")"))
      (lambda-exp (bound-var body) (~a "lambda-exp (" bound-var " " (unparse-lc-exp body) ")"))
      (app-exp (rator rand) (~a "app-exp (" (unparse-lc-exp rator) " " (unparse-lc-exp rand) ")")))))


; Tests
(module+ test
  (require rackunit)

  (check-equal? (unparse-lc-exp (var-exp 'a)) "var-exp (a)")
  (check-equal? (unparse-lc-exp (app-exp (lambda-exp 'a (app-exp (var-exp 'a) (var-exp 'b))) (var-exp 'c))) "app-exp (lambda-exp (a app-exp (var-exp (a) var-exp (b))) var-exp (c))")
  )
