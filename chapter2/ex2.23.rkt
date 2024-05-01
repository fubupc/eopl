#lang racket

; Exercise 2.23 [★] The deﬁnition of `lc-exp` ignores the condition in deﬁnition 1.1.8 that says
; "Identiﬁer is any symbol other than lambda." Modify the deﬁnition of identifier? to capture this
; condition. As a hint, remember that any predicate can be used in `define-datatype`, even ones you
; deﬁne.

(require eopl)

(define identifier?
  (lambda (i)
    (and (symbol? i)
         (not (eqv? i 'lambda)))))

; Define data type of lambda calculus expression
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

; occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and
                   (not (eqv? search-var bound-var))
                   (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or
                (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))

(module+ test
  (require rackunit)

  (define e1 (var-exp 'x))
  (define e2 (var-exp 'y))
  (define e3 (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
  (define e4 (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))
  (define e5 (app-exp (lambda-exp 'x (var-exp 'x)) (app-exp (var-exp 'x) (var-exp 'y))) )
  (define e6 (lambda-exp 'y (lambda-exp 'z (app-exp (var-exp'x) (app-exp (var-exp 'y) (var-exp 'z))))))

  (check-true (occurs-free? 'x e1))
  (check-false (occurs-free? 'x e2))
  (check-false (occurs-free? 'x e3))
  (check-true (occurs-free? 'x e4))
  (check-true (occurs-free? 'x e5))
  (check-true (occurs-free? 'x e6)))
