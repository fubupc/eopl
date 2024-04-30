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

  (check-true (occurs-free? 'x (var-exp 'x)))
  (check-false (occurs-free? 'x (var-exp 'y)))
  (check-false (occurs-free? 'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y)))))
  (check-true (occurs-free? 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y)))))
  (check-true (occurs-free? 'x (app-exp (lambda-exp 'x (var-exp 'x)) (app-exp (var-exp 'x) (var-exp 'y)))))
  (check-true (occurs-free? 'x (lambda-exp 'y (lambda-exp 'z (app-exp (var-exp'x) (app-exp (var-exp 'x) (var-exp 'y))))))))
