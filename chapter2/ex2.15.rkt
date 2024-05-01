#lang racket

(require eopl)

; Exercise 2.15 [★] Implement the lambda-calculus expression interface for the representation
; speciﬁed by the grammar above.

; Lc-exp = Var | (lambda (Var) Lc-exp) | (Lc-exp Lc-exp)
; Var = Sym

; var-exp : Var → Lc-exp
(define var-exp (lambda (var) var))

; lambda-exp : Var × Lc-exp → Lc-exp
(define lambda-exp (lambda (var exp) (list 'lambda (list var) exp)))

; app-exp : Lc-exp × Lc-exp → Lc-exp
(define app-exp (lambda (e1 e2) (list e1 e2)))

; var-exp? : Lc-exp → Bool
(define var-exp? (lambda (exp) (symbol? exp)))

; lambda-exp? : Lc-exp → Bool
(define lambda-exp?
  (lambda (exp)
    (and (list? exp)
         (eqv? (car exp) 'lambda))))

; app-exp? : Lc-exp → Bool
(define app-exp?
  (lambda (exp)
    (and (list? exp)
         (not (eqv? (car exp) 'lambda)))))

; var-exp->var : Lc-exp → Var
(define var-exp->var (lambda (exp) exp))

; lambda-exp->bound-var : Lc-exp → Var
(define lambda-exp->bound-var (lambda (exp) (caadr exp)))

; lambda-exp->body : Lc-exp → Lc-exp
(define lambda-exp->body (lambda (exp) (caddr exp)))

; app-exp->rator : Lc-exp → Lc-exp
(define app-exp->rator (lambda (exp) (car exp)))

; app-exp->rand : Lc-exp → Lc-exp
(define app-exp->rand (lambda (exp) (cadr exp)))

; occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))


; Tests
(module+ test
  (require rackunit)

  (define e1 (var-exp 'x))
  (define e2 (var-exp 'y))
  (define e3 (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
  (define e4 (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y))))
  (define e5 (app-exp (lambda-exp 'x (var-exp 'x)) (app-exp (var-exp 'x) (var-exp 'y))) )
  (define e6 (lambda-exp 'y (lambda-exp 'z (app-exp (var-exp'x) (app-exp (var-exp 'y) (var-exp 'z))))))

  ; Check the interface
  (check-equal? e1 'x)
  (check-equal? e2 'y)
  (check-equal? e3 '(lambda (x) (x y)))
  (check-equal? e4 '(lambda (y) (x y)))
  (check-equal? e5 '((lambda (x) x) (x y)))
  (check-equal? e6 '(lambda (y) (lambda (z) (x (y z)))))

  ; Check functions based on the interface
  (check-true (occurs-free? 'x e1))
  (check-false (occurs-free? 'x e2))
  (check-false (occurs-free? 'x e3))
  (check-true (occurs-free? 'x e4))
  (check-true (occurs-free? 'x e5))
  (check-true (occurs-free? 'x e6)))
