#lang racket

; Exercise 2.17 [★] Invent at least two other representations of the data type of lambda-calculus
; expressions and implement them.

; #1:
; Lc-exp = Var | (λ Var . Lc-exp) | (Lc-exp Lc-exp)
; Var = Sym
(module λ eopl
  (provide (all-defined-out))

  ; var-exp : Var → Lc-exp
  (define var-exp (lambda (var) var))

  ; lambda-exp : Var × Lc-exp → Lc-exp
  (define lambda-exp (lambda (var exp) (list 'λ var '\. exp)))

  ; app-exp : Lc-exp × Lc-exp → Lc-exp
  (define app-exp (lambda (e1 e2) (list e1 e2)))

  ; var-exp? : Lc-exp → Bool
  (define var-exp? (lambda (exp) (symbol? exp)))

  ; lambda-exp? : Lc-exp → Bool
  (define lambda-exp?
    (lambda (exp)
      (and (list? exp)
           (eqv? (car exp) 'λ))))

  ; app-exp? : Lc-exp → Bool
  (define app-exp?
    (lambda (exp)
      (and (list? exp)
           (not (eqv? (car exp) 'λ)))))

  ; var-exp->var : Lc-exp → Var
  (define var-exp->var (lambda (exp) exp))

  ; lambda-exp->bound-var : Lc-exp → Var
  (define lambda-exp->bound-var (lambda (exp) (cadr exp)))

  ; lambda-exp->body : Lc-exp → Lc-exp
  (define lambda-exp->body (lambda (exp) (cadddr exp)))

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
          (occurs-free? search-var (app-exp->rand exp))))))))


; tests
(require 'λ)
(occurs-free? 'x 'x)
(occurs-free? 'x 'y)
(occurs-free? 'x '(λ x \. (x y)))
(occurs-free? 'x '(λ y \. (x y)))
(occurs-free? 'x '((λ x \. x) (x y)))
(occurs-free? 'x '(λ y \. (λ z \. (x (y z)))))