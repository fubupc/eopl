#lang racket

; Exercise 2.10 [★] Add to the environment interface a constructor `extend-env*`, and implement it
; using the a-list representation. This constructor takes a list of variables, a list of values of the
; same length, and an environment, and is speciﬁed by
; (extend-env* (var₁ ... varₖ) (val₁ ... valₖ) ⌈f⌉) = ⌈g⌉,
;   where g(var) = valᵢ, if var = valᵢ for some i such that 1 ≤ i ≤ k
;                = f(var), otherwise

; Import a-list representation
(require "ex2.5.rkt")

; extend-env*: Listof(Var) × Listof(SchemeVal) × Env → Env
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env*
         (cdr vars)
         (cdr vals)
         (extend-env (car vars) (car vals) env)))))

(apply-env (extend-env* '(a b c) '(1 2 3) (empty-env)) 'a)
