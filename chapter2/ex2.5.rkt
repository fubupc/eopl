#lang racket

(require eopl)

; Exercise 2.5 [★] We can use any data structure for representing environments, if we can distinguish
; empty environments from non-empty ones, and in which one can extract the pieces of a non-empty
; environment. Implement environments using a representation in which the empty environment is
; represented as the empty list, and in which `extend-env` builds an environment that looks like ...
; This is called an *a-list* or *association-list* representation.

; Env = ((Var . SchemeVal) . Env)

; empty-env: () → Env
(define empty-env (lambda () '()))

; extend-env: Var × SchemeVal × Env → Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

; apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((saved-var (caar env))
              (saved-val (cdar env))
              (saved-env (cdr env)))
          (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(apply-env (extend-env 'x 666 (empty-env)) 'x)

(provide empty-env extend-env apply-env)