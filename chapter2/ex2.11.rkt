#lang racket

(require eopl)

; Exercise 2.11 [★★] A naive implementation of `extend-env*` from the preceding exercise requires
; time proportional to k to run. It is possible to represent environments so that `extend-env*`
; requires only constant time: represent the empty environment by the empty list, and represent a
; non-empty environment by the data structure ...
;
; This is called the *ribcage* representation. The environment is represented as a list of pairs
; called *ribs*; each left rib is a list of variables and each right rib is the corresponding list of
; values.
;
; Implement the environment interface, including `extend-env*`, in this representation.

; Env = () | (Rib . Env)
; Rib = (Listof(Var) . Listof(SchemeVal))

; empty-env : () → Env
(define empty-env (lambda () '()))

; extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (var val env)
    (if (null? env)
        (cons (list (list var) (list val)) '())
        (let ((first-rib (car env))
              (rest-env (cdr env)))
          (cons
           (list (cons var (car first-rib))
                 (cons val (cadr first-rib)))
           rest-env)))))

; apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (apply-env-in-rib (car env) (cdr env) search-var))))

; apply-env-in-rib : Listof(Var) × Listof(SchemeVal) × Env × Var → SchemeVal
(define apply-env-in-rib
  (lambda (first-rib rest-env search-var)
    (let ((vars (car first-rib))
          (vals (cadr first-rib)))
      (cond ((null? vars) (apply-env rest-env search-var))
            ((eqv? (car vars) search-var) (car vals))
            (else (apply-env-in-rib (cdr vars) (cdr vals) search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

; extend-env* : Listof(Var) × Listof(SchemeVal) × Env → Env
(define extend-env*
  (lambda (vars vals env)
    (cons (list vars vals) env)))

(apply-env (extend-env* '(a b c) '(1 2 3) (empty-env)) 'a)
