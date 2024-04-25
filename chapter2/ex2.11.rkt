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
              (rest-ribs (cdr env)))
          (cons
           (list (cons var (car first-rib))
                 (cons val (cadr first-rib)))
           rest-ribs)))))

; apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((first-rib (car env))
              (rest-ribs (cdr env)))
          (let ((result (search-var-in-rib (car first-rib) (cadr first-rib) search-var)))
            (if (eqv? 'found (car result))
                (cdr result)
                (apply-env rest-ribs search-var)))))))

; search-var-in-rib : Listof(Var) × Listof(SchemeVal) × Var → SchemeVal
(define search-var-in-rib
  (lambda (vars vals search-var)
    (cond ((null? vars) (cons 'notfound 'null))
          ((eqv? (car vars) search-var) (cons 'found (car vals)))
          (else (search-var-in-rib (cdr vars) (cdr vals) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

; extend-env* : Listof(Var) × Listof(SchemeVal) × Env → Env
(define extend-env*
  (lambda (vars vals env)
    (cons (list vars vals) env)))

(apply-env (extend-env* '(a b c) '(1 2 3) (empty-env)) 'a)
