#lang racket


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

(require eopl)

; Env = () | Listof(RibPair)
; RibPair = (Listof(Var) . Listof(SchemeVal))

; rib-pair : Listof(Var) × Listof(SchemeVal) → RibPair
(define rib-pair (lambda (vars vals) (cons vars vals)))

; rib-pair->vars : RibPair → Listof(Var)
(define rib-pair->vars (lambda (rib) (car rib)))

; rib-pair->vals : RibPair → Listof(SchemeVal)
(define rib-pair->vals (lambda (rib) (cdr rib)))

; rib-pair->add : Var × SchemeVal × RibPair → RibPair
(define rib-pair->add
  (lambda (var val rib)
    (rib-pair (cons var (rib-pair->vars rib))
              (cons val (rib-pair->vals rib)))))

; empty-env : () → Env
(define empty-env (lambda () '()))

; extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (var val env)
    (if (null? env)
        (list (rib-pair (list var) (list val)))
        (let ((first-rib (car env))
              (rest-env (cdr env)))
          (cons (rib-pair->add var val) rest-env)))))

; apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (apply-env-in-rib (car env) (cdr env) search-var))))

; apply-env-in-rib : RibPair × Env × Var → SchemeVal
(define apply-env-in-rib
  (lambda (first-rib rest-env search-var)
    (let ((vars (rib-pair->vars first-rib))
          (vals (rib-pair->vals first-rib)))
      (cond ((null? vars) (apply-env rest-env search-var))
            ((eqv? (car vars) search-var) (car vals))
            (else (apply-env-in-rib
                   (rib-pair (cdr vars) (cdr vals))
                   rest-env
                   search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

; extend-env* : Listof(Var) × Listof(SchemeVal) × Env → Env
(define extend-env*
  (lambda (vars vals env)
    (cons (rib-pair vars vals) env)))

(module+ test
  (require rackunit)

  (define e (extend-env* '(c d) '(4 5) (extend-env* '(a b c) '(1 2 3) (empty-env))))
  (check-equal? (apply-env e 'a) 1)
  (check-equal? (apply-env e 'b) 2)
  (check-equal? (apply-env e 'c) 4) ; not 3
  (check-equal? (apply-env e 'd) 5)
  (check-exn exn:fail? (lambda () (apply-env e 'e))))
