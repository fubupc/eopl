#lang racket

; Exercise 2.21 [★] Implement the data type of environments, as in section 2.2.2, using
; `define-datatype`. Then include `has-binding?` of exercise 2.9.

(require eopl)

; Define data type of Env
(define-datatype env env?
  (empty-env)
  (non-empty-env
   (var symbol?)
   (val (lambda (_) #t))
   (rest env?)))

; extend-env : Var × SchemeVal × Env → Env
(define extend-env non-empty-env)

; apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (e search-var)
    (cases env e
      (empty-env () (report-no-binding-found search-var))
      (non-empty-env (var val rest)
                     (if (eqv? var search-var)
                         val
                         (apply-env rest search-var))))))

; has-binding?: Env × Var → Bool
(define has-binding?
  (lambda (e v)
    (cases env e
      (empty-env () #f)
      (non-empty-env (var val rest)
                     (if (eqv? var v)
                         #t
                         (has-binding? rest v))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(module+ test
  (require rackunit)

  (check-equal? (apply-env (extend-env 'a 1 (empty-env)) 'a) 1)
  (check-false (has-binding? (empty-env) 'a))
  (check-true (has-binding? (extend-env 'a 1 (empty-env)) 'a)))
