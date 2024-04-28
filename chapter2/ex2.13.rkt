#lang racket

(require eopl)

; Exercise 2.13 [★★] Extend the procedural representation to implement `empty-env?` by representing
; the environment by a list of two procedures: one that returns the value associated with a variable,
; as before, and one that returns whether or not the environment is empty.

; Env = (Var → SchemeVal . () → Bool)

; empty-env : () → Env
(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda () #t))))

; extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))
     (lambda () #f))))

; apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

; empty-env? : Env -> Bool
(define empty-env?
  (lambda (env)
    ((cadr env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))


; Some tests
(empty-env? (empty-env))
(empty-env? (extend-env 'x 1 (empty-env)))
(apply-env (extend-env 'x 1 (empty-env)) 'x)
(apply-env (extend-env 'y 2 (extend-env 'x 1 (empty-env))) 'x)
