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


; Tests
(module+ test
  (require rackunit)

  (define e1 (extend-env 'a 1 (extend-env 'b 2 (extend-env 'c 3 (empty-env)))))
  (define e2 (extend-env 'b 4 e1))

  (check-equal? (apply-env e1 'b) 2)

  (check-equal? (apply-env e2 'a) 1)
  (check-equal? (apply-env e2 'b) 4) ; not 2
  (check-equal? (apply-env e2 'c) 3)

  (check-exn exn:fail? (lambda () (apply-env (empty-env) 'a)))
  (check-exn exn:fail? (lambda () (apply-env e2 'd))))
