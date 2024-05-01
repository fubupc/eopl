#lang racket

(require eopl)

; Exercise 2.14 [★★] Extend the representation of the preceding exercise to include a third procedure
; that implements `has-binding?` (see exercise 2.9).

; Env = (Var → SchemeVal . () → Bool . Var → Bool)

; empty-env : () → Env
(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda () #t)
     (lambda (var) #f))))

; extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))
     (lambda () #f)
     (lambda (var)
       (or (eqv? var saved-var)
           ((caddr saved-env) var))))))

; apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

; empty-env? : Env → Bool
(define empty-env?
  (lambda (env)
    ((cadr env))))

; has-binding? : Env × Var → Bool
(define has-binding?
  (lambda (env search-var)
    ((caddr env) search-var)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))


; Tests
(module+ test
  (require rackunit)

  (define e1 (extend-env 'a 1 (extend-env 'b 2 (extend-env 'c 3 (empty-env)))))
  (define e2 (extend-env 'b 4 e1))

  (check-false (has-binding? (empty-env) 'a))

  (check-true (has-binding? e2 'a))
  (check-true (has-binding? e2 'b))
  (check-true (has-binding? e2 'c))

  (check-false (has-binding? e2 'd)))
