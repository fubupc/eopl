#lang racket

; Exercise 2.9 [★] Add to the environment interface an observer called `has-binding?` that takes an
; environment `env` and a variable `s` and tests to see if s has an associated value in `env`.
; Implement it using the a-list representation.

; Import a-list representation
(require "ex2.5.rkt")

; has-binding?: Env × Var → Bool
(define has-binding?
  (lambda (env var)
    (cond ((null? env) #f)
          ((eqv? var (caar env)) #t)
          (else (has-binding? (cdr env) var)))))

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
