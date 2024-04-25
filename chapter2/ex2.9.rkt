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

(has-binding? (empty-env) 'a)
(has-binding? (extend-env 'a 666 (empty-env)) 'a)