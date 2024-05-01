#lang racket

; Exercise 2.8 [★] Add to the environment interface an observer called `empty-env?` and implement it
; using the a-list representation.

; Import a-list representation
(require "ex2.5.rkt")

; empty-env?: Env -> Bool
(define empty-env? (lambda (env) (null? env)))

(module+ test
  (require rackunit)

  (check-true (empty-env? (empty-env))))
