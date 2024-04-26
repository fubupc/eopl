#lang racket

(require eopl)

; Exercise 2.12 [★★] Implement the stack data type of exercise 2.4 using a procedural representation.

; Stack = () -> (SchemeVal . Stack)

; empty-stack : () → Stack
(define empty-stack
  (lambda ()
    (lambda ()
      report-empty-stack)))

; push : SchemeVal × Stack -> Stack
(define push
  (lambda (val stk)
    (lambda ()
      (list val stk))))

; pop : Stack -> Stack
(define pop
  (lambda (stk)
    (cadr (stk))))

; top : Stack -> SchemeVal
(define top
  (lambda (stk)
    (car (stk))))

; TODO: How to implement empty-stack?
; empty-stack? : Stack -> Bool
; (define (empty-stack? ) (lambda (stk) ??? ))

(define report-empty-stack
  (lambda ()
    (eopl:error 'empty-stack "Empty stack")))

(define stk (push 1 (push 2 (push 3 (empty-stack)))))
(eqv? 1 (top stk))
(eqv? 2 (top (pop stk)))
(eqv? 3 (top (pop (pop stk))))
