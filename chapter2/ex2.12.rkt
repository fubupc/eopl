#lang racket

(require eopl)

; Exercise 2.12 [★★] Implement the stack data type of exercise 2.4 using a procedural representation.

; Stack = () -> (SchemeVal . Stack)

; empty-stack : () → Stack
(define empty-stack
  (lambda ()
    (lambda ()
      '())))

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

; empty-stack? : Stack -> Bool
(define empty-stack?
  (lambda (stk)
    (null? (stk))))

; Tests
(module+ test
  (require rackunit)

  (define stk (push 1 (push 2 (push 3 (empty-stack)))))

  (check-true (empty-stack? (pop (pop (pop stk)))))
  (check-equal? (top stk) 1)
  (check-equal? (top (pop stk)) 2)
  (check-exn exn:fail? (lambda () (top (empty-stack))))
  (check-exn exn:fail? (lambda () (pop (empty-stack))))
  (check-exn exn:fail? (lambda () (top (pop (pop (pop stk))))))
  (check-exn exn:fail? (lambda () (pop (pop (pop (pop stk)))))))
