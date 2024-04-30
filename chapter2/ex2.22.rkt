#lang racket

; Exercise 2.22 [★] Using define-datatype, implement the stack data type of exercise 2.4.

(require eopl)

; Define data type of stack
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack
   (top (lambda (_) #t))
   (rest stack?)))

; empty-stack? : Stack → Bool
(define empty-stack?
  (lambda (stk)
    (cases stack stk
      (empty-stack () #t)
      (else #f))))

; push : SchemeVal × Stack → Stack
(define push non-empty-stack)

; pop : Stack → Stack
(define pop
  (lambda (stk)
    (cases stack stk
      (empty-stack () (eopl:error "pop: on empty stack"))
      (non-empty-stack (_ rest) rest))))

; top : Stack → SchemeVal
(define top
  (lambda (stk)
    (cases stack stk
      (empty-stack () (eopl:error "top: on empty stack"))
      (non-empty-stack (top _) top))))


(module+ test
  (require rackunit)

  (define stk (push 1 (push 2 (push 3 (empty-stack)))))

  (check-true (empty-stack? (pop (pop (pop stk)))))
  (check-equal? (top stk) 1)
  (check-equal? (top (pop stk)) 2)
  (check-equal? (top (pop (pop stk))) 3))
