#lang racket

; Exercise 2.31 [★★] Sometimes it is useful to specify a concrete syntax as a sequence of symbols and
; integers, surrounded by parentheses. For example, one might deﬁne the set of preﬁx lists by
;
; Preﬁx-list ::= (Preﬁx-exp)
; Preﬁx-exp  ::= Int
;             ::= - Preﬁx-exp Preﬁx-exp
;
; so that `(- - 3 2 - 4 - 12 7)` is a legal preﬁx list. This is sometimes called Polish preﬁx
; notation, after its inventor, Jan Łukasiewicz. Write a parser to convert a preﬁx-list to the
; abstract syntax

; (define-datatype prefix-exp prefix-exp?
;   (const-exp
;    (num integer?))
;   (diff-exp
;    (operand1 prefix-exp?)
;    (operand2 prefix-exp?)))
;
; so that the example above produces the same abstract syntax tree as the sequence of constructors
;
; (diff-exp
;  (diff-exp
;   (const-exp 3)
;   (const-exp 2))
;  (diff-exp
;   (const-exp 4)
;   (diff-exp
;    (const-exp 12)
;    (const-exp 7))))
;
; As a hint, consider writing a procedure that takes a list and produces a prefix-exp and the list of
; leftover list elements.

(require eopl)

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

; parse-prefix-exp : Listof(Sym | Int) → PrefixExp
; usage: parses a prefix-list to the abstract syntax.
(define parse-prefix-exp
  (lambda (plist)
    (let ((r (parse-prefix-exp-partial plist)))
      (if (null? (cdr r))
          (car r)
          (eopl:error "parse fail: extra tokens at the tail of list")))))

; parse-prefix-exp-partial : Listof(Sym | Int) → (PrefixExp . Listof(Sym | Int))
; usage: takes a list and produces a prefix-exp and the list of leftover list elements.
(define parse-prefix-exp-partial
  (lambda (plist)
    (if (list? plist)
        (if (null? plist)
            (eopl:error "wrong input: empty prefix-list")
            (cond
              ((integer? (car plist))
               (cons (const-exp (car plist)) (cdr plist)))
              ((eqv? (car plist) '-)
               (if (null? (cdr plist))
                   (eopl:error "missing both operands in diff-exp")
                   (let ((r1 (parse-prefix-exp-partial (cdr plist))))
                     (if (null? (cdr r1))
                         (eopl:error "missing operand2 in diff-exp")
                         (let ((r2 (parse-prefix-exp-partial (cdr r1))))
                           (cons (diff-exp (car r1) (car r2))
                                 (cdr r2)))))))
              (else (eopl:error "unknown operator:" (car plist)))))
        (eopl:error "wrong input: expect a prefix-list but get a non-list:" plist))))

; Tests
(module+ test
  (require rackunit)

  (check-equal? (parse-prefix-exp '(- - 3 2 - 4 - 12 7))
                (diff-exp
                 (diff-exp
                  (const-exp 3)
                  (const-exp 2))
                 (diff-exp
                  (const-exp 4)
                  (diff-exp
                   (const-exp 12)
                   (const-exp 7)))))

  (check-exn #rx"wrong input: empty prefix-list" (lambda () (parse-prefix-exp '())))
  (check-exn #rx"wrong input: expect a prefix-list but get a non-list: 'a" (lambda () (parse-prefix-exp 'a)))
  (check-exn #rx"unknown operator: 'a" (lambda () (parse-prefix-exp '(a))))
  (check-exn #rx"missing operand2 in diff-exp" (lambda () (parse-prefix-exp '(- 3))))
  (check-exn #rx"parse fail: extra tokens at the tail of list" (lambda () (parse-prefix-exp '(- 3 3 3)))))
