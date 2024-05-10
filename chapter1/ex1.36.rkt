#lang racket

;; Exercise 1.35 [★★★] Write a procedure g such that number-elements from page 23 could be deﬁned
;; as
;;
;; ```
;; (define number-elements
;;  (lambda (lst)
;;     (if (null? lst) '()
;;         (g (list 0 (car lst)) (number-elements (cdr lst))))))
;; ```
;;

; g: 2-lists × Listof(2-lists) -> Listof(2-lists)
; usage: produces a list of 2-lists with `p` as head and `lst` renumbered from 1 as tail.
(define g
  (lambda (p lst)
    (cons p
          (map (lambda (e) (cons (+ 1 (car e)) (cdr e))) lst))))

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(module+ test
  (require rackunit)

  (check-equal? (number-elements '(a b c d e))
                '((0 a) (1 b) (2 c) (3 d) (4 e))))

