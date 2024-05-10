#lang racket
(require "ex1.31.rkt")

;; Exercise 1.32 [★] Write a procedure `double-tree` that takes a bintree, as represented in
;; deﬁnition 1.1.7, and produces another bintree like the original, but with all the integers in the
;; leaves doubled.

; double-tree: Bintree -> Bintree
; usage: returns a bintree with all the integers in the leaves doubled.
(define double-tree
  (lambda (t)
    (if (leaf? t)
        (* t 2)
        (interior-node
         (contents-of t)
         (double-tree (lson t))
         (double-tree (rson t))))))

(module+ test
  (require rackunit)

  (check-equal? (double-tree '(bar 1 (foo 1 2))) '(bar 2 (foo 2 4))))
