#lang racket

(require "ex1.31.rkt")

;; Exercise 1.35 [★★★] Write a procedure `number-leaves` that takes a bintree, and produces a
;; bintree like the original, except the contents of the leaves are numbered starting from 0.

; number-leaves: Bintree -> Bintree
; usage: produces a bintree like the original, except the contents of the leaves are numbered starting
; from 0.
(define number-leaves
  (lambda (t)
    (car (number-leaves-from-n t 0))))

; number-leaves-from-n Bintree × Int -> (Bintree . Int)
; usage: returns a 2-list(2 values): one is a bintree like the original, except the contents of the
; leaves are numbered starting from `n`, the other is the number of leaves.
(define number-leaves-from-n
  (lambda (t n)
    (if (leaf? t)
        (list (leaf n) 1)
        (let* ((left (number-leaves-from-n (lson t) n))
               (left-tree (car left))
               (left-leaves-num (cadr left)))
          (let* ((right (number-leaves-from-n (rson t) (+ n left-leaves-num)))
                 (right-tree (car right))
                 (rigth-leaves-num (cadr right)))
            (list (interior-node (contents-of t) left-tree right-tree)
                  (+ left-leaves-num rigth-leaves-num )))))))

(number-leaves
 (interior-node 'foo
                (interior-node 'bar
                               (leaf 26)
                               (leaf 12))
                (interior-node 'baz
                               (leaf 11)
                               (interior-node 'quux
                                              (leaf 117)
                                              (leaf 14)))))