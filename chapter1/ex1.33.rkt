#lang racket

;; Exercise 1.33 [★★] Write a procedure `mark-leaves-with-red-depth` that takes a bintree
;; (deﬁnition 1.1.7), and produces a bintree of the same shape as the original, except that in the
;; new tree, each leaf contains the integer of nodes between it and the root that contain the symbol
;; `red`.

(require "ex1.31.rkt")

; mark-leaves-with-red-depth: Bintree -> Bintree
; usage: returns a bintree with each leaf contains the number of nodes between it and the root that
; contain the symbol `red`.
(define mark-leaves-with-red-depth
  (lambda (t)
    (mark-leaves-with-red-depth-from t 0)))

; mark-leaves-with-red-depth-from: Bintree -> Bintree
; usage: returns a bintree with each leaf contains `n` plus the number of nodes between it and the
; root that contain the symbol `red`.
(define mark-leaves-with-red-depth-from
  (lambda (t n)
    (if (leaf? t)
        n
        (let ((new_n (if (eqv? 'red (contents-of t)) (+ n 1) n)))
          (interior-node
           (contents-of t)
           (mark-leaves-with-red-depth-from (lson t) new_n)
           (mark-leaves-with-red-depth-from (rson t) new_n))))))

(module+ test
  (require rackunit)

  (check-equal?
   (mark-leaves-with-red-depth
    (interior-node 'red
                   (interior-node 'bar
                                  (leaf 26)
                                  (leaf 12))
                   (interior-node 'red
                                  (leaf 11)
                                  (interior-node 'quux
                                                 (leaf 117)
                                                 (leaf 14)))))
   (interior-node 'red
                  (interior-node 'bar
                                 (leaf 1)
                                 (leaf 1))
                  (interior-node 'red
                                 (leaf 2)
                                 (interior-node 'quux
                                                (leaf 2)
                                                (leaf 2))))))
