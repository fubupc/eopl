#lang racket

;; Exercise 1.31 [★] Write the following procedures for calculating on a bintree (deﬁnition 1.1.7):
;; `leaf` and `interior-node`, which build bintrees, `leaf?`, which tests whether a bintree is a leaf,
;; and `lson`, `rson`, and `contents-of`, which extract the components of a node. `contents-of` should
;; work on both leaves and interior nodes.

(provide (all-defined-out))

; leaf: Int -> Bintree
; usage: build a leaf.
(define leaf (lambda (i) i))

; interior-node: Sym × Bintree × Bintree -> Bintree
; usage: build an interior node.
(define interior-node (lambda (s t1 t2) (list s t1 t2)))

; leaf?: Bintree -> Bool
; usage: tests whether a bintree is a leaf.
(define leaf? (lambda (t) (integer? t)))

; lson: Bintree -> Bintree
; usage: extract the left child of bintree `t`.
(define lson (lambda (t) (cadr t)))

; rson: Bintree -> Bintree
; usage: extract the right child of bintree `t`.
(define rson (lambda (t) (caddr t)))

; contents-of: Bintree -> Sym | Int
; usage: extract the symbol of interior node or the integer of leaf.
(define contents-of
  (lambda (t)
    (if (leaf? t)
        t
        (car t))))

(module+ test
  (require rackunit)

  (check-equal? (leaf? (leaf 1)) #t)
  (check-equal? (leaf? (interior-node 'foo 1 2)) #f)
  (check-equal? (lson '(bar 1 (foo 1 2))) (leaf 1))
  (check-equal? (rson '(bar 1 (foo 1 2))) (interior-node 'foo 1 2))
  (check-equal? (contents-of '(bar 1 (foo 1 2))) 'bar)
  (check-equal? (contents-of (leaf 666)) (leaf 666)))
