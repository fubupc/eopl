#lang racket
(provide (all-defined-out))

;; Exercise 1.31 [★] Write the following procedures for calculating on a bintree (deﬁnition 1.1.7):
;; `leaf` and `interior-node`, which build bintrees, `leaf?`, which tests whether a bintree is a leaf,
;; and `lson`, `rson`, and `contents-of`, which extract the components of a node. `contents-of` should
;; work on both leaves and interior nodes.

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


; (leaf 1)
; (interior-node 'foo (leaf 1) (leaf 2))
; (leaf? (leaf 1))
; (leaf? (interior-node 'foo 1 2))
; (lson '(bar 1 (foo 1 2)))
; (rson '(bar 1 (foo 1 2)))
; (contents-of '(bar 1 (foo 1 2)))
; (contents-of (leaf 666))
