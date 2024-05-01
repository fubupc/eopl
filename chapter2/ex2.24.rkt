#lang racket

; Exercise 2.24 [★] Here is a deﬁnition of binary trees using define-datatype.
;
; (define-datatype bintree bintree?
;   (leaf-node
;    (num integer?))
;   (interior-node
;    (key symbol?)
;    (left bintree?)
;    (right bintree?)))
;
; Implement a bintree-to-list procedure for binary trees, so that
; `(bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))` returns the list
;
; (interior-node
;  a
;  (leaf-node 3)
;  (leaf-node 4))
;
; FIXME: Looks like there'a an error in the book here, it should be '(a 3 4)?

(require eopl)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

; bintree-to-list : Bintree → Listof(Sym | Int)
(define bintree-to-list
  (lambda (t)
    (cases bintree t
      (leaf-node (num) (list num))
      (interior-node (key left right)
                     (append (list key)
                             (bintree-to-list left)
                             (bintree-to-list right))))))


; Tests
(module+ test
  (require rackunit)

  (check-equal? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))) '(a 3 4)))
