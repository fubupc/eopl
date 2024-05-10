#lang racket

; Exercise 2.25 [★★] Here is another version of exercise 1.33. Consider a set of trees given by the
; following grammar:
;
; Red-blue-tree ::= Red-blue-subtree
; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;                  ::= (blue-node {Red-blue-subtree}∗ )
;                  ::= (leaf-node Int)
;
; Write an equivalent deﬁnition using `define-datatype`, and use the resulting interface to write a
; procedure that takes a tree and builds a tree of the same shape, except that each leaf node is
; replaced by a leaf node that contains the number of red nodes on the path between it and the root.

(require eopl)

(define-datatype red-blue-tree red-blue-tree?
  (leaf-node
   (num integer?))
  (red-node
   (left red-blue-tree?)
   (right red-blue-tree?))
  (blue-node
   (children (list-of red-blue-tree?))))

; mark-leaves-with-red-depth : RedBlueSubtree → RedBlueSubtree
(define mark-leaves-with-red-depth
  (lambda (t)
    (mark-leaves-with-red-depth-from t 0)))

; mark-leaves-with-red-depth-from : RedBlueSubtree × Int → RedBlueSubtree
(define mark-leaves-with-red-depth-from
  (lambda (t n)
    (cases red-blue-tree t
      (leaf-node (_) (leaf-node n))
      (red-node
       (left right)
       (red-node
        (mark-leaves-with-red-depth-from left (+ n 1))
        (mark-leaves-with-red-depth-from right (+ n 1))))
      (blue-node
       (children)
       (blue-node
        (map (lambda (tt) (mark-leaves-with-red-depth-from tt n)) children))))))

; Tests
(module+ test
  (require rackunit)

  (define t
    (red-node
     (blue-node
      (list (leaf-node 26)
            (leaf-node 12)))
     (red-node
      (leaf-node 11)
      (blue-node
       (list (leaf-node 117)
             (leaf-node 14))))))

  (define expect
    (red-node
     (blue-node
      (list (leaf-node 1)
            (leaf-node 1)))
     (red-node
      (leaf-node 2)
      (blue-node
       (list (leaf-node 2)
             (leaf-node 2))))))

  (check-equal? (mark-leaves-with-red-depth t) expect))
