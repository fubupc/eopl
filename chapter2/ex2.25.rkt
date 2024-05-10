#lang racket

; Exercise 2.25 [★★] Use cases to write `max-interior`, which takes a binary tree of integers (as in
; the preceding exercise) with at least one interior node and returns the symbol associated with an
; interior node with a maximal leaf sum.
;
; > (define tree-1
;     (interior-node ’foo (leaf-node 2) (leaf-node 3)))
; > (define tree-2
;     (interior-node ’bar (leaf-node -1) tree-1))
; > (define tree-3
;     (interior-node ’baz tree-2 (leaf-node 1)))
; > (max-interior tree-2)
; foo
; > (max-interior tree-3)
; baz
;
; The last invocation of `max-interior` might also have returned `foo`, since both the `foo` and `baz`
; nodes have a leaf sum of 5.

(require eopl)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

; max-interior : Bintree → Bool
(define is-leaf
  (lambda (t)
    (cases bintree t
      (leaf-node (_) #t)
      (else #f))))

; leaf-sum : Bintree → Int
(define leaf-sum
  (lambda (t)
    (cases bintree t
      (leaf-node (num) num)
      (interior-node (_ left right) (+ (leaf-sum left) (leaf-sum right))))))

; max-interior : Bintree → Sym
(define max-interior
  (lambda (t)
    (cases bintree t
      (leaf-node (_) (eopl:error "max-interior: make no sense on leaf node"))
      (interior-node
       (key left right)
       (cond
         ((and (is-leaf left) (is-leaf right)) key)
         ((and (is-leaf left) (not (is-leaf right)))
          (if (> (leaf-sum left) 0)
              key
              (max-interior right)))
         ((and (not (is-leaf left)) (is-leaf right))
          (if (> (leaf-sum right) 0)
              key
              (max-interior left)))
         (else
          (let ((left-sum (leaf-sum left))
                (right-sum (leaf-sum right)))
            (if (and (> left-sum 0) (> right-sum 0))
                key
                (if (> left-sum right-sum)
                    (max-interior left)
                    (max-interior right))))))))))

; Tests
(module+ test
  (require rackunit)

  (define tree-1
    (interior-node 'foo (leaf-node 2) (leaf-node 3)))
  (define tree-2
    (interior-node 'bar (leaf-node -1) tree-1))
  (define tree-3
    (interior-node 'baz tree-2 (leaf-node 1)))

  (check-equal? (max-interior tree-2) 'foo)
  (check-equal? (max-interior tree-3) 'baz))
