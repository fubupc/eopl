#lang racket

#|
Exercise 2.2 [★★] Deﬁne a representation of all the integers (negative and nonnegative) as
diff-trees, where a diff-tree is a list deﬁned by the grammar

    $$Diff-tree ::= (one) | (diff Diff-tree Diff-tree)$$

The list `(one)` represents 1. If `t1` represents `n1` and `t2` represents `n2`, then `(diff t1 t2 )`
is a representation of `n1 − n2`. So both `(one)` and `(diff (one) (diff (one) (one)))` are
representations of 1; `(diff (diff (one) (one)) (one))` is a representation of −1.

1. Show that every number has inﬁnitely many representations in this system.

Every number `n` can be expressed in the form of `n1 - n2` in inifinite ways, e.g. 2 can be expressed
as `3 - 1`, `4 - 2`, `5 - 3`, ..., and every different `n1` / `n2` has at least one different
diff-tree representation `t1` / `t2`, so diff-tree of `n` has at least one different child `t1` / `t2`
for different `n1` / `n2`, i.e. different diff-tree, which means `n` has infinitely man represetations
in diff-tree system.

2. Turn this representation of the integers into an implementation by writing `zero`, `is-zero?`,
`successor`, and `predecessor`, as speciﬁed on page 32, except that now the negative integers are
also represented. Your procedures should take as input any of the multiple legal representations of an
integer in this scheme. For example, if your successor procedure is given any of the inﬁnitely many
legal representations of 1, it should produce one of the legal representations of 2. It is permissible
for different representations of 1 to yield different legal representations of 2.

How to implement `is-zero?` if 0 can be represented by infinitely many ways like (1 1), (2, 2), (3, 3)
etc? First transform those representations to some unique representation and then compare the latters
by structure. These are 3 possible transform methods:
1. Transform to scheme number. But it may overflow during transformation.
2. Transform to bigint representation. No overflow issue but slower.
3. Define a unique canonical diff-tree form. Just for fun!

Definition of canonical diff-tree form of a number `n`:
1. (one) for n = 1
2. (diff (one) (one)) for n = 0
3. (diff Canonical(ceil(n/2)) -Canonical(floor(n/2)) for n != {0, 1}
For example, 7 is (diff 4 -3), -7 is (diff -3 4).

3. Write a procedure diff-tree-plus that does addition in this representation. Your procedure should
be optimized for the diff-tree representation, and should do its work in a constant amount of time
(independent of the size of its inputs). In particular, it should not be recursive.

If ignore the canonical limitation, `(diff-tree-plus a b)` can be implemented by first negate `b` by
swapping its left and right child then construct a new diff-tree with `a` as left and `-b` as right.

To obtains the cannonical representation, these are 2 possible methods:
1. Convert plus to multiple successors/predecessors. How to test positive/negative?
2. Mix `a` and `b`'s left and right child to obtain a "balanced" diff-tree.
|#

; zero: () -> Diff-tree
; usage: creates a canonical 0
(define (zero) (make-tree one one))

; is-zero?: Diff-tree -> Bool
; usage: assums `n` is the canonical form of 0
(define (is-zero? n)
  (if (is-one? n)
      #f
      (and (is-one? (left-child n))
           (is-one? (right-child n)))))

; successor: Diff-tree -> Diff-tree
; usage: assums `n` is in the canonical form
(define (successor n)
  (cond ((is-zero? n) one)
        ((is-one? n) (make-tree one (make-tree (zero) one)))
        ((is-even? n) (make-tree (successor (left-child n)) (right-child n)))
        (else
         (let ((left (left-child n))
               (right (predecessor (right-child n))))
           (if (is-zero? right)
               left ; simplify diff-tree when right child is 0
               (make-tree left right))))))

; predecessor: Diff-tree -> Diff-tree
; usage: assums `n` is in the canonical form
(define (predecessor n)
  (cond ((is-zero? n) (make-tree (zero) one))
        ((is-one? n) (zero))
        ((is-even? n)
         (let ((left (left-child n))
               (right (successor (right-child n))))
           (if (is-zero? right)
               left
               (make-tree left right))))
        (else (make-tree (predecessor (left-child n)) (right-child n)))))

; diff-tree-plus: Diff-tree × Diff-tree -> Diff-tree
; usage: returns a non-canonical diff-tree.
(define (diff-tree-plus a b)
  (if (is-one? b)
      (successor a)
      (make-tree a (make-tree (right-child b) (left-child b)))))

; diff-tree-plus: Diff-tree × Diff-tree -> Diff-tree
; usage: assums `a` and `b` are both in the canonical form, returns the canonical form of `a + b`.
(define (diff-tree-plus-canon a b)
  (cond ((is-one? b) (successor a))
        ((is-one? a) (successor b))
        ((is-zero? b) a)
        ((is-zero? a) b)
        (else
         ; Swap and plus `a` and `b`'s left and right child to make sure the result diff-tree to be
         ; the canonical form
         (let ((both-odd (and (is-odd? a) (is-odd? b))))
           (let ((left
                  (if both-odd
                      (diff-tree-plus-canon (left-child a) (negate (right-child b)))
                      (diff-tree-plus-canon (left-child a) (left-child b))))
                 (right
                  (if both-odd
                      (diff-tree-plus-canon (right-child a) (negate (left-child b)))
                      (diff-tree-plus-canon (right-child a) (right-child b)))))
             (if (is-zero? right)
                 left
                 (make-tree left right)))))))

;; The following are some helper functions

; one: '(one)
; usage: the canonical diff-tree for 1.
(define one '(one))

; is-one?: Diff-tree -> Bool
; usage: assumes `n` is in the canonical form.
(define (is-one? n) (eqv? n one))

; is-neg-one?: Diff-tree -> Bool
; usage: assumes `n` is in the canonical form, checks if `n` is -1.
(define (is-neg-one? n)
  (if (is-one? n)
      #f
      (and (is-zero? (left-child n))
           (is-one? (right-child n)))))

; negate: Diff-tree -> Diff-tree
; usage: assumes `n` is in the canonical form, negates `n` by flip its left and right child.
(define (negate n)
  (cond ((is-one? n) (make-tree (zero) one))
        ((is-neg-one? n) one)
        (else (make-tree (right-child n) (left-child n)))))


;; The following helper functions don't require parameters to be in the canonical form.

; make-tree: Diff-tree × Diff-tree -> Diff-tree
; usage: creates a parent tree out of two children.
(define (make-tree left right) (list 'diff left right))

; left-child: Diff-tree -> Diff-tree
; usage: get left-child of the diff-tree for `n` != 1
(define (left-child n) (cadr n))

; right-child: Diff-tree -> Diff-tree
; usage: get right-child of the diff-tree for `n` != 1
(define (right-child n) (caddr n))

; even?: Diff-tree -> Bool
; usage: checks if a diff-tree represents an even number.
(define (is-even? n)
  (if (is-one? n)
      #f
      (let ((left (is-even? (left-child n)))
            (right (is-even? (right-child n))))
        (or (and left right)
            (and (not left) (not right))))))

; even?: Diff-tree -> Bool
; usage: checks if a diff-tree represents an odd number.
(define (is-odd? n) (not (is-even? n)))

; Some tests

(is-zero? (predecessor one))
(is-zero? (predecessor (successor (zero))))
(is-zero? (predecessor (predecessor (predecessor (successor (successor one))))))

(is-one? (successor (zero)))
(is-one? (predecessor (successor one)))
(is-one? (predecessor (predecessor (successor (successor one)))))

(let* ((two (diff-tree-plus-canon one one))
       (three (diff-tree-plus-canon two one)))
  (and (is-one? (diff-tree-plus-canon three (negate two)))
       (is-neg-one? (diff-tree-plus-canon two (negate three)))))