#lang racket

;; Exercise 1.34 [★★★] Write a procedure `path` that takes an integer `n` and a binary search tree
;; `bst` (page 10) that contains the integer `n`, and returns a list of `lefts` and `rights` showing
;; how to ﬁnd the node containing `n`. If `n` is found at the root, it returns the empty list.

; path: Int × Binary-search-tree -> Listof('left | 'right)
; usage: returns a list of `lefts` and `rights` showing how to ﬁnd the node containing `n`.
(define path
  (lambda (n bst)
    (let ((v (car bst)))
      (cond
        ((< n v) (cons 'left (path n (cadr bst))))
        ((= n v) '())
        ((> n v) (cons 'right (path n (caddr bst))))))))

(path 17 '(14 (7 () (12 () ()))
              (26 (20 (17 () ())
                      ())
                  (31 () ()))))
