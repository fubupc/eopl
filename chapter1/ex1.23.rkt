#lang racket

;; Exercise 1.23 [★★] `(list-index pred lst)` returns the 0-based position of the ﬁrst element of
;; `lst` that satisﬁes the predicate `pred`. If no element of `lst` satisﬁes the predicate, then
;; `list-index` returns `#f`.

; list-index: Pred × List -> Int | Bool
; usage: returns the 0-based position of the ﬁrst element of `lst` that satisﬁes the predicate
;;       `pred`. If no element of `lst` satisﬁes the predicate, then `list-index` returns `#f`.
(define list-index
  (lambda (pred lst)
    (list-index-from pred lst 0)))

; list-index-from: Pred × List × Int -> Int | Bool
; usage: returns `n`-based position of the first element of `lst` that satisfies the predicate `pred`.
;        If no element of `lst` satisﬁes the predicate, then `list-index-from` returns `#f`.
(define list-index-from
  (lambda (pred lst n)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (list-index-from pred (cdr lst) (+ n 1))))))

(module+ test
  (require rackunit)

  (check-equal? (list-index number? '(a 2 (1 3) b 7)) 1)
  (check-equal? (list-index symbol? '(a (b c) 17 foo)) 0)
  (check-equal? (list-index symbol? '(1 2 (a b) 3)) #f))
