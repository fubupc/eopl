#lang racket

; Exercise 2.18 [★] We usually represent a sequence of values as a list. In this representation, it
; is easy to move from one element in a sequence to the next, but it is hard to move from one element
; to the preceding one without the help of context arguments. Implement non-empty bidirectional
; sequences of integers, as suggested by the grammar
;
;     NodeInSequence ::= (Int Listof(Int) Listof(Int))
;
; The ﬁrst list of numbers is the elements of the sequence preceding the current one, in reverse
; order, and the second list is the elements of the sequence after the current one. For example,
; `(6 (5 4 3 2 1) (7 8 9))` represents the list `(1 2 3 4 5 6 7 8 9)`, with the focus on the element
; 6.
;
; In this representation, implement the procedure `number->sequence`, which takes a number and produces
; a sequence consisting of exactly that number. Also implement `current-element`, `move-to-left`,
; `move-to-right`, `insert-to-left`, `insert-to-right`, `at-left-end?`, and `at-right-end?`.
;
; For example:
; > (number->sequence 7)
; (7 () ())
; > (current-element '(6 (5 4 3 2 1) (7 8 9)))
; 6
; > (move-to-left '(6 (5 4 3 2 1) (7 8 9)))
; (5 (4 3 2 1) (6 7 8 9))
; > (move-to-right '(6 (5 4 3 2 1) (7 8 9)))
; (7 (6 5 4 3 2 1) (8 9))
; > (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
; (6 (13 5 4 3 2 1) (7 8 9))
; > (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
; (6 (5 4 3 2 1) (13 7 8 9))

; The procedure `move-to-right` should fail if its argument is at the right end of the sequence, and
; the procedure `move-to-left` should fail if its argument is at the left end of the sequence.

(require eopl)

; NodeInSequence = (Int Listof(Int) Listof(Int))

; Interface:
; - Constructors:
; sequence : Int × Listof(Int) × Listof(Int) → NodeInSequence
(define sequence (lambda (n left right) (list n left right)))

; - Predicates: None, because there is only a single shape of NodeInSequence.

; - Extractors:
; sequence-current : NodeInSequence → Int
(define sequence-current (lambda (seq) (car seq)))

; sequence-left : NodeInSequence → Listof(Int)
(define sequence-left (lambda (seq) (cadr seq)))

; sequence-right : NodeInSequence → Listof(Int)
(define sequence-right (lambda (seq) (caddr seq)))


; Other functions depending on the interface:
; number->sequence : Int → NodeInSequence
(define number->sequence (lambda (n) (sequence n '() '())))

; current-element : NodeInSequence → Int
(define current-element sequence-current)

; move-to-left : NodeInSequence → NodeInSequence
(define move-to-left
  (lambda (seq)
    (if (at-left-end? seq)
        (eopl:error "move-to-left: sequence is at the left end")
        (let ((left (sequence-left seq))
              (right (sequence-right seq)))
          (sequence (car left)
                    (cdr left)
                    (cons (car left) right))))))

; move-to-right : NodeInSequence → NodeInSequence
(define move-to-right
  (lambda (seq)
    (if (at-right-end? seq)
        (eopl:error "move-to-right: sequence is at the right end")
        (let ((left (sequence-left seq))
              (right (sequence-right seq)))
          (sequence (car right)
                    (cons (car right) left)
                    (cdr right))))))

; insert-to-left : Int × NodeInSequence → NodeInSequence
(define insert-to-left
  (lambda (n seq)
    (sequence (current-element seq)
              (cons n (sequence-left seq))
              (sequence-right seq))))

;  insert-to-right : Int × NodeInSequence → NodeInSequence
(define insert-to-right
  (lambda (n seq)
    (sequence (current-element seq)
              (sequence-left seq)
              (cons n (sequence-right seq)))))

; at-left-end? : NodeInSequence → Bool
(define at-left-end? (lambda (seq) (null? (sequence-left seq))))

; at-right-end? : NodeInSequence → Bool
(define at-right-end? (lambda (seq) (null? (sequence-right seq))))


; Tests:
(number->sequence 7)
; (7 () ())
(current-element '(6 (5 4 3 2 1) (7 8 9)))
; 6
(move-to-left '(6 (5 4 3 2 1) (7 8 9)))
; (5 (4 3 2 1) (6 7 8 9))
(move-to-right '(6 (5 4 3 2 1) (7 8 9)))
; (7 (6 5 4 3 2 1) (8 9))
(insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
; (6 (13 5 4 3 2 1) (7 8 9))
(insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
; (6 (5 4 3 2 1) (13 7 8 9))
