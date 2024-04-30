#lang racket

; Exercise 2.19 [★] A binary tree with empty leaves and with interior nodes labeled with integers
; could be represented using the grammar
;
;     Bintree ::= () | (Int Bintree Bintree)
;
; In this representation, implement the procedure `number->bintree`, which takes a number and produces
; a binary tree consisting of a single node containing that number. Also implement `current-element`,
; `move-to-left-son`, `move-to-right-son`, `at-leaf?`, `insert-to-left`, and `insert-to-right`. For
; example,
;
; > (number->bintree 13)
; (13 () ())
; > (define t1 (insert-to-right 14
;                (insert-to-left 12
;                  (number->bintree 13)))
; > t1
; (13
;   (12 () ())
;   (14 () ()))
; > (move-to-left t1)
; (12 () ())
; > (current-element (move-to-left t1))
; 12
; > (at-leaf? (move-to-right (move-to-left t1)))
; #t
; > (insert-to-left 15 t1)
; (13
;   (15
;     (12 () ())
;     ())
;   (14 () ()))

(require eopl)

; Bintree = () | (Int Bintree Bintree)

; Interface:
; - Constructors:
; empty-bintree : () → Bintree
(define empty-bintree (lambda () '()))

; bintree : Int × Bintree × Bintree → Bintree
(define bintree (lambda (n left right) (list n left right)))

; - Predicates:
; empty-bintree? : Bintree → Bool
(define empty-bintree? (lambda (t) (null? t)))

; - Extractors:
; bintree->number : Bintree → Int
(define bintree->number (lambda (t) (car t)))

; bintree->left : Bintree → Bintree
(define bintree->left (lambda (t) (cadr t)))

; bintree->right : Bintree → Bintree
(define bintree->right (lambda (t) (caddr t)))


; Other functions depending on the interface:
; number->bintree : Int → Bintree
(define number->bintree (lambda (n) (bintree n '() '())))

; current-element : Bintree → Int
(define current-element bintree->number)

; move-to-left-son : Bintree → Bintree
(define move-to-left-son
  (lambda (t)
    (if (empty-bintree? t)
        (eopl:error "move-to-left: empty bintree")
        (bintree->left t))))

; move-to-right : Bintree → Bintree
(define move-to-right-son
  (lambda (t)
    (if (empty-bintree? t)
        (eopl:error "move-to-right: empty bintree")
        (bintree->right t))))

; at-leaf? : Bintree → Bool
(define at-leaf? empty-bintree?)

; insert-to-left : Int × Bintree → Bintree
(define insert-to-left
  (lambda (n t)
    (if (empty-bintree? t)
        (number->bintree n)
        (let ((left (bintree->left t))
              (right (bintree->right t)))
          (bintree (current-element t)
                   (bintree n left '())
                   right)))))


; insert-to-right : Int × Bintree → Bintree
(define insert-to-right
  (lambda (n t)
    (if (empty-bintree? t)
        (number->bintree n)
        (let ((left (bintree->left t))
              (right (bintree->right t)))
          (bintree (current-element t)
                   left
                   (bintree n '() right))))))

(provide (all-defined-out))

; Tests:
(module+ test
  (require rackunit)

  (define t1 (insert-to-right 14
                              (insert-to-left 12
                                              (number->bintree 13))))

  (check-equal? (number->bintree 13) '(13 () ()))
  (check-equal? t1 '(13
                     (12 () ())
                     (14 () ())))
  (check-equal? (move-to-left-son t1) '(12 () ()))
  (check-equal? (current-element (move-to-left-son t1)) 12)
  (check-equal? (at-leaf? (move-to-right-son (move-to-left-son t1))) #t)
  (check-equal? (insert-to-left 15 t1) '(13
                                         (15
                                          (12 () ())
                                          ())
                                         (14 () ()))))