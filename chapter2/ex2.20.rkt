#lang racket

; Exercise 2.20 [★★★] In the representation of binary trees in exercise 2.19 it is easy to move from
; a parent node to one of its sons, but it is impossible to move from a son to its parent without the
; help of context arguments. Extend the representation of lists in exercise 2.18 to represent nodes
; in a binary tree. As a hint, consider representing the portion of the tree above the current node
; by a reversed list, as in exercise 2.18.
;
; In this representation, implement the procedures from exercise 2.19. Also implement `move-up`,
; `at-root?`, and `at-leaf?`.

(require eopl)
(require (prefix-in raw: "ex2.19.rkt"))

; Bintree = (CurrentNode . Listof(ParentInfo))
; CurrentNode = RawBintree
; ParentInfo = (ParentVal . SiblingHandside . SiblingTree)
; ParentVal = Int
; SiblingHandside = 'left | 'right
; SiblingTree = RawBintree
; RawBintree = () | (Int RawBintree RawBinTree)

; Interface:
; - Constructors:
; bintree : CurrentNode × Listof(ParentInfo) → Bintree
(define bintree (lambda (current-node parents) (list current-node parents)))

; parent-info : ParentVal × SiblingHandside × SiblingTree → ParentInfo
(define parent-info (lambda (val shs st) (list val shs st)))

; - Predicates: None, because there is only a single shape of Bintree.

; - Extractors:
; bintree->current : Bintree → CurrentNode
(define bintree->current (lambda (t) (car t)))

; bintree->parents : Bintree → Listof(ParentInfo)
(define bintree->parents (lambda (t) (cadr t)))

; parent-info->val : ParentInfo -> ParentVal
(define parent-info->val (lambda (pn) (car pn)))

; parent-info->sib-handside : ParentInfo -> SiblingHandSide
(define parent-info->sib-handside (lambda (pn) (cadr pn)))

; parent-info->sib-tree : ParentInfo -> SiblingTree
(define parent-info->sib-tree (lambda (pn) (caddr pn)))


; Other functions depending on the interface:
; number->bintree : Int → Bintree
(define number->bintree (lambda (n) (bintree (raw:number->bintree n) '())))

; current-element : Bintree → Int
(define current-element (lambda (t) (raw:current-element (bintree->current t))))

; at-leaf? : Bintree → Bool
(define at-leaf? (lambda (t) (raw:empty-bintree? (bintree->current t))))

; at-root? : Bintree → Bool
(define at-root? (lambda (t) (null? (bintree->parents t))))

; move-to-left-son : Bintree → Bintree
(define move-to-left-son
  (lambda (t)
    (if (at-leaf? t)
        (eopl:error "move-to-left-son: already at leaf")
        (let ((current (bintree->current t)))
          (bintree (raw:move-to-left-son current)
                   (cons (parent-info (raw:current-element current)
                                      'right
                                      (raw:move-to-right-son current))
                         (bintree->parents t)))))))

; move-to-right-son : Bintree → Bintree
(define move-to-right-son
  (lambda (t)
    (if (at-leaf? t)
        (eopl:error "move-to-left-son: already at leaf")
        (let ((current (bintree->current t)))
          (bintree (raw:move-to-right-son current)
                   (cons (parent-info (raw:current-element current)
                                      'left
                                      (raw:move-to-left-son current))
                         (bintree->parents t)))))))

; move-up : Bintree → Bintree
(define move-up
  (lambda (t)
    (if (at-root? t)
        (eopl:error "move-up: already at root")
        (let ((current (bintree->current t))
              (parent-info (car (bintree->parents t))))
          (let ((parent (if (eqv? (parent-info->sib-handside parent-info) 'left)
                            (raw:bintree (parent-info->val parent-info) (parent-info->sib-tree parent-info) current)
                            (raw:bintree (parent-info->val parent-info) current (parent-info->sib-tree parent-info)))))
            (bintree parent (cdr (bintree->parents t))))))))

; insert-to-left : Int × Bintree → Bintree
(define insert-to-left
  (lambda (n t)
    (let ((new-current (raw:insert-to-left n (bintree->current t))))
      (bintree new-current (bintree->parents t)))))

; insert-to-right : Int × Bintree → Bintree
(define insert-to-right
  (lambda (n t)
    (let ((new-current (raw:insert-to-right n (bintree->current t))))
      (bintree new-current (bintree->parents t)))))

; update-parents : CurrentNode × ParentNodes → ParentNodes
(define update-parents
  (lambda (new-current old-parents)
    (if (null? old-parents)
        '()
        (let ((old-parent (cadr old-parents))
              (hand-side  (caar old-parents)))
          (let ((new-parent (if (eqv? hand-side 'left)
                                (raw:bintree (raw:bintree-number old-parent)
                                             new-current
                                             (raw:bintree-right old-parent))
                                (raw:bintree (raw:bintree-number old-parent)
                                             (raw:bintree-left old-parent)
                                             new-current))))
            (cons (cons hand-side new-parent)
                  (update-parents new-parent (cdr old-parents))))))))


; Tests:
(module+ test
  (require rackunit)

  (define t1 (insert-to-right 14
                              (insert-to-left 12
                                              (number->bintree 13))))
  (check-equal? (current-element (number->bintree 13)) 13)
  (check-equal? (current-element (move-to-left-son t1)) 12)
  (check-equal? (current-element (move-to-left-son t1)) 12)
  (check-equal? (at-leaf? (move-to-right-son (move-to-left-son t1))) #t)
  (check-equal? (at-root? t1) #t)
  (check-equal? (parent-info->val (car (bintree->parents (move-to-left-son t1)))) 13)

  (define t2 (insert-to-left 15 t1))
  (check-equal? (current-element t2) 13)
  (check-equal? (current-element (move-to-left-son t2)) 15)
  (check-equal? (current-element (move-to-left-son (move-to-left-son t2))) 12)
  (check-equal? (current-element (move-to-right-son t2)) 14)
  (check-equal? (current-element (move-up (move-to-left-son t2))) 13)
  (check-equal? (current-element (move-up (move-to-left-son (move-to-left-son t2)))) 15))