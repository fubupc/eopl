#lang racket

; Exercise 1.7 [★★] The error message from nth-element is uninformative. Rewrite nth-element so that
; it produces a more informative error message,  such as “(a b c) does not have 8 elements.”

(require eopl)

(define nth-element
  (lambda (lst n)
    (nth-element-helper lst n lst n)))

(define nth-element-helper
  (lambda (orig-lst orig-n curr-lst curr-n)
    (if (null? curr-lst)
        (report-list-too-short orig-lst orig-n)
        (if (zero? curr-n)
            (car curr-lst)
            (nth-element-helper orig-lst orig-n (cdr curr-lst) (- curr-n 1))))))

(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~s does not have ~s elements." lst (+ n 1))))

(module+ test
  (require rackunit)

  (check-equal? (nth-element '(a b c) 1) 'b)
  (check-exn #rx"nth-element: \\(a b c\\) does not have 8 elements." (lambda () (nth-element '(a b c) 7))))
