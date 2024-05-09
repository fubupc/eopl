#lang racket

; Exercise 2.30 [★★] The procedure parse-expression as deﬁned above is fragile: it does not detect
; several possible syntactic errors, such as `(a b c)`, and aborts with inappropriate error messages
; for other expressions, such as `(lambda)`. Modify it so that it is robust, accepting any s-exp and
; issuing an appropriate error message if the s-exp does not represent a lambda-calculus expression.

(require eopl)

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

; parse-expression : SchemeVal → LcExp
(define parse-expression
  (lambda (sexp)
    (if (symbol? sexp)
        (var-exp sexp)
        (if (eqv? (car sexp) 'lambda)
            (if (and (= (length sexp) 3)
                     (list? (cadr sexp))
                     (symbol? (caadr sexp)))
                (lambda-exp (caadr sexp) (parse-expression (caddr sexp)))
                (eopl:error "invalid lambda-exp, expect (lambda (<arg>) <body>), but get: ~a" sexp))
            (if (= (length sexp) 2)
                (app-exp (parse-expression (car sexp)) (parse-expression (cadr sexp)))
                (eopl:error "invalid app-exp, should contains 2 parts, but get: ~a" sexp))))))


; Tests
(module+ test
  (require rackunit)

  (check-equal? (parse-expression '(lambda (x) (y x)))
                (lambda-exp 'x (app-exp (var-exp 'y) (var-exp 'x))))

  (check-exn #rx"invalid lambda-exp" (lambda () (parse-expression '(lambda))))
  (check-exn #rx"invalid app-exp" (lambda () (parse-expression '(a b c)))))
