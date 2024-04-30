#lang racket

; Exercise 2.1 [★] Implement the four required operations for bigits. Then use your implementation to
; calculate the factorial of 10. How does the execution time vary as this argument changes? How does
; the execution time vary as the base changes? Explain why.

; Bignum base
(define N 3)

; zero: () -> Bignum
(define zero (lambda () '()))

; is-zero?: Bignum -> Bool
(define is-zero? (lambda (n) (null? n)))

; successor: Bignum -> Bignum
(define successor
  (lambda (n)
    (if (is-zero? n)
        (list 1)
        (let ((first_digit (car n))
              (remaining (cdr n)))
          (if (< (+ first_digit 1) N)
              (cons (+ first_digit 1) remaining)
              (cons 0 (successor remaining)))))))

; predecessor: Bignum -> Bignum
(define predecessor
  (lambda (n)
    (if (is-zero? n)
        (error "predecessor of 0 is not defined.")
        (let ((first_digit (car n))
              (remaining (cdr n)))
          (cond ((= first_digit 0) (cons (- N 1) (predecessor remaining)))
                ((and (= first_digit 1) (is-zero? remaining)) (zero))
                (else (cons (- first_digit 1) remaining)))))))


; Question: Calculate the factorial of 10. How does the execution time vary as this argument changes?
;           How does the execution time vary as the base changes?

; plus complexity: O(log_N(max(x, y)))
(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (plus (predecessor x) y)))))

; muliply complexity: O(log_N(y)) + O(log_N(2*y)) + ... + O(log_N(x*y))
;                   = O(log_N(y^x*x!))
;                   = O(x*log_N(y) + log_N(x!))
;                   = O(x*log_N(y) + x*log_N(x))
;                   = O(x*log_N(x*y))
(define multiply
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (plus y (multiply (predecessor x) y)))))

; n! complexity: O(n^2*log_N(n)) + O((n-1)^2*log_N(n-1)) + .. = O(n^3*log_N(n))
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor (zero))
        (multiply n (factorial (predecessor n))))))

(module+ test
  (require rackunit)

  (define one (successor (zero)))
  (define two (successor one))
  (define three (successor two))
  (define four (successor three))

  (check-equal? (predecessor one) (zero))
  (check-equal? (predecessor two) one)
  (check-equal? (predecessor three) two)
  (check-equal? (predecessor four) three)
  (check-equal? (plus three three) (multiply two three))
  (check-equal? (plus two four) (multiply two three))
  (check-equal? (factorial four) (multiply four (multiply three (multiply two one)))))