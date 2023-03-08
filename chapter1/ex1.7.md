Exercise 1.7 [**] The error message from nth-element is uninformative. Rewrite nth-element so that it produces a more informative error message,  such as “(a b c) does not have 8 elements.”

```Scheme
(define nth-element
    (lambda (lst n)
        (nth-element-helper lst n lst n)))

(define nth-element-helper)
    (lambda (lst n curr-lst curr-n)
        (if (null? curr-lst)
            (report-list-too-short lst n)
            (if (zero? curr-n)
                (car curr-lst)
                (nth-element-helper lst n (cdr curr-lst) (- curr-n 1))))))

(define report-list-too-short
    (lambda (lst n)
        (eopl:error 'nth-element
            "~s does not have ~s elements.~%" lst (+ n 1))))        
```