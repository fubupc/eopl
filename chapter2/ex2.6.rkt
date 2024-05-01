#lang racket

; Exercise 2.6 [★] Invent at least three different representations of the environment interface and
; implement them.

; #1: Flat list: Env = () | (Var SchemeVal . Env)
(module flat-list eopl
  (provide empty-env extend-env apply-env)

  ; empty-env: () → Env
  (define empty-env (lambda () '()))

  ; extend-env: Var × SchemeVal × Env → Env
  (define extend-env
    (lambda (var val env)
      (cons var (cons val env))))

  ; apply-env : Env × Var → SchemeVal
  (define apply-env
    (lambda (env search-var)
      (if (null? env)
          (report-no-binding-found search-var)
          (let ((saved-var (car env))
                (saved-val (cadr env))
                (saved-env (cddr env)))
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var))))))

  (define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var)))

  )

; #2: Binary search tree: Env = () | ((Var . SchemeVal) Env Env)
(module bst eopl
  (provide empty-env extend-env apply-env)

  ; empty-env: () → Env
  (define empty-env (lambda () '()))

  ; extend-env: Var × SchemeVal × Env → Env
  (define extend-env
    (lambda (var val env)
      (if (null? env)
          (list (cons var val) (empty-env) (empty-env))
          (let ((curr (car env))
                (left (cadr env))
                (right (caddr env)))
            (cond
              ((string=? (symbol->string var) (symbol->string (car curr)))
               (list (cons var val) left right))
              ((string<? (symbol->string var) (symbol->string (car curr)))
               (list curr (extend-env var val left) right))
              (else
               (list curr left (extend-env var val right))))))))

  ; apply-env : Env × Var → SchemeVal
  (define apply-env
    (lambda (env search-var)
      (if (null? env)
          (report-no-binding-found search-var)
          (let ((curr (car env))
                (left (cadr env))
                (right (caddr env)))
            (cond
              ((string=? (symbol->string search-var) (symbol->string (car curr)))
               (cdr curr))
              ((string<? (symbol->string search-var) (symbol->string (car curr)))
               (apply-env left search-var))
              (else
               (apply-env right search-var)))))))

  (define report-no-binding-found
    (lambda (search-var)
      (eopl:error 'apply-env "No binding for ~s" search-var))))

; TODO: #3: ???


; Tests
(module+ test
  (require rackunit)
  (require (prefix-in flist: (submod ".." flat-list)))
  (require (prefix-in bst: (submod ".." flat-list)))

  (test-case
   "Test flat-list representation"
   (define e1 (flist:extend-env 'a 1 (flist:extend-env 'b 2 (flist:extend-env 'c 3 (flist:empty-env)))))
   (define e2 (flist:extend-env 'b 4 e1))

   (check-equal? (flist:apply-env e1 'b) 2)

   (check-equal? (flist:apply-env e2 'a) 1)
   (check-equal? (flist:apply-env e2 'b) 4) ; not 2
   (check-equal? (flist:apply-env e2 'c) 3)

   (check-exn exn:fail? (lambda () (flist:apply-env (flist:empty-env) 'a)))
   (check-exn exn:fail? (lambda () (flist:apply-env e2 'd))))

  (test-case
   "Test bst representation"
   (define e1 (bst:extend-env 'a 1 (bst:extend-env 'b 2 (bst:extend-env 'c 3 (bst:empty-env)))))
   (define e2 (bst:extend-env 'b 4 e1))

   (check-equal? (bst:apply-env e1 'b) 2)

   (check-equal? (bst:apply-env e2 'a) 1)
   (check-equal? (bst:apply-env e2 'b) 4) ; not 2
   (check-equal? (bst:apply-env e2 'c) 3)

   (check-exn exn:fail? (lambda () (bst:apply-env (bst:empty-env) 'a)))
   (check-exn exn:fail? (lambda () (bst:apply-env e2 'd)))))
