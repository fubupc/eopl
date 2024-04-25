#lang racket

; Exercise 2.6 [★] Invent at least three different representations of the environment interface and
; implement them.

; #1: Flat list: Env ::= () | (Var SchemeVal . Env)
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
      (eopl:error 'apply-env "No binding for ~s" search-var))))

; #2: Binary search tree: Env ::= () | ((Var . SchemeVal) Env Env)
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

(require 'flat-list)
; (require 'bst)

; (extend-env 'a 111 (empty-env))
(let ((env (extend-env 'c 333 (extend-env 'b 222 (extend-env 'a 111 (empty-env))))))
  (printf "env: ~s\n" env)
  (printf "a: ~s\n" (apply-env env 'a))
  (printf "b: ~s\n" (apply-env env 'b))
  (printf "c: ~s\n" (apply-env env 'c))
  (printf "d: ~s\n" (apply-env env 'd)))
