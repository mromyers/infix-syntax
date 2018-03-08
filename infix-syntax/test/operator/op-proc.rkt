#lang racket/base
(require "private/stx.rkt"
         racket/list
         rackunit)

(define (check-arity f a)
  (check-equal? (procedure-arity f) a))

(define-syntax-rule (check-equal-v2? e x y)
  (let-values ([(a b) e])
    (check-equal? x a)
    (check-equal? y b)))

(define l 'x)
(define in '(f a b c d e f g))

(let ([proc (op-proc (λ(o l r)(list o l r))
                     (λ(in)(split-at in 3)))])
  (check-arity proc 2)
  (check-equal-v2?
   (proc l in) '(f x (a b c)) '(d e f g)))

(let ([proc (op-proc (λ(o l r)(list o l r))
                     (λ(prec in)(split-at in prec))
                     3)])
  (check-arity proc 2)
  (check-equal-v2?
   (proc l in) '(f x (a b c)) '(d e f g)))


(let ([proc (op-proc (λ(o r)(list o r))
                     (λ(in)(split-at in 3)))])
  (check-arity proc 1)
  (check-equal-v2?
   (proc in) '(f (a b c)) '(d e f g)))

(let ([proc (op-proc (λ(o r)(list o r))
                     (λ(in)(split-at in 3)))])
  (check-arity proc 1)
  (check-equal-v2?
   (proc in) '(f (a b c)) '(d e f g)))

(let ([proc (op-proc (λ(o r)(list o r))
                     (λ(prec in)(split-at in prec))
                     3)])
  (check-arity proc 1)
  (check-equal-v2?
   (proc in) '(f (a b c)) '(d e f g)))
