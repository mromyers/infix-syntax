#lang racket/base
(require infix-syntax/core
         rackunit)

(struct T1 ()
  #:property prop:infix-procedure
  (λ(self e stx) 42))
(struct T2 (proc)
  #:property prop:infix-procedure
  (struct-field-index proc))


(define t1 (T1))
(define t2 (T2 (λ(l in) 42)))

(check-equal? (infix-app t1 0 0) 42)
(check-equal? (infix-app t2 0 0) 42)

(struct P1 ()
  #:property prop:infix-precedence
  (λ(x) 42))

(struct P2 (prec)
  #:property prop:infix-precedence
  (struct-field-index prec))

(define p1 (P1))
(define p2 (P2 42))

(check-equal? (infix-prec p1) 42)
(check-equal? (infix-prec p2) 42)









  
  
  
  







#|
(define-syntax define-infix
  (syntax-rules ()
    [(_ (name args ...) #:precedence n stuff ...)
     (define-syntax name (tok-λ (args ...) #:prec n stuff ...))]
    [(_ name #:precedence n proc)
     (define-syntax name (token proc n))]))


(define-infix (ol₁ left in)
  #:precedence 1
  (let-values ([(right out)(frob in < 3)])
    (with-syntax ([left left][right right])
      (values #'(ol₁ left right) out))))

(define-infix (#%jx left in)
  #:precedence 1
  (let-values ([(right out)(frob in < 1)])
    (with-syntax ([left left][right right])
      (values #'(#%jx left right) out))))


(define-infix #%parens
  #:precedence 9
  (case-infix
   [(l in) (infix-default l in)]
   [(  in) (values (infix-parse-all (car in)) (cdr in))]))





(check-parsed-equal?
 [1 ol₁ 1] (ol₁ 1 1))

|#
