#lang racket/base
(require (rename-in racket/base
                    [procedure-arity-includes? pai?]))

(provide prop:infix-procedure  infix-app   infix-default
         prop:infix-precedence infix-precedence cmp-prec)

;;; Infix Properties
(define-values [prop:infix-procedure
                infix-procedure?
                infix-procedure]
  (make-struct-type-property 'infix-procedure
    (λ(proc lst)
      (cond [(exact-nonnegative-integer? proc)
             (field-proc (make-struct-field-accessor
                          (list-ref lst 3) proc))]
            [(procedure? proc) (static-proc proc)]))))

(define ((field-proc ref) self l in)
  (let ([proc (ref self)])
    (if l (if (pai? proc 2)(proc l in)(infix-default l in))
          (if (pai? proc 1)(proc   in)         (proc l in)))))

(define (static-proc proc)
  (if (pai? proc 3)
      (if (pai? proc 2) (static-case proc) proc)
      (if (pai? proc 2) (static-un   proc)
          (error "You screwed up."))))

(define ((static-un proc) self l in)
  (if l (infix-default l in) (proc self in)))
(define ((static-case proc) self l in)
  (if l (proc self l in) (proc self in)))


(define (infix-app v l in)
  (if (infix-procedure? v)
      ((infix-procedure v) v l in)
      (infix-default l in)))

(define (infix-default l in)
  (if l (values l (jx-cons in))
      (values (car in) (cdr in))))

(define (jx-cons in)
  (let ([a (car in)])
    (if (equal? (syntax-e a) '#%jx)
        (raise-syntax-error #f "#%jx undefined")
        (cons (datum->syntax a '#%jx) in))))



;;; Infix Precedence
(define-values [prop:infix-precedence
                infix-precedence?
                infix-precedence-ref]
  (make-struct-type-property
   'infix-precedence
   (λ(v lst)
     (if (exact-nonnegative-integer? v)
         (let ([ref (list-ref lst 3)])
           (make-struct-field-accessor ref v))
         v))))

(define (infix-precedence v)
  (if (boolean? v) v
      (if (infix-precedence? v)
          ((infix-precedence-ref v) v) #f)))

(define cmp-prec
  (case-lambda
    [(v R m)(cond [(boolean? v) v]
                  [(infix-precedence? v)
                   (define n ((infix-precedence-ref v) v))
                   (if (boolean? n) n (n . R . m))]
                  [else #f])]
    [(R m)(λ(v)(cmp-prec v R m))]))



