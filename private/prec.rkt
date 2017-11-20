#lang racket/base
(require "parse.rkt")
(provide prop:infix-precedence
         infix-precedence
         infix-parse/cmp cmp-prec
         infix-parse-expr
         left-assoc right-assoc
         (rename-out [infix-precedence   infix-prec]))


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

(define (kwd? v)
  (equal? #t (infix-precedence v)))

(define infix-parse/cmp
  (case-lambda
    [(e in R m) (infix-parse  e in (cmp-prec R m))]
    [(  in R m) (infix-parse #f in (cmp-prec R m))]))

(define infix-parse-expr
  (case-lambda
    [(e in)(infix-parse  e in kwd?)]
    [(  in)(infix-parse #f in kwd?)]))

(define left-assoc
  (case-lambda
    [(prec in)(infix-parse/cmp #f (cdr in) <=  prec)]
    [(prec)(λ(in)(left-assoc prec in))]))

(define right-assoc
  (case-lambda
    [(prec in)(infix-parse/cmp #f (cdr in) <   prec)]
    [(prec)(λ(in)(left-assoc prec in))]))



