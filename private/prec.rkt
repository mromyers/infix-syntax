#lang racket/base
(require "parse.rkt")
(provide prop:infix-precedence
         infix-precedence?
         infix-precedence

         compare-precedence cmp->stop
         infix-parse/cmp

         (rename-out [infix-precedence? infix-prec?]
                     [infix-precedence  infix-prec ]))


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
  (if (infix-precedence? v)
      ((infix-precedence-ref v) v) #f))

(define (compare-precedence v R m)
  (and v (infix-precedence? v)
       (let ([n ((infix-precedence-ref v) v)])
         (if (boolean? n) n
             (n . R . m)))))

(define ((cmp->stop R m) s v)
  (compare-precedence v R m))

(define infix-parse/cmp
  (case-lambda
    [(e in R m) (infix-parse  e in (cmp->stop R m))]
    [(  in R m) (infix-parse #f in (cmp->stop R m))]))



