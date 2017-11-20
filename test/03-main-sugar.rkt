#lang racket
(require "private/test.rkt"
         infix-syntax)

(check-expand-once*
 [(with-right [r get] com)
  => (let-values ([(r out) get])
       (values com out))]
 [(with-right [r get]
    #:syntax ([id (car in)] l r)
    #'(id l r))  
  => (let-values ([(r out) get])
       (with-syntax ([id (car in)][l l][r r])
         (values #'(id l r) out)))])

(check-expand-once*
 [(infix-lambda (l in) #:prec 3
                body stuff in here
                #:expand ex)
  => (make-infix #:precedence 3
                 (λ(l in) body stuff in here)
                 #:expand ex)])
