#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     infix-syntax))

(provide #%parse quote/infix-parse
         define-infix define-operator
         (for-syntax #%app #%datum
                     infix-app infix-default
                     jx-cons cmp->stop
                     with-infix-binding infix-local-value
                     infix-parse infix-parse/cmp
                     infix-parse-all
                     infix-precedence? infix-prec?
                     infix-precedence  infix-prec

                     make-infix infix-lambda
                     with-get
                     left-assoc right-assoc
                     postfix))


(define-syntax (#%parse stx)
  (infix-parse-all (cdr (syntax-e stx))))

(define-syntax (quote/infix-parse stx)
  (with-syntax ([stuff (infix-parse-all (cdr (syntax-e stx)))])
    #'(quote stuff)))


(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ name:id stuff ...)
     #'(define-syntax name (infix-lambda stuff ...))]
    [(_ (name:id x:id ...) stuff ...)
     #'(define-syntax name (infix-lambda (x ...) stuff ...))]))

(define-syntax (define-operator stx)
  (syntax-parse stx
    [(_ (name:id x ...) stuff ...)
     #'(define-syntax name
         (operator-lambda (x ...) stuff ...))]
    [(_ name:id stuff ...)
     #'(define-syntax name
         (make-operator stuff ...))]))

    

