#lang racket/base
(require "private/parse.rkt"
         "private/prec.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide
 ;; private/parse
 prop:infix-procedure
 infix-app infix-default fn-application-cons
 infix-local-table with-infix-binding
 infix-local-value infix-lookup-syntax
 infix-parse infix-parse-all

 ;; private/precedence
 prop:infix-precedence
 infix-precedence  infix-prec
 cmp-prec infix-parse/cmp
 left-assoc right-assoc

 ;; Here
 with-right)

(define-syntax (with-right stx)
  (define (foo stx)
    (map (λ(s)(if (identifier? s)
                  (datum->syntax s (list s s)) s))
         (syntax-e stx)))
  (syntax-parse stx
    [(_ [r get] com)
     #'(let-values ([(r out) get])
         (values com out))]
    [(_ [r get] #:syntax (b ...)
        com)
     (with-syntax ([(b* ...)(foo #'(b ...))])
       #'(let-values ([(r out) get])
           (with-syntax (b* ...)
             (values com out))))]))





