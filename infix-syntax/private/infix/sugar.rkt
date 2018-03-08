#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "token.rkt")

(provide infix-token infix-lambda)

(define (infix-token proc
                     #:precedence [prec #f]
                     #:expand     [ex   #f])
  (make-infix-token proc prec ex))

(begin-for-syntax
  (define-splicing-syntax-class prec-opt
    [pattern (~seq (~or #:prec
                        #:precedence) prec:expr)
             #:with val #'prec]
    [pattern (~seq) #:with val #'#f])
  
  (define-splicing-syntax-class ex-opt
    [pattern (~seq #:expand ex:expr)
             #:with val #'ex]
    [pattern (~seq) #:with val #'#f]))


(define-syntax (infix-lambda stx)
  (syntax-parse stx
    [(_ args:expr o1:prec-opt
        body:expr ...
        o2:ex-opt)
     #'(make-infix-token (λ args body ...) o1.val o2.val)]))
