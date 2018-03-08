#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     infix-syntax))

(provide define-infix #%parse
         (rename-out [#%parse $])
         (for-syntax (all-from-out infix-syntax) #%app))

(define-syntax (define-infix stx)
  (syntax-parse stx
    [(_ (name:id . args) stuff ...)
     #'(define-syntax name
         (infix-lambda args stuff ...))]
    [(_ name:id stuff ...)
     #'(define-syntax name
         (infix-token stuff ...))]))

(define-syntax (#%parse stx)
  (parse-all (cdr (syntax-e stx))))

(define-syntax (qt-p: stx)
  (with-syntax ([stx* (parse-all (cdr (syntax-e stx)))])
    #'(quote stx*)))
