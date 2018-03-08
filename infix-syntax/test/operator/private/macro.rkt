#lang racket/base
(require (for-syntax racket/base
                     "stx.rkt")
         rackunit)

(provide (for-syntax (all-from-out racket/base)
                     (all-from-out "stx.rkt"))
         (rename-out [#%parse $])
         #%parse check-parsed-equal? check-equal?)

(define-syntax (#%parse stx)
  (parse-all (cdr (syntax-e stx))))

(define-syntax (qt-p: stx)
  (with-syntax ([stx* (parse-all (cdr (syntax-e stx)))])
    #'(quote stx*)))

(define-syntax-rule (check-parsed-equal? (unparsed ...) (parsed ...))
  (check-equal? (qt-p: unparsed ...) (quote (parsed ...))))
