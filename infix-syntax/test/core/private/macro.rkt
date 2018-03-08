#lang racket/base
(require (for-syntax racket/base
                     "stx.rkt")
         rackunit)

(provide (for-syntax (all-from-out racket/base)
                     (all-from-out "stx.rkt"))
         (rename-out [#%parse $]
                     [define-syntax def-stx])
         #%parse def-tok Q
         check-parsed-equal? check-equal?)

(define-syntax (#%parse stx)
  (parse-all (cdr (syntax-e stx))))

(define-syntax-rule (def-tok id stuff ...)
  (define-syntax id (make-tok stuff ...)))

(define-syntax (qt-p: stx)
  (with-syntax ([stx* (parse-all (cdr (syntax-e stx)))])
    #'(quote stx*)))

(define-syntax-rule (check-parsed-equal? (unparsed ...) (parsed ...))
  (check-equal? (qt-p: unparsed ...) (quote (parsed ...))))

(def-tok Q
  (λ(in)(values (with-syntax ([e (parse-all (cdr in))])
                  #'(quote e)) '())))