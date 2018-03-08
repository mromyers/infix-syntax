#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     infix-syntax)
         rackunit)
(provide (all-defined-out) check-equal?)

(define-syntax (qt-p: stx)
  (with-syntax ([stx* (parse-all (cdr (syntax-e stx)))])
    #'(quote stx*)))

(define-syntax-rule (check-parsed-equal? (unparsed ...) (parsed ...))
  (check-equal? (qt-p: unparsed ...) (quote (parsed ...))))

(begin-for-syntax
  (define (local-expand-once stx)
    (syntax-parse stx
      [(a:id stuff ...)
       (let ([v (syntax-local-value #'a (λ() #f))])
         (if (and v (procedure? v))
             (v stx) stx))]))
  )

(define-syntax (quote/expand-once stx)
  (syntax-parse stx
    [(_ a)(with-syntax ([a* (local-expand-once #'a)])
            #'(quote a*))]))


(define-syntax-rule (check-expand-once a => b)
  (check-equal? (quote/expand-once a) 'b))

(define-syntax-rule (check-expand-once* (stuff ...) ...)
  (begin (check-expand-once stuff ...) ...))
