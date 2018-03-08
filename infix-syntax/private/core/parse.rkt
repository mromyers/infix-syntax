#lang racket/base
(require "props.rkt" "scope.rkt")

(provide parse-until parse-cmp 
         parse-next  parse-all)

(define (do-parse l in stop?)
  (define tbl (infix-local-table))
  (let parse ([l l][in (stx?-e in)])
    (if (null? in) (values l in)
        (let ([v (infix-lookup-syntax (car in) tbl)])
          (if (and l (stop? v)) (values l in)
              (let-values ([(e out)(infix-app v l in)])
                (parse e out)))))))

(define parse-until
  (case-lambda
    [(l in stop?)(do-parse  l in stop?)]
    [(  in stop?)(do-parse #f in stop?)]))

(define parse-cmp
  (case-lambda
    [(e in R m) (parse-until  e in (cmp-prec R m))]
    [(  in R m) (parse-until #f in (cmp-prec R m))]))

(define parse-next
  (case-lambda
    [(e in)(parse-until  e in kwd?)]
    [(  in)(parse-until #f in kwd?)]))

(define parse-all
  (case-lambda
    [(e in)(let-values ([(e* out)(parse-until e in no?)]) e*)]
    [(  in)(let-values ([(e* out)(parse-until   in no?)]) e*)]))

;; misc
(define (stx?-e o) (if (syntax? o) (syntax-e o) o))

(define (kwd? v)  (equal? #t (infix-precedence v)))
(define (no? v) #f)
