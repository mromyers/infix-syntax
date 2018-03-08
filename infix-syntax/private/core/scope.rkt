#lang racket/base
(provide infix-local-table
         infix-lookup-syntax
         with-infix-binding)

(define *infix-local-table* (make-parameter #f))
(define (infix-local-table)(*infix-local-table*))

(define (infix-lookup id tbl [th (λ() #f)])
  (define o (syntax-e id))
  (if (symbol? o)
      (if tbl (hash-ref tbl o (λ()(syntax-local-value id th)))
          (syntax-local-value id th)) (th)))

(define (infix-local-value id th)
  (infix-lookup id (*infix-local-table*) th))

(define (add-infix-binding s v)
  (define tbl (*infix-local-table*))
  (if tbl (hash-set tbl s v)
      (hash s v)))


(define-syntax-rule (with-infix-binding [s v] body ...)
  (parameterize ([*infix-local-table* (add-infix-binding s v)]) body ...))


;; misc syntax stuff
(define (infix-lookup-syntax s [tbl (infix-local-table)])
  (infix-lookup (maybe-delim s) tbl))

(define (ps-sym s)
  (case s [(#f) '#%parens][(#\[) '#%brackets][(#\{) '#%braces]))

(define (maybe-delim s)
  (if (list? (syntax-e s))
      (datum->syntax
       s (ps-sym (syntax-property s 'paren-shape))) s))
