#lang racket/base
(provide with-infix-binding infix-lookup)

;; Local state
(define *infix-local-table* (hash))

(define (bind-in! k v)
  (set! *infix-local-table*
        (hash-set *infix-local-table* k v)))

(define (call-with-bind body k v)
  (define old *infix-local-table*)
  (dynamic-wind
    (λ()(bind-in! k v)) body
    (λ()(set! *infix-local-table* old))))

(define (table-lookup sym th)
  (hash-ref *infix-local-table* sym th))

;; Sugar
(define-syntax-rule
  (with-infix-binding [s v] body ...)
  (call-with-bind (λ() body ...) s v))

(define (infix-lookup stx)  
  (define o (syntax-e stx))
  (cond [(symbol? o)(raw-lookup stx o)]
        [(list? o)
         (let* ([sym (paren-shape stx)]
                [stx (datum->syntax stx sym)])
           (raw-lookup stx sym))]
        [else #f]))

(define (raw-lookup stx sym)
  (table-lookup sym (λ()(syntax-local-value
                         stx (λ() #f)))))

(define (paren-shape stx)
  (case (syntax-property stx 'paren-shape)
    [(#f)  '#%parens  ]
    [(#\[) '#%brackets]
    [(#\{) '#%braces  ]))
