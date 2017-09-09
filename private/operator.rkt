#lang racket
(require (for-syntax syntax/parse)
         infix-syntax/core
         "infix.rkt")

(provide with-get
         make-operator operator-lambda
         left-assoc right-assoc postfix)


(define-syntax (with-stx stx)
  (define (foo stx)
    (map (λ(s)(if (identifier? s)(with-syntax ([s s])
                                   #'[s s])
                  s))
         (syntax-e stx)))
  (syntax-parse stx
    [(_ (s ...) body ...)
     (with-syntax ([(s* ...)(foo #'(s ...))])
       #'(with-syntax (s* ...)
           body ...))]))

(define-syntax with-get
  (syntax-rules ()
    [(_ [r get] com)
     (let-values ([(r out) get])
       (values com out))]
    [(_ [r get]
        #:syntax (b ...)
        com)
     (let-values ([(r out) get])
       (with-stx (b ...)
         (values com out)))]))

;; Now, let's make some specialized token types for
;; the most common cases of custom operators.
(define (d-ctx id lst)
  (if (null? lst) id (car lst)))

(define (id+ id lst [ctx (d-ctx id lst)])
  (datum->syntax ctx (cons id lst)))




(struct id-unop (id get)
  #:property prop:infix-procedure
  (λ(self l in)
    (if l (infix-default l in)
        (let ([id (id-unop-id self)][get (id-unop-get self)])
          (with-get [es (get in)]
            (id+ id es)))))
  #:property prop:procedure
  (λ(self stx)
    (id+ (id-unop-id self) (cdr (syntax-e stx)) stx)))

(struct com-unop (com get)
  #:property prop:infix-procedure
  (λ(self l in)
    (if l (infix-default l in)
        (let ([com (com-unop-com self)][get (com-unop-get self)])
          (with-get [es (get in)]
            (com es)))))
  #:property prop:procedure
  (λ(self stx)
    ((com-unop-com self)(syntax-e stx))))

(struct id-binop (id get prec)
  #:property prop:infix-procedure
  (λ(self l in)
    (let ([id   (id-binop-id   self)]
          [get  (id-binop-get  self)]
          [prec (id-binop-prec self)])
      (with-get [rs (get prec in)]
          (if l (id+ id (cons l rs))
              (id+ id rs)))))
  #:property prop:procedure
  (λ(self stx)
    (id+ (id-binop-id self) (cdr (syntax-e stx)) stx))
  #:property prop:infix-precedence
  (struct-field-index prec))
  
(struct com-binop (com get prec)
  #:property prop:infix-procedure
  (λ(self l in)
    (let ([com  (com-binop-com   self)]
          [get  (com-binop-get  self)]
          [prec (com-binop-prec self)])
      (with-get [rs (get prec in)]
        (if l (com l rs) (com rs)))))
  #:property prop:procedure
  (λ(self stx)
    (define lst (syntax-e stx))
    (define com (com-binop-com self))
    (if (null? lst) (com '())
        (com (car lst) (cdr lst))))
  #:property prop:infix-precedence
  (struct-field-index prec))

(define *none* (gensym))

(define (make-operator #:get get
                       #:com com
                       #:precedence [prec *none*])
  (if (equal? *none* prec)
      (if (identifier? com) (id-unop com get)
          (com-unop com get))
      (if (identifier? com) (id-binop com get prec)
          (com-binop com get prec))))


(define-syntax (operator-lambda stx)
  (syntax-parse stx
    [(_ (l:id r:id) #:precedence prec:expr
        #:get get:expr
        body:expr ...)
     #'(com-binop (λ(l r) body ...) get prec)]
    [(_ (r:id) #:get get:expr
        body ...)
     #'(com-unop (λ(r) body ...) get)]))


(define left-assoc
  (case-lambda
    [(prec in)
     (let-values ([(r out)(infix-parse/cmp #f (cdr in) < prec)])
       (values (list r) out))]
    [(prec)
     (λ(in)(right-assoc prec in))]))

(define right-assoc
  (case-lambda
    [(prec in)
     (let-values ([(r out)(infix-parse/cmp #f (cdr in) <= prec)])
       (values (list r) out))]
    [(prec)
     (λ(in)(right-assoc prec in))]))

(define (postfix prec in)
  (values '() (cdr in)))
