#lang racket
(require (for-syntax syntax/parse)
         infix-syntax/core
         "infix.rkt")

(provide make-operator operator-lambda)

;; Now, let's make some specialized token types for
;; the most common cases of custom operators.

(define (replace id stx)
  (datum->syntax stx (cons id (cdr (syntax-e stx)))))

(define (id-wrap ctx id . args)
  (datum->syntax ctx (cons id args)))


(struct id-unop (id get)
  #:property prop:infix-procedure
  (λ(self l in)
    (if l (infix-default l in)
        (let ([id (id-unop-id self)][get (id-unop-get self)])
          (with-right [r (get in)] (id-wrap (car in) id r)))))
  #:property prop:procedure
  (λ(self stx)(replace (id-unop-id self) stx)))

(struct com-unop (com get)
  #:property prop:infix-procedure
  (λ(self l in)
    (if l (infix-default l in)
        (let ([com (com-unop-com self)][get (com-unop-get self)])
          (with-right [r (get in)] (com r)))))
  #:property prop:procedure
  (λ(self stx) ((com-unop-com self)(syntax-e stx))))

(struct id-binop (id get prec)
  #:property prop:infix-procedure
  (λ(self l in)
    (let ([id   (id-binop-id   self)]
          [get  (id-binop-get  self)]
          [prec (id-binop-prec self)])
      (with-right [r (get prec in)]
        (if l (id-wrap (car in) id l r)
            (id-wrap (car in) id r)))))
  #:property prop:procedure
  (λ(self stx)(replace (id-binop-id self) stx))
  #:property prop:infix-precedence
  (struct-field-index prec))
  
(struct com-binop (com get prec)
  #:property prop:infix-procedure
  (λ(self l in)
    (let ([com  (com-binop-com  self)]
          [get  (com-binop-get  self)]
          [prec (com-binop-prec self)])
      (with-right [r (get prec in)]
        (if l (com l r) (com r)))))
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
