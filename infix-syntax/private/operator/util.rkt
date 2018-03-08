#lang racket/base
(require (rename-in racket/base
                    [procedure-arity-includes? pai?])
         "../core.rkt")

(provide with-right com->ex     get-cmp
         left-assoc right-assoc get-none
         operator-procedure)

(define-syntax-rule (with-right [r get] com)
  (let-values ([(r out) get])(values com out)))

(define (get-cmp R)
  (case-lambda
    [(n    in)(parse-cmp in R n) ]
    [(n)(λ(in)(parse-cmp in R n))]))

(define left-assoc      (get-cmp <=))
(define right-assoc     (get-cmp < ))
(define (get-none in) (values #f in))

(define ((com->ex com) stx)
  (let ([lst (syntax-e stx)])(apply com lst)))

(define (operator-procedure com get [prec #f])
  (if (symbol? get)
      (case get
        [(right)(op-proc com right-assoc prec)]
        [(left) (op-proc com  left-assoc prec)]
        [(postfix)(pfop-proc com)]
        [(nilfix)(nilop-proc com)]
        [else (error 'BAD)])
      (op-proc com get prec)))

(define (op-proc com get [prec #f])
  ((if* [(pai? com 3) (pai? get 2)] :b2 :b1 :u2 :u1)
   com get prec))

(define-syntax-rule (if* [x y] a b c d)
  (if x (if y a b)(if y c d)))

(define-syntax-rule (def-wr head r g c)
  (define head (with-right [r g] c)))

(def-wr ((:b2 c g p) l in)
  r (g p (cdr in)) (c (car in) l r))
(def-wr ((:b1 c g p) l in)
  r (g   (cdr in)) (c (car in) l r))
(def-wr ((:u2 c g p)   in)
  r (g p (cdr in)) (c (car in)   r))
(def-wr ((:u1 c g p)   in)
  r (g   (cdr in)) (c (car in)   r))

(define-syntax-rule (nr-λ com (l ...) (r ...))
  (λ(l ... in)(values (com (car in) l ... r ...) (cdr in))))

(define (pfop-proc com)
  (cond [(pai? com 3) (nr-λ com (l) (#f))]
        [(pai? com 2) (nr-λ com (l) (  ))]
        [else         (error "Damn.")]))
(define (nilop-proc com)
  (cond [(pai? com 3) (nr-λ com ( ) (#f))]
        [(pai? com 2) (nr-λ com ( ) (  ))]
        [else         (error "Damn.")]))
