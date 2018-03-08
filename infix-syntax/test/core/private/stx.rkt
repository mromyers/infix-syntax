#lang racket/base
(require infix-syntax/private/core)
(provide (all-from-out infix-syntax/private/core))

(provide mk-tok make-tok
         split-proc op-proc
         with-right ?list
         id-com id-ex replace-id
         left-assoc right-assoc get-none)

(struct tok (proc prec)
  #:property prop:infix-procedure  0
  #:property prop:infix-precedence 1)
(struct tok+ tok (ex)
  #:property prop:procedure
  (struct-field-index ex))

(define (mk-tok proc prec [ex #f])
  (if ex (tok+ proc prec ex) (tok proc prec)))

(define (make-tok proc #:prec [n #f] #:ex [ex #f])
  (mk-tok proc n ex))


(define ((split-proc bin un) l in)
  (if l (bin l in) (un l in)))

(define-syntax-rule (with-right [r get] com)
  (let-values ([(r out) get]) (values com out)))

(define ((op-proc com get) l in)
  (with-right [r (get (cdr in))]
    (com (car in) l r)))

(define  ((left-assoc n) in)(parse-cmp in <= n))
(define ((right-assoc n) in)(parse-cmp in <  n))
(define (get-none in)(values #f in))

(provide ?list split-proc)
(define (?cons x y)(if x (cons x y) y))
(define-syntax ?list
  (syntax-rules ()
    [(_ x y ...)(?cons x (?list y ...))][(_) '()]))

(define ((id-com id) o l r)
  (datum->syntax o (cons id (?list l r))))

(define (replace-id id stx)
  (let ([stx-e (syntax-e stx)])
    (cond [(pair? stx-e)(datum->syntax stx (cons id (cdr stx-e)))]
          [(symbol? stx-e) id])))

(define ((id-ex id) stx) (replace-id id stx))
