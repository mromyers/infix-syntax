#lang racket/base
(require infix-syntax/private/operator
         rackunit)

(define (chk-stx x y)
  (check-equal? (syntax->datum x) y))
(define (chk-arity f a)
  (check-equal? (procedure-arity f) a))

(define-values (id o l r)
  (values #'id #'o #'l #'r))


(chk-stx (replace-id #'x #'(f z)) '(x z))

(let ([ex (id-ex #'x)])
  (chk-stx (ex #'(f z)) '(x z))
  (chk-stx (ex #'f)     ' x   ))

(chk-stx (add-id o id  l  r) '(id l r))
(chk-stx (add-id o id  l #f) '(id l  ))
(chk-stx (add-id o id #f  r) '(id   r))
(chk-stx (add-id o id #f #f) '(id    ))


(define-syntax cc-aux
  (syntax-rules ()
    [(_ com t f)(begin (chk-stx (com o  r) 't)
                       (chk-stx (com o #f) 'f))]
    [(_ com tt tf ft ff)
     (begin (chk-stx (com o  l   r) 'tt)
            (chk-stx (com o  l  #f) 'tf)
            (chk-stx (com o #f   r) 'ft)
            (chk-stx (com o #f  #f) 'ff))]))

(define-syntax-rule (chk-com [b ...] arity x ...)
  (let ([com (id-com id b ...)])
    (chk-arity com arity)
    (cc-aux com x ...)))

;; ========================================
(chk-com [     ] 3
         (         id        l         r )
         (         id        l           )
         (         id                  r )
         (         id                    ))
;; ========================================
(chk-com [#t   ] 3
         (         id        l         r )
         (         id        l           )
         (#%bad-op id #:bad ( ) #:ok  (r))
         (#%bad-op id #:bad ( ) #:ok  ( )))
(chk-com ['?   ] 3
         (         id        l         r )
         (         id        l           )
         (         id                  r )
         (         id                    ))
(chk-com [#f   ] 2
         (         id                   r )
         (         id                     ))
;; ========================================
(chk-com [#t #t] 3
         (         id        l         r )
         (#%bad-op id #:ok  (l) #:bad ( ))
         (#%bad-op id #:bad ( ) #:ok  (r))
         (#%bad-op id #:bad ( ) #:bad ( )))
(chk-com [#t '?] 3
         (         id        l         r )
         (         id        l           )
         (#%bad-op id #:bad ( ) #:ok  (r))
         (#%bad-op id #:bad ( ) #:ok  ( )))
(chk-com [#t #f] 3
         (#%bad-op id #:ok  (l) #:bad (r))
         (         id        l           )
         (#%bad-op id #:bad ( ) #:bad (r))
         (#%bad-op id #:bad ( ) #:ok  ( )))
;; ========================================
(chk-com ['? #t] 3
         (         id        l         r )
         (#%bad-op id #:ok  (l) #:bad ( ))
         (         id                  r )
         (#%bad-op id #:ok  ( ) #:bad ( )))
(chk-com ['? '?] 3
         (         id        l         r )
         (         id        l           )
         (         id                  r )
         (         id                    ))
(chk-com ['? #f] 3
         (#%bad-op id #:ok  (l) #:bad (r))
         (         id        l           )
         (#%bad-op id #:ok  ( ) #:bad (r))
         (         id                    ))
;; ========================================
(chk-com [#f #t] 2
         (         id                   r )
         (#%bad-op id #:ok   ( ) #:bad ( )))
(chk-com [#f '?] 2
         (         id                   r )
         (         id                     ))
(chk-com [#f #f] 2
         (#%bad-op id #:ok   ( ) #:bad (r))
         (         id                     ))




