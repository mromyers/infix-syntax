#lang racket/base
(require (rename-in racket/base
                    [datum->syntax dtm->stx]))
(provide id-com id-ex replace-id add-id)

;; #t #t - strict binary
;; #t #f - strict postfix
;; #f #t - strict unary
;; #f #f - strict nilfix

;; '? #t - binary  or unary
;; #t '? - binary  or postfix
;; '? #f - postfix or nilfix
;; #f '? - unary   or nilfix

;; '? '? - all

(define id-com
  (case-lambda
    [(id) (id-com:?? id)]
    [(id l?) (arg-if l? (error 'bad)
                     (id-com:t? id)
                     (id-com:?? id)
                     (id-com:f? id))]
    [(id l? r?)
     (arg-if r? (error 'bad)
             (arg-if l? (error 'bad)
                     (id-com:tt id)
                     (id-com:?t id)
                     (id-com:ft id))
             (id-com id l?)
             (arg-if l? (error 'bad)
                     (id-com:tf id)
                     (id-com:?f id)
                     (id-com:ff id)))]))

(define-syntax-rule (arg-if p e t m f)
  (if p (cond [(eq? #t p) t]
              [(eq? '? p) m]
              [else e]) f))
;; Cases
(define ((id-com:tt id) o l r)
  (if (and l r) (do-id/b id o l r)
      (let ([lt (if l ok-tag bad-tag)]
            [rt (if r ok-tag bad-tag)])
        (do-bad o id lt l rt r))))
(define ((id-com:tf id) o l r)
  (if (and l (not r)) (do-id/b id o l r)
      (let ([lt (if l  ok-tag bad-tag)]
            [rt (if r bad-tag  ok-tag)])
        (do-bad o id lt l rt r))))
(define ((id-com:ft id) o   r)
  (if r (do-id/u id o r)
      (let ([rt (if r ok-tag bad-tag)])
        (do-bad o id ok-tag #f rt r))))
(define ((id-com:ff id) o   r)
  (if r (do-bad o id ok-tag #f bad-tag r)
      (do-id/u id o r)))

(define ((id-com:t? id) o l r)
  (if l (do-id/b id o l r)
      (do-bad o id bad-tag l ok-tag r)))
(define ((id-com:?t id) o l r)
  (if r (do-id/b id o l r)
      (do-bad o id ok-tag l bad-tag r)))
(define ((id-com:?f id) o l r)
  (if r (do-bad o id ok-tag l bad-tag r)
      (do-id/b id o l r)))

(define ((id-com:f? id) o   r) (do-id/u id o   r))
(define ((id-com:?? id) o l r) (do-id/b id o l r))

;; Utils
(define (do-id/b id o l r)
  (dtm->stx o (cons id (?list l r))))
(define (do-id/u id o   r)
  (dtm->stx o (cons id (?list   r))))

(define (do-bad o id lt l rt r)
  (dtm->stx o (list (dtm->stx o '#%bad-op) id
                    (dtm->stx o lt)
                    (dtm->stx o (?list l))
                    (dtm->stx o rt)
                    (dtm->stx o (?list r)))))

(define bad-tag '#:bad)
(define  ok-tag '#:ok )

#;
(define (bad-tag s o)(dtm->stx o '#:bad) #;
  (dtm->stx o (cons (?list s))))
#;
(define  (ok-tag s o)(dtm->stx o '#:ok)   #;
  (dtm->stx o (?list s)))

(define (?cons x y)(if x (cons x y) y))
(define-syntax ?list
  (syntax-rules ()
    [(_ x y ...)(?cons x (?list y ...))][(_) '()]))

(define id-ex
  (case-lambda
    [(id) (λ(stx)(replace-id id stx))]
    [(id l?   ) (id-ex id)]
    [(id l? r?) (id-ex id)]))

(define (replace-id id stx)
  (let ([stx-e (syntax-e stx)])
    (cond [(pair? stx-e)(datum->syntax stx (cons id (cdr stx-e)))]
          [(symbol? stx-e) id])))

(define add-id
  (case-lambda
    [(o id    )(dtm->stx o (list id            ))]
    [(o id   r)(dtm->stx o (cons id (?list   r)))]
    [(o id l r)(dtm->stx o (cons id (?list l r)))]))
