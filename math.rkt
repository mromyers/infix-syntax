#lang racket/base
(require infix-syntax/define
         (rename-in racket/base)
         (for-syntax racket/base
                     infix-syntax))

(define-syntax def-op
  (syntax-rules ()
    [(_ name id prec get)
     (define-operator name #:com #'id #:precedence prec #:get get)]
    [(_ name id get)
     (define-operator name #:com #'id #:get get)]))


(def-op add + 4 left-assoc)
(def-op sub - 4 left-assoc)
(def-op mul * 5 left-assoc)
(def-op div / 5 right-assoc)

(def-op eq-op = 1 left-assoc)


(define-infix (#%jx l in)
  #:precedence 9
  (with-right [r (right-assoc 9 in)]
    #:syntax (l r)
    #'(#%jx l r))
  #:expand
  (λ(stx)
    (syntax-case stx (#%jx)
      [(_ (#%jx x ...) y ...)
       #'(#%jx x ... y ...)]
      [(_ x ...) #'(x ...)])))

(define-infix (#%parens l in)
  (if l (infix-default l in)
      (with-syntax ([(x ...)(car in)])
        (values #'(#%parse x ...) (cdr in)))))

(provide (rename-out [add +]
                     [mul *]
                     [sub -]
                     [div /]
                     [eq-op =]
                     [#%parse $])
         #%jx #%parens)
