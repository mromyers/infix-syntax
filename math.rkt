#lang racket/base
(provide (rename-out [add +]
                     [mul *]
                     [sub -]
                     [div /]
                     [pow ^]
                     [eq-op =]
                     [geq-op >=]
                     [leq-op <=]
                     [gt-op >]
                     [le-op <]
                     [#%parse $])
         #%fn-application #%parens)

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
(def-op div / 5 left-assoc)
(def-op pow expt 6 right-assoc)
(def-op eq-op = 1 right-assoc)
(def-op geq-op >= 1 right-assoc)
(def-op leq-op <= 1 right-assoc)
(def-op gt-op > 1 right-assoc)
(def-op le-op < 1 right-assoc)

(define-infix (#%fn-application l in)
  #:precedence 9
  (with-right [r (right-assoc 9 in)]
    #:syntax (l r)
    #'(#%fn-application l r))
  #:expand
  (λ (stx)
    (syntax-case stx (#%fn-application)
      [(_ (#%fn-application x ...) y ...)
       #'(#%fn-application x ... y ...)]
      [(_ x ...) #'(x ...)])))

(define-infix (#%parens head rest)
  (if head (infix-default head rest)
      (with-syntax ([(x ...) (car rest)])
        (values #'(#%parse x ...) (cdr rest)))))
