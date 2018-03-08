#lang racket/base
(require "private/macro.rkt")

(module arith racket/base
  (require "private/macro.rkt")
  (def-tok add #:prec 4
    (op-proc (id-com #'+)
             (left-assoc 4))
    #:ex (id-ex #'+))
  
  (def-tok sub #:prec 4
    (op-proc (id-com #'-)
             (left-assoc 4))
    #:ex (id-ex #'-))
  
  (def-tok mul #:prec 5
    (op-proc (id-com #'*)
             (left-assoc 5))
    #:ex (id-ex #'*))
  
  (def-tok div #:prec 5
    (op-proc (id-com #'/)
             (left-assoc 5))
    #:ex (id-ex #'/))
  
  (def-tok eq-op #:prec 1
    (op-proc (id-com #'=)
             (left-assoc 1))
    #:ex (id-ex #'=))

  (provide (rename-out [add +]
                       [mul *]
                       [sub -]
                       [div /])))

(require (submod "." arith))

(check-equal?
 ($ Q 1 + 1 * 3)
 '(+ 1 (* 1 3)))



