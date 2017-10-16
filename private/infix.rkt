#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "parse.rkt" "prec.rkt")
(provide make-infix infix-lambda)

(struct _i_:token (proc)
  #:property prop:infix-procedure
  (struct-field-index proc))

(struct ei_:token (ex proc)
  #:property prop:procedure (struct-field-index ex)
  #:property prop:infix-procedure
  (struct-field-index proc))
(struct _ip:token _i_:token (prec)
  #:property prop:infix-precedence
  (struct-field-index prec))
(struct eip:token ei_:token (prec)
  #:property prop:infix-precedence
  (struct-field-index prec))

(define *none* (gensym))
(define (none? o) (equal? o *none*))

(define (:infix proc ex prec)
  (if (none? ex)
      (cond[(none? prec) (_i_:token    proc     )]
           [else         (_ip:token    proc prec)])
      (cond[(none? prec) (ei_:token ex proc     )]
           [else         (eip:token ex proc prec)])))


(define (make-infix proc
                    #:precedence [prec *none*]
                    #:expand     [ex   *none*])
  (:infix proc ex prec))

(begin-for-syntax
  (define-splicing-syntax-class prec-opt
    [pattern (~seq (~or #:prec
                        #:precedence) prec:expr)
             #:with (arg ...) #'(#:precedence prec)]
    [pattern (~seq) #:with (arg ...) #'()])
  (define-splicing-syntax-class ex-opt
    [pattern (~seq #:expand prec:expr)
             #:with (arg ...) #'(#:expand prec)]
    [pattern (~seq) #:with (arg ...) #'()]))

(define-syntax (infix-lambda stx)
  (syntax-parse stx
    [(_ head:expr prec:prec-opt body:expr ... ex:ex-opt)
     #'(make-infix prec.arg ... (λ head body ...) ex.arg ...)]))


