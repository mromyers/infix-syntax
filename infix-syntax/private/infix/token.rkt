#lang racket/base
(require "../core.rkt"
         (rename-in racket/base
                    [struct-field-index :sfi]))

(provide make-infix-token)

(struct infix-token (proc)
  #:property prop:infix-procedure [:sfi proc]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write-string "#<infix-token>" port))])

(define-syntax-rule
  (extend-token tok mk-tok)
  (begin
    (struct tok:p_ tok (prec   )
      #:property prop:infix-precedence [:sfi prec])
    (struct tok:_e tok (     ex)
      #:property prop:procedure        [:sfi ex  ])
    (struct tok:pe tok (prec ex)
      #:property prop:procedure        [:sfi ex  ])
    (define (mk-tok proc prec ex)
      (cond [ex   (cond [prec (tok:pe proc prec ex)]
                        [else (tok:_e proc      ex)])]
            [else (cond [prec (tok:p_ proc prec   )]
                        [else (tok    proc        )])]))))

;; Yes, this looks silly, but it's for a reason.
(extend-token infix-token make-infix-token)



