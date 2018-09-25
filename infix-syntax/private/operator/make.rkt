#lang racket/base
(require "util.rkt" "id-tools.rkt" "../infix.rkt"
         (rename-in "util.rkt"
                    [operator-procedure op-proc]))

(provide make-operator)

(define (make-operator com get [prec #f])
  (let-values ([(proc prec ex)(op->values com get prec)])
    (make-infix-token proc prec ex)))


(define (op->values com get prec)
  (cond [(procedure? com)(proc-op->values com get prec)]
        [else            (  id-op->values com get prec)]))

(define (proc-op->values com get prec)
  (values (op-proc com get prec)
          prec    (com->ex  com)))

(define (id-op->values id get prec)
  (if (list? id)
      (let ([com (apply id-com id)]
            [ex  (apply id-ex  id)])
        (values (op-proc com get prec) prec ex))
      (let ([com (id-com id)][ex (id-ex id)])
        (values (op-proc com get prec) prec ex))))



