#lang racket
(require "private/test.rkt"
         infix-syntax/define)

(check-expand-once*
 [(define-infix (I x y z w) stuff (and so) on)
  => (define-syntax I
       (infix-lambda (x y z w) stuff (and so) on))]
 [(define-infix (I) stuff)
  => (define-syntax I (infix-lambda () stuff))]
 [(define-infix I  stuff (and so) on)
  => (define-syntax I (infix-lambda stuff (and so) on))])

(check-expand-once*
 [(define-operator (I x y z w) stuff (and so) on)
  => (define-syntax I
       (operator-lambda (x y z w) stuff (and so) on))]
 [(define-operator (I) stuff)
  => (define-syntax I (operator-lambda () stuff))]
 [(define-operator I stuff (and so) on)
  => (define-syntax I (make-operator stuff (and so) on))])
