#lang racket/base
(require "private/macro.rkt"
         "private/base.rkt")


(check-equal?
 [$ Q f(x,y,z,w) = x + y * z - w]
 '(= (f x y z w) (- (+ x (* y z)) w)))

;; Handles dangling else
(check-equal?
 [$ Q if a then
        if b 
        then c
        else d
      else e]
 '(if a (if b c d) e))
