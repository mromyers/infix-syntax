#lang racket/base
(require "reprovide.rkt")
(reprovide
 ;; prop:infix-procedure  infix-app   infix-default
 ;; prop:infix-precedence infix-precedence cmp-prec 
 "core/props.rkt"
 
 ;; with-infix-binding infix-lookup
 "core/scope.rkt"
 
 ;; parse-until parse-cmp 
 ;; parse-next  parse-all
 "core/parse.rkt")

