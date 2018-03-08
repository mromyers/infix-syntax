#lang racket/base
(provide reprovide)
(define-syntax-rule (reprovide s ...)
  (begin (require s ...)(provide (all-from-out s ...))))
