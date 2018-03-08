#lang racket/base
(require infix-syntax/private/reprovide)
(reprovide infix-syntax/private/core
           infix-syntax/private/infix
           infix-syntax/private/operator)

(provide (rename-out [operator-procedure op-proc]))
