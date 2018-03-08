#lang info
(define collection "infix-syntax")
(define deps '("base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings
    '(("scribblings/infix-syntax.scrbl" (multi-page))))
