#lang info
(define collection "infix-syntax")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/infix-syntax.scrbl" (multi-page))))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(mrm))
