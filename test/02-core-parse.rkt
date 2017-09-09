#lang racket
(require (for-syntax racket/base
                     "../private/parse.rkt"
                     "../private/prec.rkt")
         rackunit)

(begin-for-syntax
  (struct tok (proc prec)
    #:property prop:infix-procedure  0
    #:property prop:infix-precedence 1)
  (define (wrap l)(if l (list l) '()))

  (define ((mk-opl-proc s n) l in)
    (let-values ([(r out)(infix-parse/cmp (cdr in) <= n)])
      (with-syntax ([s s][(l ...) (wrap l)][r r])
        (values #'(s l ... r) out))))
  (define ((mk-opr-proc s n) l in)
    (let-values ([(r out)(infix-parse/cmp (cdr in) < n)])
      (with-syntax ([s s][(l ...) (wrap l)][r r])
              (values #'(s l ... r) out))))

  (define ((mk-oppf-proc s n) l in)
    (with-syntax ([s s][(l ...) (wrap l)])
      (values #'(s l ...) (cdr in))))

  (define (mk-op s n d)
    (tok (case d
           [(l)(mk-opl-proc s n)]
           [(r)(mk-opr-proc s n)]
           [(pf)(mk-oppf-proc s n)]) n))

  (define-syntax-rule (op/r s d n)
    (mk-op #'s n (quote d)))

  (define (tag-op s n)
    (tok (λ(l in)
           (with-syntax ([s s][(l ...)(wrap l)][r (car in)])
             (values #'(s l ... . r) (cdr in)))) n))
)

(define-syntax-rule (def-op/r s d n)
  (define-syntax s (mk-op #'s n (quote d))))
(define-syntax-rule (def-tag s n t)
  (define-syntax s (tag-op 't n)))

(define-syntax (qt-p: stx)
  (with-syntax ([stx* (infix-parse-all (cdr (syntax-e stx)))])
    #'(quote stx*)))


(def-op/r ol1 l 1) (def-op/r or1 r 1) (def-op/r opf1 pf 1)
(def-op/r ol2 l 2) (def-op/r or2 r 2) (def-op/r opf2 pf 2)
(def-op/r ol3 l 3) (def-op/r or3 r 3) (def-op/r opf3 pf 3)
(def-op/r ol4 l 4) (def-op/r or4 r 4) (def-op/r opf4 pf 4)
(def-tag #%parens   3 |()|)
(def-tag #%brackets 3 |[]|)
(def-tag #%braces   3 |{}|)
(def-op/r #%jx l 1)

(define-syntax-rule (check-parsed-equal? (unparsed ...) (parsed ...))
  (check-equal? (qt-p: unparsed ...) (quote (parsed ...))))

; sanity check
(check-parsed-equal? [1 ol1  2]   (ol1 1 2))
(check-parsed-equal? [  ol1  2]   (ol1   2))
(check-parsed-equal? [1 opf1  ]  (opf1 1  ))
(check-parsed-equal? [1      2]  (#%jx 1 2))
(check-parsed-equal? [ (1)]      (|()|   1))
(check-parsed-equal? [f(1)]      (|()| f 1))
(check-parsed-equal? [ [1]]      (|[]|   1))
(check-parsed-equal? [f[1]]      (|[]| f 1))
(check-parsed-equal? [ {1}]      (|{}|   1))
(check-parsed-equal? [f{1}]      (|{}| f 1))


; basic precedence
(check-parsed-equal?
 [ 1   ol1    2    ol2   3 ]
 [ 1 . ol1 . (2 .  ol2 . 3)])
(check-parsed-equal?
 [ 1   ol2    2    ol1   3 ]
 [(1 . ol2 .  2) . ol1 . 3 ])

(check-parsed-equal?
 [     ol1    2    ol2   3 ]
 [     ol1   (2 .  ol2 . 3)])
(check-parsed-equal?
 [     ol2    2    ol1   3 ]
 [    (ol2    2) . ol1 . 3 ])

(check-parsed-equal?
 [      1   ol1          2    opf2]
 [      1 . ol1 .  (opf2 2)       ])
(check-parsed-equal?
 [      1   ol2          2    opf1]
 [opf1 (1 . ol2 .        2)       ])
(check-parsed-equal?
 [      1   opf1   ol1   2        ]
 [(opf1 1)       . ol1 . 2        ])


(check-parsed-equal?
 [1   ol1         f (2)]
 [1 . ol1 . (|()| f  2)])
(check-parsed-equal?
 [      f   ol3   g (2)]
 [|()| (f . ol3 . g) 2 ])

(check-parsed-equal?
 [1   ol1         f [2]]
 [1 . ol1 . (|[]| f  2)])
(check-parsed-equal?
 [      f   ol3   g [2]]
 [|[]| (f . ol3 . g) 2 ])
(check-parsed-equal?
 [1   ol1         f {2}]
 [1 . ol1 . (|{}| f  2)])
(check-parsed-equal?
 [      f   ol3   g {2}]
 [|{}| (f . ol3 . g) 2 ])


;; go for broke
(check-parsed-equal?
 [ol1  or4  ol3  or2 1] (ol1 (or4 (ol3 (or2 1)))))

(check-parsed-equal?
 [1 2 3 4 5 6 7]
 (#%jx (#%jx (#%jx (#%jx (#%jx (#%jx 1 2) 3) 4) 5) 6) 7))

(check-parsed-equal?
 [   1   ol1    2    ol1     3     ol1    4    ol1    5   ]
 [(((1 . ol1 .  2) . ol1 .   3) .  ol1 .  4) . ol1 .  5   ])
(check-parsed-equal?
 [   1   ol4    2    ol3     3     ol2    4    ol1    5   ]
 [(((1 . ol4 .  2) . ol3 .   3) .  ol2 .  4) . ol1 .  5   ])

(check-parsed-equal?
 [   1   or1    2    or1     3     or1    4    or1    5   ]
 [   1 . or1 . (2 .  or1 .  (3 .   or1 . (4 .  or1 .  5)))])
(check-parsed-equal?
 [   1   or1    2    or2     3     or3    4    or4    5   ]
 [   1 . or1 . (2 .  or2 .  (3 .   or3 . (4 .  or4 .  5)))])

(check-parsed-equal?
 [   1   ol2    2    or1     3     ol2    4    or1   5    ]
 [  (1 . ol2 .  2) . or1 . ((3 .   ol2 .  4) . or1 . 5)   ])

(check-parsed-equal?
 [   1   ol2    2    or3     3     ol1    4   or4    5    ]
 [  (1 . ol2 . (2 .  or3 .   3)) . ol1 . (4 . or4 .  5)   ])
(check-parsed-equal?
 [   1   ol2    2    or3     3     ol1    4   or4    5    ]
 [  (1 . ol2 . (2 .  or3 .   3)) . ol1 . (4 . or4 .  5)   ])


