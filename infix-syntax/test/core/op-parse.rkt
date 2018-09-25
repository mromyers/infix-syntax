#lang racket/base
(require "private/macro.rkt")

;; Define a bunch of operators
(def-op ol1 l 1) (def-op or1 r 1) (def-op opf1 pf 1)
(def-op ol2 l 2) (def-op or2 r 2) (def-op opf2 pf 2)
(def-op ol3 l 3) (def-op or3 r 3) (def-op opf3 pf 3)
(def-op ol4 l 4) (def-op or4 r 4) (def-op opf4 pf 4)
(def-tag #%parens   3 |()|) (def-tag #%brackets 3 |[]|)
(def-tag #%braces   3 |{}|) (def-op #%jx l 1)
 
;; Special operator to test binding
(def-tok a=> #:prec 3
  (op-proc (λ(o l r)(datum->syntax
                     o (cons #'a=> (?list r))))
           (λ(in)(with-infix-binding ['a #f]
                   (parse-cmp in < 3)))))
(def-op a l 1)

;; = BEGIN TESTING =

;; Sanity Check
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

(check-parsed-equal?
 [a=> a ol3 a] (a=> (ol3 a a)))

;; Basic Precedence
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

(check-parsed-equal?
 [  a=> a    ol2   b    a   c ]
 [((a=> a) . ol2 . b) . a . c ])
(check-parsed-equal?
 [ a=>  a   ol3   a   ol1  a 1 ]
 [(a=> (a . ol3 . a)) . ol1 . (a 1)])


;; Go For Broke Mate
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
