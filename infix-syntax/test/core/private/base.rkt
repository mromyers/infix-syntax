#lang racket/base
(require "macro.rkt"
         (for-syntax "util.rkt"))

(define-syntax-rule (def-op name id get prec)
  (def-tok name #:prec prec
    (op-proc (id-com #'id)
             (get prec))
    #:ex (id-ex #'id)))


(def-op mul   *     left-assoc 7)
(def-op div   /     left-assoc 7)

(def-op add   +     left-assoc 6)
(def-op sub   -     left-assoc 6)

(def-op :     cons right-assoc 5)
(def-op eq-op =     left-assoc 4)

(def-op l-and and  right-assoc 3)
(def-op l-or  or    left-assoc 2)


(def-tok #%parens #:prec 9
  (case-lambda
    [(l in)(with-syntax ([f l][(x ...)(map parse-all
                                           (group-comma (car in)))])
             (values #'(f x ...) (cdr in)))]
    [(in)(values (parse-all (car in)) (cdr in))]))

(def-tok #%brackets #:prec 9
  (λ(in)(with-syntax ([(x ...)(let ([a (car in)])
                                (if (null? a) a
                                    (map parse-all
                                         (group-comma (car in)))))])
          (values #'(list x ...) (cdr in)))))



(def-tok \;   #:prec #t
  (λ(l in)
    (with-syntax ([l l][(x ...) (cdr in)])
      (values #'(begin l (#%parse x ...)) '()))))

(def-tok IF   #:prec #t
  (λ(in)
    (let*-values
     ([(b  out)(parse-next (cdr in))]
      [(c1 out)
       (begin (when (or (null? out)
                        (not (free-identifier=?
                              (car out) #'THEN)))
                (error 'bad))
              (parse-next (cdr out)))])
      (if (and (pair? out)
               (free-identifier=? #'ELSE (car out)))
          (let-values ([(c2 out)(parse-next (cdr out))])
            (with-syntax ([b b][c1 c1][c2 c2])
              (values #'(if b c1 c2) out)))
          (with-syntax ([b b][c1 c1])
            (values #'(unless b c1) out)))))
  #:ex (id-ex #'if))

(def-tok THEN #:prec #t
  (λ(l in)(error 'bad)))

(def-tok ELSE #:prec #t
  (λ(l in)(error 'bad)))




(provide (rename-out [add +][mul *][sub -][div /]
                     [eq-op =]
                     [l-and and][l-or or]
                     [IF if][THEN then][ELSE else])
         : #%parens #%brackets
         \;)

