#lang racket/base
(provide prop:infix-procedure infix-token?
         infix-app
         
         infix-default jx-cons
         
         with-infix-binding infix-local-value
         infix-parse infix-parse-all)


;; Infix Procedures
(define-values [prop:infix-procedure
                infix-token?
                infix-proc]
  (make-struct-type-property 'infix-procedure
    (λ(proc lst)
      (cond [(exact-nonnegative-integer? proc)
             (define proc-ref (make-struct-field-accessor
                               (list-ref lst 3) proc))
             (λ(self l in)((proc-ref self) l in))]
            [else proc]))))

(define (infix-app v l in)
  (if (infix-token? v)
      ((infix-proc v) v l in)
      (infix-default l in)))

(define (infix-default l in)
  (if l (values l (jx-cons in))
      (values (car in) (cdr in))))

(define (jx-cons in)
  (let ([a (car in)])
    (if (equal? (syntax-e a) '#%jx)
        (raise-syntax-error #f "#%jx undefined")
        (cons (datum->syntax a '#%jx) in))))


;; Local Scope
(define *infix-local-table* (make-parameter #f))

(define (infix-lookup id tbl [th (λ() #f)])
  (define o (syntax-e id))
  (if (symbol? o)
      (if tbl (hash-ref tbl o (λ()(syntax-local-value id th)))
          (syntax-local-value id th)) (th)))

(define (infix-local-value id th)
  (infix-lookup id (*infix-local-table*) th))

(define (add-infix-binding s v)
  (define tbl (*infix-local-table*))
  (if tbl (hash s v)
      (hash-set tbl s v)))

(define-syntax-rule (with-infix-binding [s v] body ...)
  (parameterize ([*infix-local-table* (add-infix-binding s v)]) body ...))


;; misc syntax stuff
(define (lookup-syntax s tbl)
  (infix-lookup (maybe-delim s) tbl))

(define (maybe-delim s)
  (if (list? (syntax-e s))
      (datum->syntax
       s (case (syntax-property s 'paren-shape)
           [(#f) par][(#\[) brk][(#\{) brc])) s))

(define-values (par brk brc)
  (values '#%parens '#%brackets '#%braces))

(define (stx?-e o)
  (if (syntax? o) (syntax-e o) o))


;; Actual Parsing
(define (do-parse l in₀ stop?)
  (define tbl (*infix-local-table*))
  (let parse ([l l][in (stx?-e in₀)])
    (if (null? in) (values l in)
        (let* ([rs (car in)][rv (lookup-syntax rs tbl)])
          (if (and l (stop? rs rv)) (values l in)
              (let-values ([(l+ out)(infix-app rv l in)])
                (when (eq? in out) (bad rs))
                (parse l+ out)))))))

(define (bad s)
  (raise-syntax-error
   (format "Infinite loop: blame ~a.\n"
           (syntax->datum s))))


;; Nicer interface
(define infix-parse
  ;; Pretend this reads as (infix-parse [l #f] in stop?)
  ;; It should be an optional argument, but it makes more
  ;; sense if it's on the left.
  (case-lambda
    [(l in stop?)(do-parse  l in stop?)]
    [(  in stop?)(do-parse #f in stop?)]))

(define infix-parse-all
  ;; Pretend this reads as (infix-parse-all [l #f] in).
  (case-lambda
    [(e in) (let-values ([(e* out) (infix-parse e in no?)]) e*)]    
    [(  in) (let-values ([(e* out) (infix-parse   in no?)]) e*)]))

(define (no? s v) #f)
