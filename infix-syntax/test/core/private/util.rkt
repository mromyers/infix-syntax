#lang racket/base

(define (reverse* lst rst)
  ;; The same as (append (reverse lst) rst)
  (let loop ([rev rst][lst lst])
    (if (null? lst) rev
        (loop (cons (car lst) rev)
              (cdr lst)))))

(define (group-left lst chk)
  (let loop ([rev0 '()][rev '()][lst lst])
    (if (null? lst)(reverse* rev0 (list (reverse rev)))
        (let* ([a (car lst)][a* (chk a)])
          (if a*
              (loop (cons (reverse rev) rev0)
                    (if (list? a*) a*
                        (list (if (equal? a* #t) a a*)))
                    (cdr lst))
              (loop rev0 (cons a rev) (cdr lst)))))))

(define (group-right lst chk)
  (let loop ([rev0 '()][rev '()][lst lst])
    (if (null? lst)
        (if (null? rev)(reverse rev0)
            (reverse* rev0 (list (reverse rev))))
        (let* ([a (car lst)][a* (chk a)])
          (if a*
              (loop (cons (if (list? a*) (append a* (reverse rev))
                              (cons (if (equal? a* #t) a a*)
                                    (reverse rev))) rev0)
                    '() (cdr lst))
              (loop rev0 (cons a rev) (cdr lst)))))))

;; Actual Parsing
(define (chk-comma x)
  (define e (syntax-e x))  
  (if (equal? e '|,|) '()
      (if (and (pair? e) (equal? (syntax-e (car e)) 'unquote))
          (cdr e) #f)))

(define (group-comma stx)
  (define in (syntax-e stx))
  (map (λ(x)(datum->syntax stx x))
       (group-left in chk-comma)))
  


(provide group-left group-right group-comma)
