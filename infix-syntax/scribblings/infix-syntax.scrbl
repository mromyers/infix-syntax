#lang scribble/manual
@require[@for-label[infix-syntax
                    racket/base]]

@title{infix-syntax}
@author{mrm}
@defmodule[infix-syntax]

The goal of this library is to provide a standard method of integrating
infix expressions into racket macros and languages.


@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Parsing}

@subsection{Parsing Functions}
@defproc[(parse-until [l (or/c syntax? #f) #f]
                      [in (listof syntax?)]
                      [stop? (any/c -> boolean?)])
         (values syntax? (listof syntax?))]{

  Repeatedly applies the infix procedure associated with
  the value @racket[v] of the first element of @racket[in]
  until @racket[(stop? v)] is true and @racket[l] is not
  @racket[#f], or @racket[in] is @racket['()].         
  
}

@defproc[(parse-cmp [l  (or/c syntax? #f) #f]
                          [in (listof syntax?)]
                          [R  (any/c any/c -> boolean?)]
                          [m  any/c])
         (values syntax? (listof syntax?))]{
  Repeatedly applies the infix procedure associated with value @racket[v]
  of the first element of @racket[in] until the precedence of @racket[v] is
  less than @racket[m] and @racket[l] is not  @racket[#f], or @racket[in]
  is @racket['()].


}

@defproc[(parse-next [l (or/c syntax? #f) #f]
                     [in (listof syntax?)])
         (values syntax? (listof syntax?))]{

  
}

@defproc[(parse-all [l (or/c syntax? #f) #f]
                    [in (listof syntax?)])
         syntax?]{

}
@subsection{Handling Scope}
@defform[(with-infix-binding [s v] body ...)]{

}

@defproc[(infix-lookup-syntax
           [s syntax?]
           [tbl dictionary? (infix-local-table)])
         any/c]{
  
}
@defproc[(infix-local-table) dictionary?]{
  
}


@section{Infix Tokens}
@subsection{Infix Procedure}
@defthing[prop:infix-procedure struct-type-property?]{

}

@defproc[(infix-app [v  any/c            ]
                    [l  (or/c syntax? #f)]
                    [in (listof syntax?) ])
         (values syntax? (listof syntax?))]{
}

@defproc[(infix-default [l  (or/c syntax? #f)]
                        [in (listof syntax?)])
         (values syntax? (listof syntax?))]{
  
}


@subsection{Infix Precedence}

@defthing[prop:infix-precedence struct-type-property?]{

}
@defproc[(cmp-prec [v  any/c]
                   [R  (any/c any/c -> boolean?)]
                   [n  any/c])
         boolean?]{
  
}

@subsection{Constructing Tokens}

@defproc[(make-infix-token
          [proc procedure?]
          [prec any/c #f]
          [ex   (syntax? -> syntax?) #f])
         any/c?]{
         
}

@defform*[((infix-token infix-opt ...+)
           (infix-lambda head prec-opt body ...+ ex-opt))
          #:grammar
         [(infix-arg prec-arg proc ex-arg)
          (prec-arg (code:line)
                    (code:line #:prec prec)
                    (code:line #:precedence prec))
          (ex-arg   (code:line)
                    (code:line #:ex ex)
                    (code:line #:expand ex))]]{

}

@defform[(infix-lambda head prec-opt body ...+ ex-opt)]{
  
}


@section{Operators}
@defform[(with-right [id get-expr] com-expr)]{
  
}

@defproc[(operator-procedure [com (or/c procedure? identifier?)]
                             [get (or/c procedure? symbol?)]
                             [prec any/c #f])
         procedure?]{
         
}

@defproc[(id-com [id identifier?]
                 [l? (or/c boolean? '?) '?]
                 [r? (or/c boolean? '?) '?])
         procedure?]{

}

@defproc*[([((get-cmp [R procedure?])
             [n any/c]) procedure?]
           [((get-cmp [R procedure?])
             [n any/c]
             [in (listof syntax?)])             
            (values syntax? (listof syntax?))])]{

}

@defproc*[([(right-assoc [n any/c]) procedure?]
           [(right-assoc [n any/c] 
                         [in (listof syntax?)])
            (values syntax? (listof syntax?))])]{

}
@defproc*[([(left-assoc [n any/c]) procedure?]
           [(left-assoc [n any/c]
                        [in (listof syntax?)])
            (values syntax? (listof syntax?))])]{

}
@defproc[(get-none [in (listof syntax?)])
         (values #f (listof syntax?))]{

}

@defproc[(make-operator [com (or/c procedure? identifier?)]
                        [get (or/c procedure? symbol?)]
                        [prec any/c #f])
         any/c]{
  
}





@;------------------------------------------------------------------------

@bibliography[
  @bib-entry[
    #:key "Pratt73"
    #:title "Top Down Operator Precedence"
    #:author "Vaughan Pratt"
    #:location @italic{Principles of Programming Languages}
    #:date "1973"
  ]
  @bib-entry[
    #:key "Crockford07"
    #:title "Top Down Operator Precedence"
    #:author "Douglas Crockford"
    #:date "2007"
    #:url "http://crockford.com/javascript/tdop/tdop.html"
  ]
  @bib-entry[
    #:key "Rafkind12"
    #:title "Honu: A Syntactically Extensible Language"
    #:author "Jon Rafkind"
    #:date "2012"
    #:url "https://www.cs.utah.edu/~rafkind/papers/honu-2012.pdf"
  ]
]
