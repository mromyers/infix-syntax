#lang scribble/manual
@require[@for-label[infix-syntax
                    racket/base]]

@title{infix-syntax}
@author{mrm}

@defmodule[infix-syntax]
The goal of this package is to provide a standard method of integrating
infix expressions into racket macros and languages.



@table-of-contents[]

@; ----------------------------------------------------------------------


@section{Overview}
@subsection{Parsing}
A function that @emph{parses}
looks something like this:
@racketblock[(parsed unparsed -> parsed unparsed)]

For our purposes, something @emph{parsed} is a syntax object corresponding
to a valid racket expression, while something @emph{unparsed} is a list
of syntax objects, with no guarantees that any of them would be valid
racket expressions as-is. We refer to the syntax objects in the unparsed
list as @emph{tokens}.

To be a bit more specific, our pattern usually looks like this:
@racketblock[l (t₀ ... t₁ ...) -> e (t₁ ...)]
That is to say, we take some parsed left-hand expression @racket[l] and
consume some amount of the unparsed right-hand bit to produce some presumably
more complex parsed expression @racket[e]. We then return @racket[e] and the
remaining unconsumed tokens to whatever function called us, hopefully letting
them sort out the rest.

The reason we speak generally of `@emph{a function that parses}' is because
there are quite a lot of them. We associate every token with its own procedure
of that form, to decide how it should be parsed. Thus, the most important
@emph{function that parses} is:

@defproc[#:link-target? #f
         (infix-parse [l syntax?]
                      [in (listof syntax?)]
                      [stop? (any/c -> boolean?)])
         (values syntax? (listof syntax?))]{
... which repeatedly looks up the procedure @racket[f] associated with the first
token @racket[t] in @racket[in], and calls @racket[(f l in)] to get a new parsed
and unparsed bit.

It uses the provided @racket[stop?] function to decide when to finally return,
applying it to the value of @racket[t].

An @racket[l] value of @racket[#f] is used to indicate that there is no current
parsed-expression.
}

@section{Defining and Using Infix Syntax}
@defmodule[infix-syntax/define]
@defform[(define-infix head #:precedence prec
           body ...
           #:expand ex)]{
}

@defform[(define-operator head #:precedence prec
           body ...
           #:expand ex)]{
}

@defform[(#%parse t ...)]{
}

@section{Patterns and Conventions}

@section{Reference}
@subsection{Parsing Syntax}

@defproc[(infix-parse [l (or/c syntax? #f) #f]
                      [in (listof syntax?)]
                      [stop? (any/c -> boolean?)])
         (values syntax? (listof syntax?))]{
  
}

@defproc[(infix-parse-all [l (or/c syntax? #f) #f]
                          [in (listof syntax?)])
         (values syntax? (listof syntax?))]{

  
}

@defproc[(infix-parse/cmp [l  (or/c syntax? #f) #f]
                          [in (listof syntax?)]
                          [R  procedure?]
                          [m  any/c])
         (values syntax? (listof syntax?))]{   
  
}

@subsection{Infix Transformers}
@defthing[prop:infix-procedure struct-type-property?]{

}
@defthing[prop:infix-precedence struct-type-property?]{

}


@defproc[(infix-app [v any/c]
                    [l  (or/c syntax? #f)]
                    [in (listof syntax?)])
         (values syntax? (listof syntax?))]{
}

@defproc[(infix-default [l  (or/c syntax? #f)]
                        [in (listof syntax?)])
         (values syntax? (listof syntax?))]{
  
}                               

@subsection{Default Constructors}

@defproc[(make-infix [proc (syntax? (listof syntax?) ->
                            syntax? (listof syntax?))]
                     [#:precedence prec any/c]
                     [#:expand ex procedure?])
         any/c]{

}

@defproc[(make-operator [#:precedence prec #f]
                        [#:get get procedure?]
                        [#:com com procedure?])
         any/c]{

}


