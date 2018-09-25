# Internals
Tinkering around with the internals of this library has been kind of
messy for a while, primarily because of how mutually dependent its
various parts are. The current organization might seem kind of over
the top, introducing all these layers and heirarchy for a relatively
small amount of code, but it currently does serve a couple uses. In
the present state of this library, I find myself extremely confident
in its internal logic and algorithms, but hopelessly dithering back
and forth with regard to the interface it presents. The current setup
is geared around making it as easy as possible for me to make small
changes to the latter without it having unanticipated consequences for
the former.

## Core
This is the minimum set of functionality needed to build either infix
transformers that can cooperate with the provided parse functions, or
to parse syntax containing infix-transformers.

This provides:

1. Support for the key struct type properties for infix transformers, 
   namely `prop:infix-procedure` and `prop:infix-precedence`.
2. Functions/macros for handling parse-time bindings, such as for 
   lambda forms.
3. The core parsing functions: `parse-until` `parse-cmp` `parse-next` 
   and `parse-all`.

This does *not* provide any structures instantiating the given properties.

## Infix
Provides a nice default infix transformer struct with subtypes allowing 
for optional precedence and/or `prop:procedure` to allow it to double as
a standard transformer. 

## Operator
This provides
1. A set of combinators and special forms for expressing the 
   'operator pattern'.
2. Some constructors intended to cover the most common cases of operators
   (unary postfix left/right associativity, etc).

