#lang scribble/manual

@(require scriblib/footnote)
@(require "../scribbleUtilities.rkt")

@title[#:version ""]{Pattern Macroing}

So far, you have "penned your thoughts within nests of parentheses", and
perhaps @emph{Lots of Irritating Superflous Parentheses} @note{"We toast the
Lisp programmer who pens his thoughts within nests of parentheses.", Alan J.
Perlis,
@hyperlink["http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-5.html#%_chap_Temp_2"]{Foreword}
of @hyperlink["http://mitpress.mit.edu/sicp/"]{Structure and Interpretations
of Computer Programs}. @(linebreak) @(linebreak) "Lots of Irritating
Superflous Parentheses", a wild nickname for LISP which actually abbreviates
@emph{LISt Processing},
@hyperlink["http://www.catb.org/~esr/jargon/html/L/LISP.html"]{The Jargon
File-LISP}}.  The essense of the Racket syntax is that every compound
expression should be enclosed by a pair of parentheses @litchar{(}
@litchar{)}.  Not many people have a sense of the simplicity of this uniform
syntax.  In one way, the syntax indeed looks confusing.  Not everything
appearing in the operator position is a function.  Instead it may be one of
the so-called @emph{special forms}, such as @racket[lambda], @racket[if],
@racket[let], @racket[cond], @racket[and], @racket[or], and so on.  Moreover,
it seems some special forms like @racket[if], @racket[and] and @racket[or]
should be functions but somehow are not.  All these mysteries will be
dispelled in this lecture.  In particular, we will introduce to you another
abstraction mechanism, macros.  In contrast to functions which abstract over
@emph{computations}, macros abstract over @emph{code} that @emph{describes}
some computations.  This new abstraction mechanism allows us to avoid code
repetition, to abbreviate common coding patterns @note{@(linebreak) Be aware
that "pattern" here has little to with @emph{data structures}.  If it does
with any structure, then say, @emph{code structure}.}, and to extend the
language syntax.  All these possibilities make the language and eventually
ourselves more expressive, since we can write @emph{less} but say @emph{more}.
Unfortunately, those people who failed to sense the simplicity of the uniform
syntax usually cannot also appreciate the power of macros.  In a large degree,
the extremely regular syntax facilitates the work of macros, that is,
manipulating @emph{code}, though @emph{as data}.

@section{Code and Data}

A program is composed of code and data.  Code @emph{expresses how} to compute.
Data @emph{expresses what} to compute.  For example, code @racket[(define (inc
x) (+ x 1))] expresses a function definition, and @racket[(inc 2)] expresses a
function application; data @racket[3] expresses a number, and @racket[(list 1
2 3)] expresses a list of three numbers.  Racket, as with other Lisp-family
languages, are expressions-oriented.  Thus both code and data are expressions.
Many languages maintain a clear distinction between code and data in syntax.
Racket, and its Lisp relatives, blur the boundaries between the two so that
all expressions are written in a uniform syntax.  Expressions "in this
uniform" are called @emph{s-expressions}.

@subsection{S-expressions}
The name "s-expression" stands for @emph{symbolic expression} for some
historical reasons.  It was invented in the birth of the Lisp programming
language.  Originally it was used only as a notation for data.  Later it was
also adopted for code.  Since then, the Lisp family of languages maintain this
tradition of using the same notation for both code and data.  An s-expression
is defined @emph{inductively} (or @emph{recursively}) as

@itemlist[#:style 'ordered
@item{a literal: @racket[true], @racket[false], @racket[0], @racket[1],
      @racket[0.618], @racket[3.14], @racket["hello"], @racket["world"], ...,
      or}
@item{an identifier: @racket[define], @racket[if], @racket[struct],
      @racket[+], @racket[empty], @racket[cons], @racket[posn],
      @racket[posn-x], @racket[double], @racket[iamavariable], ..., or}
@item{the terminator: @litchar{()}, or}
@item{a juxtaposition of two s-expressions, enclosed by parentheses
      @litchar{(} @litchar{)} and separated by a dot @litchar{.}: @litchar{(0
      . false)}, @litchar{(1 . (2 . 3))}, @litchar{((1 . 2) .  3)},
      @litchar{(+ . (1 . (2 . 3)))}, @racket[(* . ((+ .  (1 . (2 . ()))) . ((-
      . (5 . (1 . ()))) . ())))], ...}
]

The first three forms of s-expressions are said to be @emph{atomic}, the last
form be @emph{compound}.  Note that in the definition, the dot @litchar{.} is
non-assocciative.  Thus @litchar{(1 . (2 . 3))} and @litchar{((1 . 2) .  3)}
are different s-expressions.  It turns out that if we stick to the dotted
notation, we will soon be overwhelmed by the dots and parentheses.  To
overcome this verbosity, a right-deviating abbreviation for the dotted
notation is allowed, for example, @litchar{(1 . (2 . 3))} can be abbreviated
as @litchar{(1 2 . 3)}, and similarly @litchar{(+ . (1 . (2 . 3)))} as
@litchar{(+ 1 2 . 3)}, @racket[(* . ((+ .  (1 . (2 . ()))) .  ((- . (5 . (1 .
()))) .  ())))] as @racket[(* (+ 1 2 . ()) (- 5 1 . ()) .  ())].  This last
example shows that we can build very complex s-expressions by nesting.  In
particular, if the rightmost element of such a nesting, in other words the
most deeply nested element, is @litchar{()}, @litchar{()} and the dot
@litchar{.} preceeding it can be completely ommited, so the last example can
be further abbreviated as simply @racket[(* (+ 1 2) (- 5 1))].  This extremely
abbreviated notation is indeed what we use most.  Actually, if you type
directly in a Racket interactive session any of these s-expressions invovling
dots and parentheses you have seen so far, except @racket[(* . ((+ .  (1 . (2
. ()))) . ((- . (5 . (1 . ()))) . ())))], @racket[(* (+ 1 2 . ()) (- 5 1 .
()) . ())] and @racket[(* (+ 1 2) (- 5 1))], then ask Racket to evaluate it,
you will surprisingly get a syntax error.

@eg[
(1 . (2 . 3))
]

@eg[
((1 . 2) . 3)
]

@eg[
(+ 1 2 . 3)
]

@eg[
(* . ((+ .  (1 . (2 . ()))) . ((- . (5 . (1 . ()))) . ())))
]

@eg[
(* (+ 1 2 . ()) (- 5 1 . ()) . ())
]

@eg[
(* (+ 1 2) (- 5 1))
]

It suggests that Racket accepts only a subset of compound s-expressions,
namely, those that always end up with the terminator @litchar{()} in the
deepest position of a nesting.  They form one part of the syntax of Racket
@emph{expressions}.  Those atomic s-expressions, except @litchar{()} form the
other part.  The status of @litchar{()} is rather embarrassing.  On one hand,
it enables further abbreviation of notation.  On the other hand, it is not
accepted as a valid Racket expression.

@eg[
()
]

Now you see no matter it is code like @racket[(define (inc x) (+ x 1))] and
@racket[(inc 2)], or data like @racket[3] and @racket[(list 1 2 3)], both code
and data are written in a uniform syntax, that is, an abbreviated
s-expresssion syntax.  Should this uniformity only lie on the surface syntax,
it would be rather shallow.  Nevertheless it turns out that this seeming
superficial uniformity is actually a reflection of that inside the language
processor.  It is this latter uniformity that enables code to be manipulated
as data.

@subsection{Code as Data}

A language processor does not directly manipulate code and data, i.e.,
expressions.  We, programmers do---we type in them, delete them, copy and
paste them in our favorite text editors.  Instead, a language processor
manipulates the @emph{internalization} of code and data.  Thus code and data
must first be @emph{internalized}.  @note{Internalization is usually done by a
@emph{reader} or @emph{parser}.  The process is interesting on its own but out
of the main topic of this lecture.}

We mentioned earlier that in Lisp-family languages, code and data are written
in the s-expression uniform.  One even special feature of these languages is
that after internalization, code and data are still in uniform, no longer in
s-expression syntax but with almost isomorphic structure.  More precisely,
code resides in the language processor in the form of the internalization of
one primitive data.  This primitive data are supported by all Lisp-family
languages.  It represents a @emph{pair}.  A pair is internalized as two
@racket[cons]-cells, with each cell holding one component of the pair.  The
name comes from @racket[cons], the primitive operation to @emph{cons}truct a
pair.  The other two primitive operations are @racket[car] and @racket[cdr].
@note{@hyperlink["http://en.wikipedia.org/wiki/Car_and_cdr"]{@racket[car] and
@racket[cdr]} are historical names.} They are accessor functions that extract
respectively the first and second component of a pair.  So code are
internalized as @racket[cons]-cells, in fact, @racket[cons]-cells ended by
the internalization of the terminator @litchar{()}.  @racket[cons]-cells of
this kind is actually the internalization of data that represents a list.

@eg[
(cons 1 (cons 2 3))
]

@eg[
(car (cons 1 (cons 2 3)))
]

@eg[
(cdr (cons 1 (cons 2 3)))
]


In previous section, we have noted that Racket accepts 

@section{Pattern-based Macros}

@subsection{Avoiding Code Repetition}

@subsection{Abbreviating Coding Patterns}

@subsection{Extending Language Syntax}

