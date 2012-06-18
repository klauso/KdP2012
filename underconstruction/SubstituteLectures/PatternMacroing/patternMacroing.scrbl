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

We mentioned earlier that in Lisp-family languages, code and data are in the
s-expression uniform.  One even special feature of these languages is that
after internalization, code and data are still in uniform, no longer of
s-expression but with almost isomorphic structure.  More precisely, code
resides in the language processor in the form of the internalization of one
primitive data.  This primitive data are supported by all Lisp-family
languages.  It represents a @emph{pair}.  A pair is internalized as two
@racket[cons]-cells, with each cell holding one component of the pair.  The
name comes from @racket[cons], the primitive operation to @emph{cons}truct a
pair.  The other two primitive operations are @racket[car] and @racket[cdr].
@note{@hyperlink["http://en.wikipedia.org/wiki/Car_and_cdr"]{@racket[car] and
@racket[cdr]} are historical names.} They are accessor functions that extract
respectively the first and second component of a pair.

@eg[
(cons 1 (cons 2 3))
]

@eg[
(car (cons 1 (cons 2 3)))
]

@eg[
(cdr (cons 1 (cons 2 3)))
]

Thus code are internalized as @racket[cons]-cells, in fact, @racket[cons]-cells
ended by the internalization of the terminator @litchar{()}.
@racket[cons]-cells of this kind is actually the internalization of data that
represents a list.  Nevertheless, code is code, it expresses some computation.
When Racket sees code, it will always try to perform the computation the code
describes, and deliver the result of the computation back to you.

@eg[
(+ 1 2 3)
]

@eg[
(list (+ 0 1) (+ 1 1) (+ 1 2))
]

You see when you write an expression like these, Racket see them as code.
Note in the second example, what you get as result is indeed data
(representing a list of @racket[1], @racket[2] and @racket[3]), but the
expression you type in, @racket[(list (+ 0 1) (+ 1 1) (+ 1 2))], contains
sub-expressions (@racket[(+ 0 1)], @racket[(+ 1 1)], @racket[(+ 1 2)]) that
describe computation.  That means the whole expression is still code, not
data, because pure data should not contain any sub-expression that describe
any computation, such as @racket[1], @racket["hi"], etc.  Racket, more
precisely, the reader, expects all compound s-expressions as code.  This is
why you get error when you type in something like @litchar{(+ 1 2 .  3)}
because the reader cannot internalize it.  It is not in valid code syntax,
that is, it does not end with the terminator @litchar{()}.

What if we just want to get @racket[(+ 1 2 3)] or @racket[(list (+ 0 1) (+ 1
1) (+ 1 2))] as (resulting) data?  A first attempt is

@eg[
(list + 1 2 3)
]

@eg[
(list (list + 0 1) (list + 1 1) (list + 1 2))
]

We are one step closer, but the results are still not what we want.  We want
the symbol @racket[+] in place but it still gets evaluated,\footnote{Actually
so do @racket[0], @racket[1], @racket[2].  Since they evaluate to themselves,
they remain the same in the result.  But be aware something still happens
behind the scene.} @litchar{#<procedure:+>} representing its value.  What we
need is a mechanism to mention some code to Racket, so that it understands
that it should not evaluate the internalization of this piece of code.  We
meet this situation fairly often in our writing.  When writing an article, we
often needs to mention a word, a phrase or a sentence, so that our reader know
that we are talking about the word, phrase or sentence itself, instead of what
it expresses.  What do we dd?  We quote it!  For example, the sentence--- "Get
out of here!" is a rude expression---quotes the rude expression we are talking
about.  When you read this sentence, you know that you are not asked to get
out of here, but told that "get out of here!" is a rude expression.  Inspired
by this mechanism we employ in writing, the inventor of Lisp introduced an
operator called @racket[quote].  Using @racket[quote] we can tell Racket not
to evaluate the quoted code.

@eg[
(quote (+ 1 2 3))
]

@eg[
(quote (list (+ 0 1) (+ 1 1) (+ 1 2)))
]

Note that Racket provides a shorthand for @racket[quote], instead of
@racket[(quote (+ 1 2 3))], you can write @racket['(+ 1 2 3)], that is, you
just put a single quotation mark @litchar{'} before the code you want to
quote.  Be aware that there is no more @litchar{'} after the code you quote.

In addition to quoting code, you can quote also other data.

@eg[
'1
]

@eg[
'"hi"
]

The outcome is that @racket[quote] has no effect on literals.  Racket simply
return the quoted literal as is, and no prefixing @litchar{'}.

Furthermore, you can also quote identifiers and dotted-pairs.  In otherwords,
you can quote any s-expression.

@eg[
(quote x)
]

@eg[
(quote lambda)
]

@eg[
(quote ())
]

@eg[
(quote (+ 1 2 . 3))
]

@eg[
(quote (+ 1 2 3))
]

A quoted identifier represents a symbol.  A quoted dotted-pair represents a
pair, that is, @racket[(quote (+ 1 2 . 3))] is equivalent to @racket[(cons
(quote +) (cons (quote 1) (cons (quote 2) (quote 3))))], @racket[(quote (+ 1 2
3))] equivalent to @racket[(list (quote +) (quote 1) (quote 2) (quote 3))] and
also to @racket[(cons (quote +) (cons (quote 1) (cons (quote 2) (cons (quote
3) (quote ())))))].  This reveals the relationship between @racket[list] and
@racket[cons] and in turn, that between the lists and pairs.

So @racket[quote] provides a fast way to construct instances of pairs and
lists.  These instances can be used as usual.

@eg[
(car (quote (1 2 . 3)))
]

@eg[
(cdr (quote (1 2 . 3)))
]

@eg[
(first (quote (+ 1 2 3)))
]

@eg[
(rest (quote (+ 1 2 3)))
]

If @racket[quote] is only used for this purpose, you are using a sledgehammer
to crack a nut.  Recall that @racket[quote] is introduced because we want to
have quoted code treated as data.  But what is this good for?  The answer is
with it, we can write code that manipulates code.  In particular, we can write
a Racket program that executes a Racket program.  Below is a function that
does simply arithmetic calculation.

@racketblock[
(define (ask inp)
  (match inp
    [(list '+ x y) (make-add x y)]
    [(list '- x y) (make-sub x y)]
    [(list '* x y) (make-mul x y)]
    [(list '/ x y) (make-div x y)]
    [_ (if (number? inp)
           (make-lit inp)
           (error "Not a number: " inp) ) ] ) )

(define (ans exp)
  (match exp
    [(struct add (lhs rhs)) (+ (calc lhs) (calc rhs))]
    [(struct sub (lhs rhs)) (- (calc lhs) (calc rhs))]
    [(struct mul (lhs rhs)) (* (calc lhs) (calc rhs))]
    [(struct div (lhs rhs)) (/ (calc lhs) (calc rhs))]
    [(struct lit (num)) num] ) )
]

Now we can feed some quoted Racket arithmetic expressions to the function
@racket[calc] to calculate.  For example, @racket[(calc '(* (+ 1 2) (- 6
3)))], which should return @racket[9], exactly the same with what we will get
by let Racket evaluate the expression directly.

@eg[
(* (+ 1 2) (- 6 3))
]

This still does not reach far enough.  The inital and ultimate goal of the
existence of @racket[quote], and in turn this "code as data" magic is to allow
one to write an interpreter in Racket for Racket.  Below is the @racket[parse]
and @racket[reduce] function from
@hyperlink["https://github.com/klauso/KdP2012/blob/master/islinterpreterwostructs.rkt"]{islinterpreterwostructs.rkt}
in previous lectures, refactored to use pattern matching instead.

@#reader scribble/comment-reader
(racketblock
;; s-exp -> Exp
;; parses an s-expression to an abstract syntax tree
(define (parse sexp)
  (match sexp
    [(list 'lambda args body) (make-lam args (parse body))]
    [(list 'define name e) (make-var-def name (parse e))]
    [(list 'local defs e) (make-locl (map parse defs) (parse e))]
    [(list 'cond (list test body) ...)
     (make-cnd (map make-cnd-clause test body)) ]
    [(list rator rand ...) (make-app (parse rator) (map parse rand))]
    [_ (if (symbol? sexp)
           (make-var sexp)
           (make-primval sexp) ) ] ) )

; Exp Env -> Exp
; reduces an expression in an environment
(define (reduce e env)
  (match e
    [(struct app (fun args)) (reduce-app fun args env)]
    [(struct lam (args body)) (make-closure args body env)]
    [(struct var (x)) (lookup-env env x)]
    [(struct locl (defs e)) (reduce-local defs e env)]
    [(struct cnd (list (struct cnd-clause (c e)) clauses ...))
     (reduce-cond c e clauses env) ]
    [_ (error "Cannot reduce: " e)] ) )
)

@section{Pattern-based Macros}

@subsection{Avoiding Code Repetition}

@subsection{Abbreviating Coding Patterns}

@subsection{Extending Language Syntax}

