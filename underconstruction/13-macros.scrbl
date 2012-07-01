#lang scribble/manual

@(require scriblib/footnote)
@(require "scribbleUtilities.rkt")

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
the so-called @emph{syntactic keywords}, such as @racket[lambda], @racket[if],
@racket[let], @racket[cond], @racket[and], @racket[or], and so on.  Moreover,
it seems some identifiers like @racket[and] and @racket[or] should designate
functions but somehow do not.  All these mysteries will be dispelled in this
lecture.  In particular, we will introduce to you another abstraction
mechanism, macros.  In contrast to functions which abstract over
@emph{computations}, macros abstract over @emph{code} that @emph{describes}
some computations.  This new abstraction mechanism allows us to avoid code
repetition, to abbreviate common coding patterns @note{@(linebreak) Be aware
that "pattern" here has little to with @emph{data structures}.  If it does
with any structure, then say, @emph{code structure}.}, and to extend the
language with customized (but compatible) syntax.  All these possibilities
make the language and eventually ourselves more expressive, since we can write
@emph{less} but say @emph{more}.  Unfortunately, those people who failed to
sense the simplicity of the uniform syntax usually cannot also appreciate the
power of macros.  In a large degree, the extremely regular syntax facilitates
the work of macros, that is, manipulating @emph{code}, though @emph{as data}.

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
also adopted for code.  Since then, the Lisp-family languages maintain this
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
form be @emph{compound}.  This compound form of s-expression is traditionally
called a pair.  But to not confuse with the pair data structure, we will
called it s-pair.  Note that in an s-pair, the dot @litchar{.} is
non-assocciative.  Thus @litchar{(1 . (2 . 3))} and @litchar{((1 . 2) .  3)}
are different s-expressions.  It turns out that if we stick to the dotted
notation, we will soon be overwhelmed by the dots and parentheses.  To
overcome this verbosity, a right-deviating abbreviation for the dotted
notation is allowed, for example, @litchar{(1 . (2 . 3))} can be abbreviated
as @litchar{(1 2 . 3)}, and similarly @litchar{(+ . (1 . (2 . 3)))} as
@litchar{(+ 1 2 . 3)}, @litchar{(* . ((+ .  (1 . (2 . ()))) .  ((- . (5 . (1 .
()))) .  ())))} as @litchar{(* (+ 1 2 . ()) (- 5 1 . ()) .  ())}.  This last
example shows that we can build very complex s-expressions by nesting.  In
particular, if the rightmost element of such a nesting, in other words the
most deeply nested element, is @litchar{()}, @litchar{()} itself and the dot
@litchar{.} preceeding it can be completely omitted, so the last example can
be further abbreviated as simply @racket[(* (+ 1 2) (- 5 1))].  This
maximally abbreviated form of s-expression is traditionally called list.  But
to not confuse with the list data structure, we will call it s-list.
@litchar{()} is used as an end marker, terminating an s-list, thus its name.
Traditionally it is called nil or NIL.  s-lists are what we write most.
Actually, if you type directly in a Racket interactive session any of these
s-expressions invovling dots and parentheses you have seen so far, except
@litchar{(* . ((+ .  (1 . (2 . ()))) . ((- . (5 . (1 .  ()))) . ())))},
@litchar{(* (+ 1 2 . ()) (- 5 1 .  ()) . ())} and @racket[(* (+ 1 2) (- 5
1))], and ask Racket to evaluate it, you will surprisingly get a syntax error.

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
@emph{reader} or @emph{parser}.}

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
(code:quote (+ 1 2 3))
]

@eg[
(code:quote (list (+ 0 1) (+ 1 1) (+ 1 2)))
]

Note that Racket provides a shorthand for @racket[quote], instead of
@racket[(code:quote (+ 1 2 3))], you can write @racket['(+ 1 2 3)], that is,
you just put a single quotation mark @litchar{'} before the code you want to
quote.  Be aware that there is no more @litchar{'} after the code you quote.

In addition to quoting code, you can also quote other data.

@eg[
'1
]

@eg[
'"hi"
]

The outcome suggests that @racket[quote] has no effect on literals.  Racket
simply return the quoted literal as is, and no prefixing @litchar{'}.

Furthermore, you can also quote identifiers and s-pairs.  In otherwords, you
can quote any s-expression.

@eg[
'x
]

@eg[
'lambda
]

@eg[
'()
]

@eg[
'(+ 1 2 . 3)
]

@eg[
'(+ 1 2 3)
]

A quoted identifier represents a symbol.  A quoted s-pair represents a pair
and a quoted s-list represents a list, so @racket[(quote (+ 1 2 . 3))] is
equivalent to @racket[(cons '+ (cons '1 (cons '2 '3)))], @racket['(+ 1 2 3)]
equivalent to @racket[(list '+ '1 '2 '3)] and also to @racket[(cons '+ (cons
'1 (cons '2 (cons '3 '()))))].

So @racket[quote] provides a fast way to construct instances of pairs and
lists.  These instances can be used as usual.

@eg[
(car '(1 2 . 3))
]

@eg[
(cdr '(1 2 . 3))
]

@eg[
(first '(+ 1 2 3))
]

@eg[
(rest '(+ 1 2 3))
]

If @racket[quote] is only used for this purpose, you are using a sledgehammer
to crack a nut.  Recall that @racket[quote] is introduced because we want to
have quoted code treated as data.  But what is this good for?  The answer is
with it, we can write code that manipulates code.  Its meaning is well
illustrated by the following example.

@#reader scribble/comment-reader
(racketblock
;; parse : sexp -> aexp
;; parses an s-expression to an arithmetic expression
(define (parse sexp)
  (match sexp
    [(list '+ x y) (make-add (parse x) (parse y))]
    [(list '- x y) (make-sub (parse x) (parse y))]
    [(list '* x y) (make-mul (parse x) (parse y))]
    [(list '/ x y) (make-div (parse x) (parse y))]
    [_ (if (number? sexp)
           (lit sexp)
           (error "Invalid syntax: " sexp) ) ] ) )

;; reduce : aexp -> aexp
;; reduces an arithmetic expression in one step
(define (reduce aexp)
  (match aexp
    [(struct add (lhs rhs)) (reduce-op make-add + lhs rhs)]
    [(struct sub (lhs rhs)) (reduce-op make-sub - lhs rhs)]
    [(struct mul (lhs rhs)) (reduce-op make-mul * lhs rhs)]
    [(struct div (lhs rhs)) (reduce-op make-div / lhs rhs)] ) )

;; evaluate : aexp -> number
;; evaluates an arithmetic expression to a number
(define (evaluate aexp)
  (match aexp
    [(struct lit (n)) n]
    [_ (evaluate (reduce aexp))] ) )
)

Both @racket[reduce] and @racket[evaluate] together are responsible for
arithmetic expression evaluations.  But with them only, whenever we want to
evaluate an arithmetic expression, we have to type something like
@racket[(evaluate (make-mul (make-add (make-lit 1) (make-lit 2)) (make-mul
(make-lit 5) (make-lit 3))))].  This is just too much.  Life becomes simpler
with the @racket[parse] function.  Essentially, it makes an arithmetic
expression from an s-expression.  So that we can first feed a quoted
s-expression to @racket[parse], and let @racket[parse] build the corresponding
arithmetic expression, then feed this arithmetic expression to
@racket[evaluate].  In other words, we compose @racket[evaluate] and
@racket[parse].  For example, we can now write @racket[(evaluate (parse '(* (+
1 2) (- 5 3))))] which is way much simpler.  Note the quoted s-expression
@racket[(* (+ 1 2) (- 5 3))] is itself valid Racket code.  If we do not quote
it, Racket will evaluate it to 6.  Quoting it turns it to data.  We say
(Racket) functions like @racket[parse] manipulate (Racket) code as (Racket)
data.

Yet this still does not reach far enough.  The inital and ultimate purpose of
the existence of @racket[quote], and in turn this "code as data" magic is to
allow one to write an interpreter in Racket for Racket, so called
self-interpreter.  Below is the @racket[parse] and @racket[reduce] function
from
@hyperlink["https://github.com/klauso/KdP2012/blob/master/islinterpreterwostructs.rkt"]{islinterpreterwostructs.rkt}
in previous lectures, refactored to use pattern matching instead.

@#reader scribble/comment-reader
(racketblock
;; parse : sexp -> Exp
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

; reduce : Exp Env -> Exp
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

Although the interpreted language is just a subset of Racket, it illustrates
how @racket[quote] and "code as data" has rendered the language
self-contained.  Not many languages have this feature.  It is one part that
makes Lisp-family languages unique.  The other part is their powerful macro
systems.

@section{Pattern-based Macros}

Lisp-family languages are well known for their powerful macro systems.
Similar to functions, macros are also abstractions.  While functions abstract
over run-time computations, macros abstract over compile-time code.  This new
dimention of abstraction provides us a new perspective to view what we code
and how we code.  Do we repeat some piece of code too many times?  Do we keep
applying some coding patterns?  Do we sometimes find the need of one specific
piece of syntax that is unfortunately unavailable but could express our ideas
more naturally?  Refecting for a while, we will probably answer "yes" to
almost all these questions.  Then the natural question is this: can we do
something to improve the situation?  The answer is also "yes".  This is
exactly where macros come into play.  With macros, typical code repetitions
can be avoided; common coding patterns can be abbreviated; specific (but
compatible @note{It will become clear soon what "compatible" means.}) can be
invented.  In a word, macros can ease our work.

A traditional Lisp macro takes as inputs s-expressions and produce as ouput an
s-expression which is supposed to be valid code.  Note that the input
s-expressions do not have to be quoted, even if they are code.  A macro does
not distinguish code and data at all.  From its point of view, they are both
s-expressions and that is all it cares about.  On the other hand, since macros
are often defined to accept code as input and produce code as output, macros
are also called code transformers.  Due to this transformation property,
writing macros is also considered as a form of programming, called macro
programming, or simply macroing.  This is why some Lisp programmers describe
macro programming as "writing programs that write programs".  Nevertheless
traditional Lisp macro systems lack both high-level abstractions to manipulate
s-expressions and automatic mechanisms to resolve name capturing. @note{Name
capturing occurs when some names in the prduced code by a macro no longer
retain their inital binding.  The produced code is said to be
@emph{unhygienic}.} This makes macro programming in these systems both tedious
and error-prone.  These two dimentions of complexity make macro programming
almost inaccessible to junior, even senior Lisp programmers.  To remove these
barriers, @emph{high-level} @emph{hygienic} macro systems were developped.
These systems provide high-level abstractions, namely, patterns and templates,
to access and construct s-expressions.  More importantly, they also resolve to
their greatest extent the name capturing problem under the scene without
programmers' concern, so that the produced code is hygienic.  Scheme features
this kind of pattern-based macro system.  Racket inherits and refines it.  In
the following subsections, we will explore the three use cases of macros
inside Racket's macro system.

@subsection{Avoiding Code Repetition}

In
@hyperlink["https://github.com/klauso/KdP2012/tree/master/underconstruction/SubstituteLectures/PatternMatching/card.rkt"]{card.rkt},
we try to represent the 52 poker cards using our
@racket[card]-@racket[struct].  For example, we would like to have the
following definition for the card called "Ace of Spades".

@racketblock[
(define 🂡 (make-card "A" "♠"))
]

We use the unicode identifier 🂡 to name the @racket[card] instance.  It is
clear that we have to write 51 more such definitions.  Even we can copy and
paste, it is actually still quite some work.  Moreover, all the work is just
boring repetition.  Let us be explicit what pieces of code will be repeated:
for each suit , we have to repeat @litchar{(define}, @litchar{(make-card}, the
string represents the suit (one of @racket["♠"], @racket["♥"], @racket["♦"]
and @racket["♣"]), and @litchar{))}, regardless of spaces; for all suits, we
have to repeat strings that represent ranks (one of @racket["A"], @racket["2"]
to @racket["10"], @racket["J"], @racket["Q"] and @racket["K"]).  Clearly we
do not want to do this.  

Racket's macro system can free us from this kindof boring task.  For it to
work, we give Racket a template that specifies the form of code that should be
produced.  Then Racket will automatically generate code following the
template.  Such a template usually contains holes to be fill in.  We can label
these holes using same or different names accordingly to whether they are
supposed to be filled by same or different code pieces.  A moment's thinking
suggests that these code pieces cannot be those repeated.  So other parts of
a template than its holes must have those repeated code pieces.  Now a
template for our example can be formed.

@racketblock[
(begin (define cs (make-card r "♠")) ...
       (define ch (make-card r "♥")) ...
       (define cd (make-card r "♦")) ...
       (define cc (make-card r "♣")) ... )
]

Since we are going to generate a sequence of top-level definitions, we collect
them in a @racket[begin] expression.  The main reason for this is that Racket
expects a fully-filled template to be a valid single s-expression, not a
sequence of s-expressions.  @racket[cs], @racket[ch], @racket[cd], @racket[cc]
and @racket[r] are names that label the holes.  The ellipsis @litchar{...}
indicates that the sub-template preceeding it will be repeated.  Now that we
have the template, but there still remains the question how should we tell
Racket this is a template.  Obviously just giving this template to Racket to
evaluate will not work.  What we are assured is only that after fully-filled,
the template will be valide code.  Before it gets filled, it is of course not
because those names labelling the holes are not bound to anything at all.
Also, ellipses are not allowed in expressions that Racket can directly
evaluate.  What should we do?

Actually when we introduced functions we encountered a similar situation.  We
have some expression containing some unknowns, for example, @racket[(+ x y
1)], we wanted to tell Racket we would later fill in the holes in the
expression labelled by @racket[x] and @racket[y].  What did we do?  We define
a function that paremeterizes the expression.

@racketblock[
(define (f x y)
  (+ x y 1) )
]

We can do similar things for templates.  But we cannot use @racket[define]
since it is used to give names to an expression that represents some
computation. @note{This is probably the most general description of
@racket[define].  To see that it indeed covers all its uses we have seen so
far, simply note that (1) if the named expression actually builds data, it can
be considered to represent a trivial computation that simply returns the data
as is; (2) a function definition using a function header can always be
rewritten in a form that uses @racket[define] to name a @racket[lambda]
abstraction.} Racket provides @racket[define-syntax-rule].  Using it, we can
define a macro that parameterizes a template that represents some code.  For
example, we can define a macro called @racket[make-cards] that abstract our
example template as follows.

@racketblock[
(define-syntax-rule (make-cards (r ...)
                                (cs ...)
                                (ch ...)
                                (cd ...)
                                (cc ...) )
  (begin (define cs (make-card r "♠")) ...
         (define ch (make-card r "♥")) ...
         (define cd (make-card r "♦")) ...
         (define cc (make-card r "♣")) ... ) )
]

The only thing new is in the macro header @racket[(make-cards (r ...) (cs ...)
(ch ...) (cd ...) (cc ...))]: those names labelling holes in the template are
followed by ellipses.  They do not look like parameters in function headers.
Because they are not.  If we say this whole header specifies a pattern, does
it remind you of something?  Yes, pattern matching, though is no longer on
data structures but on code structures.  Essentially a macro header specifies
the macro's call pattern.  The names that labelling holes in the template are
also called pattern variables.  Ellipses in the pattern act similarly.  But be
aware that you must make sure that if a name in the pattern is followed
(immediately or not) by some ellipses, exactly the same number of ellipses
must follow the name (immediately or not) in the template.  Otherwise, you
will get an error complaining that ellipses are missing.  Check yourself that
our macro definition above satisfies this requirement.  To see how it could
fail, just delete one ellipsis in the pattern or the template to make them not
match.

Now we can call this macro in the same way we call a function.

@racketblock[
(make-cards ("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K")
            ( 🂡  🂢  🂣  🂤  🂥  🂦  🂧  🂨  🂩  🂪   🂫  🂭  🂮)
            ( 🂱  🂲  🂳  🂴  🂵  🂶  🂷  🂸  🂹  🂺   🂻  🂽  🂾)
            ( 🃁  🃂  🃃  🃄  🃅  🃆  🃇  🃈  🃉  🃊   🃋  🃍  🃎)
            ( 🃑  🃒  🃓  🃔  🃕  🃖  🃗  🃘  🃙  🃚   🃛  🃝  🃞) )
]

When Racket's macro processor sees this s-expression, it first decides that
its head names a macro, then it tries to match the whole call with the pattern
of the macro definition, if it matches successfully, a binding for the pattern
variabels will be generated and what they are bound to will be substituted
into the template.  For our example, the match goes like this: first, the
macro name @racket[make-cards] matches as if it a literal; then the @racket[(r
...)] matches @racket[("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K")];
similarly for other pattern variables.  So the two indeed match perfectly,
resulting in a binding in which @racket[r] is bound to the s-expression
@racket[("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K")] and similarly
for others.  Then the macro processor starts to substitute these s-expressions
for these pattern variables.  But note that, @racket[r] in the template will
not be substituted for @racket[("A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J"
"Q" "K")], neither do others due to the ellipses coming after them.  We
mentioned before an ellipsis in a template indicates repetition of the
sub-template preceeding it.  But we did not make it explicit how many times
the repetition should be.  Now we can be clear that the sub-template
preceeding an ellipsis will be repeated as many times as the number of
elements contained in the s-list bound to the pattern variable, and the
pattern variable in the repeated sub-templates will be substituted for
elements of the bound s-list in a one-to-one fashion.  Note that, if in the
pattern there exist several pattern variables followed by ellipses, and two or
more of them appear together inside a sub-template followed by ellipses, their
bound s-lists must be of the same length.  Otherwise, we get an error
complaining that their counts do not match.  All ellipsis-followed
sub-templates in our example contain two ellipsis-followed pattern variables.
It is easy to see that the s-lists bound to these pattern variables are indeed
of the same length.  To see how it could cause an error, try to call
@racket[make-cards] with s-lists of different lengths.  Now it should be clear
that after all the substitutions are done, we will have a sequence of card
definitions inside @racket[begin].  The process of replacing a macro call with
the fully substituted template from the macro definition is called @emph{macro
expansion}. 

@subsection{Abbreviating Coding Patterns}

Not all code repetitions are as plain as that is shown in the previous
subsection.  Sometimes we find that what we are repeating actually appears in
a structural form.  Programmers notice these kinds of code structures.  They
call them coding patterns or coding idioms.  They collect them into their
coding dictionaries and practice using them whenever possible.  One day when
they become seasoned, they teach these idioms or patterns to their disciples,
and so on.  This is after all how knowledge is usually conveyed.  But it is
clear that the recurrence of these coding patterns is definitely a form of
repetition.  Why should we be forced to repeat them?  Why does not the
programming language provide some more convenient constructs to ease our
coding task?  The answer is that language designers cannot predict these
coding patterns beforehand.  These patterns emerge in our coding practice.
The crux of the problem is that the programming language does not provide any
or any sophisticated enough way that allows the programmer to abbreviate or
abstract over these coding patterns.  Lisp-family languages do, via their
powerful macro systems.  So does Racket.

Let's first see a simple example, which is an immediate application of a
@racket[lambda]-expression to some arguments, for instance,

@racketblock[
((lambda (x y z)
   (* (+ x y) (- x z)) )
 (/ 8 4) 6 3 )
]

This coding pattern is useful when we want to avoid repeated computations.  In
the above example, the varaible @racket[x] will get bound to the value of
@racket[(+ 1 2)].  In the body exrepssion, even though @racket[x] appears
twice, the expression @racket[(+ 1 2)] will be evaluated only once before
entering the body of the @racket[lambda]-expression.  When the expression
@racket[(* (+ x y) (- x z))] is evaluated, @racket[x] is already bound to the
value @racket[3], no computation is need at all.

It seems this coding pattern is quite handy.  It indeed recurs whenver we want
to introduce local variable bindings.  But it has a drawback.  For the simple
example above, it is not that obvious.  We can still manage to the binding
relations between the variables and the arguments.  But it will be difficult
if the body of the @racket[lambda]-expression becomes more complex.  Then the
variables and the arguments will be so far that, we can no longer easily see
their binding relations.  Therefore, it is reasonable to abstract it away.
Let's start again with the template.

@racketblock[
((lambda (var ...) body)
 arg ... )
]

Now we need to design the macro call pattern, which is its user-interface.
Since we want not to separate the variables and arguments far away, we should
gather them to one place that is easy to find.  Two good choices may be either
before or after the body expression.  Without loss of generality, we choose
the former.  If we name the macro @racket[with], its header look likes this
@racket[(with [(var arg) ...] body)].  Its definition is shown below:

@racketblock[
(define-syntax-rule (with [(var arg) ...] body)
  ((lambda (var ...) body)
   arg ... ) )
]

Now we can rewrite the simple example using @racket[with].

@racketblock[
(with [(x (/ 8 4))
       (y 6)
       (z 3) ]
  (* (+ x y) (- x z)) )
]

This time, it is clear to the binding relations between the variables and
arguments.  The benefit will of course be more obvious for larger examples.
Racket provide @racket[let] that can do the same thing and more.

In the example above, the coding pattern is actually quite simple.  It just
shows one basic usage of macros to abbreviate coding patterns.  Macros can
also be used to abstract over more complicated coding patterns.  One typical
example is nested @tt{if}-@tt{else} statements found in C-family languages.
In programs written in these languages, you can easily find code of the
following structure:

@verbatim|{
if ( ... ) {
  ...
}
else if ( ... ) {
  ...
}
  ...
else {
  ...
}
}|

This coding pattern essentially expresses multi-conditional analysis.  In
Racket, we use @racket[cond] for that purpose.  It proves convenient.  But so
far you have been told that it is a primitive control construct.
@note{@racket[cond] is indeed provided as a primitive control construct in
original Lisp.  But it is not in Scheme, neither in Racket.}  It is actually
not.  The primitive control construct to form a conditional expression in
Racket is @racket[if].  Now suppose, Racket does not provide @racket[cond] but
only @racket[if] out of the box, what would happen?  Does it mean that you
have to, as programmers of other languages do, identify nested @racket[if]
expressions as a coding pattern and fron now on write every multi-conditional
expression that way?  Fortunately not.  Racket allows us to define our own
@racket[cond] as a macro and use it as if it is provided out of box.

To see how @racket[cond] can be defined as a macro, let's start again with a
template.  The template should cover the coding pattern, that is, it may
contain nested @racket[if] sub-templates. 

@racketblock[
(if test-expr1
    then-expr1
    (if test-expr2
        then-expr2
        ??? ) )
]

Note the above piece is neither valid code nor valid template.  We use
@litchar{???} to indicate that some repetition is needed to complete the
template.  Since we know that ellipsis @litchar{...} is used for repetition in
a template, a first attempt to build the template may be just replacing
@litchar{???} with ellipsis @litchar{...}, parameterizing the pattern
variables, and defining a macro like this:

@eg[
(define-syntax-rule (condition (test-expr1 then-expr1)
                               (test-expr2 then-expr2) ... )
  (if test-expr1
      then-expr1
      (if test-expr2
          then-expr2
          ... ) ) )
]

Unfortunately it does not work.  Not only because an ellipsis is missing after
@racket[test-expr2]; but also because during expansion, @racket[then-expr2]
together with the ellipsis in the template will be substituted by all the
then-expressions in a call of @racket[condition], yet lined in sequence,
leaving out any nested @racket[if] and other test-expressions.  Convince
yourself that a template like

@racketblock[
(if test-expr1
    then-expr1
    (if test-expr2
        then-expr2 ) ... )
]

does not work either.  What we wish @litchar{???} could do is to say that
repeating the sub-template @litchar{(if test-expr2 then-expr2} as many times
as required and supplying as many closing parentheses as needed.
Unfortunately, this is out of the reach of ellipses @litchar{...}.  But notice
that what we wish ellipsis to do is exactly what @racket[conditonal] is
designed to do.  This suggests that if we can call @racket[condition]
recursively, we are done.  Racket supports recursive macro calls in a macro
definition.  The way to write a recursive macro call is similar to the way to
write a recursive function call.  But you must make sure that the recursive
macro call will actually match the call pattern:

@eg[
(define-syntax-rule (condition (test-expr1 then-expr1)
                               (test-expr2 then-expr2) ... )
  (if test-expr1
      then-expr1
      (condition (test-expr2 then-expr2) ...) ) )
]
                   
This time it looks good.  The macro definiton is accepted.  Nevertheless, it
fails when we use it:

@racketblock[
(define (absolute n)
  (condition
    [(< n 0) (- n)]
    [(>= n 0) n] ) )
]

When Racket's macro processor tries to expand the macro call in the above
definition, it will complain that the macro call @racket[(condition)] does
not match the call pattern.  The problem is that since both
@racket[test-expr2] and @racket[then-expr2] are supposed to to bound to an
sequence of s-expressions, in particular, these sequences may be empty; but if
you look at the macro header carefully, you will notice that it expects at
least one couple of test-exression and then-expression.  We can see the point
more clearly from the fully expanded code for the definiton of
@racket[absolute]:

@racketblock[
(define (absolute n)
  (if (< n 0)
      (- n)
      (if (>= n 0)
          n
          (condition) ) ) )
]

Because of the recursive call of @racket[condition], the expansion continues
until the bottom case is hit, that is, when both @racket[test-expr2] and
@racket[then-expr2] become @litchar{()}.  At this point, there is no
s-expression fed to @racket[condition], thus the recursive macro call becomes
@racket[(condition)], which fails to match the call pattern.  The solution is
to provide a pattern that would cover this base case.  Thus we need a
@racket[match]-like construct that allows us to supply multiple patterns to
cover different cases.  Racket provides us @racket[syntax-rules] which allows
us to define an anonymous macro, then we can use @racket[define-syntax] to
give the macro a name.  They work tegother in a similar way that
@racket[lambda] and @racket[define] do.  Using @racket[syntax-rules] and
@racket[define-syntax], the macro @racket[condition] will be defined as
follows:

@racketblock[
(define-syntax condition
  (syntax-rules ()
    [(condition) (void)]
    [(condition (test-expr then-expr) clause ...)
     (if test-expr
         then-expr
         (condition clause ...) ) ] ) )
]

The body of @racket[syntax-rules] looks similar to the body of @racket[match].
For each clause, the left hand side is a pattern specifying the form that a
macro call could take; the right hand side is the template corresponding to
this pattern.  Matching also goes from top down, favoring the template
corresponding to the first pattern that matches a macro call.  Note the
macro name acts as if it is a literal.  In other words, in pattern matching,
macro names must match exactly, equivalent to equality test.

For the base case @racket[(condition)], we choose to return nothing, which is
what @racket[(void)] does.  For the recursive case, we simplify the
ellipsis-pattern @racket[(test-expr2 then-expr2) ...] to just @racket[clause
...].  The reason is the corresponding template and this ellipsis-pattern
appears exactly the same.  That means their structure actually does not
matter.  We can simply use a pattern variable instead.

The pair of parentheses folling immediately @racket[syntax-rules] are used to
introduce some extra keywords.  We know that inside @racket[cond], we can use
the keyword @racket[else] to take care of all othercases.  We can do similar
thing for our @racket[condition], to introduce a keyword, say
@racket[otherwise], we simply put it in the parentheses following
@racket[syntax-rules].

@racketblock[
(define-syntax condition
  (syntax-rules (otherwise)
    [(condition) (void)]
    [(condition (otherwise othw-expr)) othw-expr]
    [(condition (test-expr then-expr) clause ...)
     (if test-expr
         then-expr
         (condition clause ...) ) ] ) )
]

We can rewrite @racket[absolute] as

@racketblock[
(define (absolute n)
  (condition
    [(< n 0) (- n)]
    [otherwise n] ) )
]

It get expanded to

@racketblock[
(define (absolute n)
  (if (< n 0)
      (- n)
      n ) )
], 
exactly what we want.

Note the position of the clause @racket[[(condition (otherwise othw-expr))
othw-expr]] matters.  If we put it after the clause that deals with the
recursive case, pattern matching could never reach it.  Since the pattern
variable @racket[test-expr] can match anything, the clause handling the
recursive case will always be taken.  The call of @racket[conditon] in
@racket[absolute] gets expanded by the macro processor to:

@racketblock[
(define (absolute n)
  (if (< n 0)
      (- n)
      (if otherwise
          n
          (condition) ) ) )
]

The problem of this expanded code is that, the identifier @racket[otherwise]
may not evaluate to a boolearn value and it may even be unbound at all.
Therefore, always be careful about the order of the clauses.

@;{ The following block is commented out for future consideration.

Another coding pattern commonly appears in functional programming languages
is an immediate application of a lambda abstraction to bind some value to some
variables, for example,

@eg[
(((lambda (x)
    (lambda (y)
      (* (+ x y)
         (- x y) ) ) )
  (* 3 4) )
 (* 2 3) )
]

This coding pattern is useful when we try to avoid repeated computation.  In
the above example, the varaibles @racket[x] and @racket[y] will get bound to
the values of @racket[(* 3 4)] and @racket[(* 2 3)], that is, 12 and 6
respectively.  Inside the the exrepssion @racket[(* (+ x y) (- x y))], even
though both @racket[x] and @racket[y] appear twice, @racket[(* 3 4)] and
@racket[(* 2 3)] will not be evaluated since @racket[x] and @racket[y] have
already bound to the values of these two expressions respectively.

This coding pattern recurs whenver we want to introduce local variable
bindings.  But note that the more local variables we introduce, the deeper the
nesting of @racket[lambda]-expressions goes; the more complicated the
innermost expression is, the farther the local variables and their bound
values are separated.  Both cause readability problems.  Therefore, it is
reasonable to abbreviate it, and more importantly to abstract it away.  Let's
start again with the template.  It must have the following shape.

@racketblock[
(??? (((lambda (var1)
         (lambda (var2)
           ???
             body ) ??? )
       exp1 )
      exp2 )
     ??? )
]

Lessons learned from the definition of @racket[condition] tells us to be
careful about the @litchar{???}s, not to blindly replace them with
@litchar{...}s.  Indeed the first and third of them just specify repeated
openning and closing parentheses, which is not our concern.  If we watch the
shape carefully, we will realize there is something similar to that of the
template of @racket[condition], namely, nested repetetion.  This again
suggests a recursive macro definition.  Let's name the macro @racket[with].
Now comes the design of the macro call pattern which is its user-interface.
One way to avoid the two problems of the coding pattern demonstrated above is
to gather the local variables and their bound values together, and put them
either before or after the main expression that references these local
variables.  Without loss of generality, we choose the former.  The natural and
default way to put s-expressions together is to use s-list.  Fruthermore, we
would like also to allow the s-list to be empty in which case, no local
variable is introduced.  All that said, our macro header can be either
@racket[(with [] body)] or @racket[(with [(var1 exp1) binds ...] body)].  The
former is obviously the base case.  It is easy to see a macro call like that
should simply be replaced by @racket[body].  The latter is of course the
recursive case.  We need to form a @racket[lambda]-expression with the local
variable @racket[var1] as parameter, with a recursive call to @racket[with] as
its body, and then apply it to the expression @racket[exp1].  Below is the
defintion.

@racketblock[
(define-syntax with
  (syntax-rules ()
    [(with [] body) body]
    [(with [(var1 exp1) binds ...] body)
     ((lambda (var1)
        (with [binds ...] body) )
      exp1 ) ] ) )
]

Now we can use @racket[with] to rewrite the above example.

@eg[
(with [(x (* 3 3))
       (y (* 4 4)) ]
  (+ x y) )
]

It expands to

@racketblock[
((lambda (x)
   (with [(y (* 4 4))]
     (+ x y) ) )
 (* 3 3) )
]

It seems the expansion does not reach the bottom since there is still an
@racket[with]-call inside the @racket[lambda]-expression.

}

@subsection{Extending Language Syntax}

In the previous subsection, we have seen the macro @racket[with] and
@racket[condition] allow us to write code in a new way.  In a sense, we have
extended the language syntax.  Both @racket[with] and @racket[condition] are
already provided by Racket, but under different names: @racket[let] for
@racket[with], and @racket[cond] for @racket[condition].  They are indeed
pre-defined as macros.  Their definitions subsume ours.  The call for
@racket[with] and @racket[condition] comes from our observation of some
recurring coding patterns.  But macros can also be used to make some syntax
out of our imagination.

When imperative programmers moves to the functional programming world, they
usually miss those loop constructs provided by the imperative language, if no
such construct is provided by the functional language.  For a functional
language with no syntax extensibility, all they can do is to hope such
construct would be supported in future version of the language. @note{This is
almost hopeless, since for a non-extensible functional language, if it omits
such constructs from the very beginning, it suggests a strong favor of
recursion.} But for Lisp-family languages, this is no problem.  If programmers
want, they can always build such constructs as macros.

A proper loop construct maintains a set of iteration variables (or loop
variabels).  Some of them are tested in a boolean condition in order to
control the entering and exiting of the loop.  All the iteration variables
should have already been initialized before entering the loop.  When entering
the loop, an iteration starts.  During the iteration, some iteration variables
may be modified.  These modifications will affect the next boolean condition
test, which in turn will determine whether to reenter the loop and start a new
iteration or to terminate the current iteration and exit the loop.

In the imperative programming world, there are several variants of loop
constructs.  C provides @tt{while}-loop and @tt{for}-loop.  The latter is
actually a better organized version of the former, encouraging the programmer
to put initializations and modifications of these iteration variables
together along with the boolean condition, before the body of the loop.  Python
provides slightly different @tt{for}-loop and @tt{while}-loop, each allowing
an optional @tt{else} branch that does something default when the boolean
condition is no longer met, that is, when control exits the loop.  We will try
to design a macro that brings together all the goods of these loop constructs.
Therefore, this time, we will start with the macro call pattern.  Let's call
our macro @racket[loop].  Furthermore, we would like to introduce two more
keywords for special purpose: one is @racket[when] which labels the boolean
expression, the other is @racket[else] which labels the default expression.
Eventually, we decide our macro call pattern to be:

@racketblock[
(loop [(ivar init step) ...]
      [when test]
      body ...
      [else just] )
]

We collect an iteration variable @racket[ivar], its initialization
@racket[init] and its modification @racket[step] into an s-list.  In
particular, the ellipsis following the s-list indicates that our loop
construct can have zero or more such collections.  The @racket[body] of the
loop can also be an arbitrary number of expressions.  These expressions are
usually evaluated in sequence but their values are ignored.  The boolean
expression @racket[test] follows the extra keyword @racket[when].  The default
expression @racket[just] follows the extra keyword @racket[else].  This
default expression is evaluated when the loop is exitted, and its value
becomes the value of the whole @racket[loop]-expression.

The next step is to design the code template.  Recall that in Racket, the
recommended way of expressing loops is via recursion.  Thus, since a call of
@racket[loop] is supposed to be transformed away, in our template, we should
use recursion.  Here is the template:

@racketblock[
(local [(define (iterate ivar ...)
          (if test
              (begin body ...
                     (iterate step ...) )
              just ) ) ]
  (iterate init ...) )
]

Essentially we define a @racket[local] recursive function @racket[iterate]
that takes all the iterative variables as parameters.  Inside its body,
@racket[if] @racket[test] evaluates to @racket[true], we @racket[begin] to
evaluate the expressions of the loop @racket[body] in sequence, ignoring their
values, and then we recursively call @racket[iterate] with new arguments that
are supposed to push the initial values of the iterative variables one step
further, that is, get them bound to new values.  When @racket[test] evaluates
to @racket[false], we know that the iteration is done, we should return the
value of the default expression @racket[just].  Putting the macro call pattern
and the template together, we get the following macro definition:

@racketblock[
(define-syntax loop
  (syntax-rules (when else)
    [(loop [(ivar init step) ...]
           [when test]
           body ...
           [else just] )
     (local [(define (iterate ivar ...)
               (if test
                   (begin body ...
                          (iterate step ...) )
                   just ) ) ]
       (iterate init ...) ) ] ) )
]

Now we can use our @racket[loop] macro to define a fucntion that sums a list
of numbers.

@racketblock[
(define (sum nums)
  (loop [(ns nums (rest ns))
         (s 0 (+ s (first ns))) ]
    [when (not (empty? ns))]
    [else s] ) )
]

In this example, the body of the loop is empty.  Another classic example of
using loop is to print out the times table.

@racketblock[
(loop [(i 1 (+ i 1))]
      [when (<= i 9)]
      (loop [(j 1 (+ j 1))]
            [when (<= j i)]
            (printf "~a * ~a = ~a\t" i j (* i j))
            [else (newline)] )
      [else (void)] )
]

For more detail of the function @racket[printf], @racket[newline] and
@racket[void], please refer to the Racket documentation.

@exercise[] Some C-family languages also provide another form of loop, called
@hyperlink[@tt{do-while}
loop]{http://en.wikipedia.org/wiki/Do_while_loophttp://en.wikipedia.org/wiki/Do_while_loop}.
Write a macro that can do  the same.

Racket is a descendant of Scheme.  In Scheme, a few identifiers are reserved,
sometimes called @emph{syntactic keywords}.  Their use cases are in the Lisp
tradition called @emph{special forms}.  They are special for both syntax and
semantics.  Let's take a look at @racket[lambda].  It is special in syntax
because if you use it incorrectly you will get a syntax error.  It is special
in semantics because it (when used correctly) @emph{creates} an anonymous
function.  Due to their special status, these syntactic keywords  need special
support from the language implementation.  Implementing a language is not an
easy and cheap task.  It takes a lot of efforts and resources.  Generally, the
richer the language syntax is, the more convenient to program in the language,
but on the other hand the more efforts and resources needs to be put into the
language implementation.  Clearly the language designers must make a
trade-off.  The choice of Scheme, and in turn Racket, is to keep the core
language syntax minimum but leaves open the possibility to extend it.  This
follows the design philosophy stated in the Scheme language report:

@emph{Programming languages should be designed not by piling feature on top of
feature, but by removing the weaknesses and restrictions that make additional
features appear necessary.} @note{This passage has been replicated at the very
beginning of every @emph{Revised@superscript{n} Report on the Algorithmic
Language Scheme} since n = 3.}

Scheme has only a few syntactic keywords, two of them are @racket[lambda] and
@racket[if].  They render the primitive syntax of the language.  Of course,
with them, it can hardly be convenient to program anything in the language.
For example, to write a multi-conditional expression, you have to write deep
nested @racket[if]-expressions.  Fortunately Scheme provides a powerful macro
system.  New macros can be defined on the fly.  Moreover, a macro call is
indistinguishable from a special form, due to the uniform s-expression syntax.
@note{In parallel, in Scheme and Racket, the call of a user-defined function
is also indistinguishable from that of a primitive function.  This is not by
coincidence but by intention, intention for a simple, uniform look and feel.}
This gives us a feeling that we are extending the core language syntax in a
compatible way, which means we do not introduce some alien syntax.  Since
macros can be used to extend the language syntax, they are sometimes also
called @emph{syntactic extensions}.  Actually, Racket goes much further than
Scheme, everything is a macro, including all seeming syntactic keywords, even
@racket[lambda] and @racket[if].  Their uses are replaced by code in Racket's
core language syntax during macro expansion. @note{The way how Racket's macro
expansion works is out of the scope of this lecture.  If you are interested,
please refer to the Racket documentation.} At the end of this subsection, we
use one example to demonstrate how the extensibility macros bring to the
language can influence its design.

We have claimed that everything in Racket is a macro.  Then it should be easy
to accept that @racket[match], the construct we have focused on in our
previous lecture is also just a macro.  Racket chooses to build @racket[match]
as a macro on top of equality test on literals and isomorphism test on data
structures.  Another perspective is to view pattern matching as a more general
and more primitive mechanism that should be supported out of the core
language.  The reason turns out to be that the most basic conditional
construct @racket[if] can be simply defined as a macro on top of pattern
matching!  Here you go:

@racketblock[
(define-syntax-rule (if test-expr then-expr else-expr)
  (match test-expr
    [#t then-expr]
    [#f else-expr] ) )
]

Thus every conditinoal expression is essentially a pattern matching
expression.

@subsection{The Use or Abuse of Macros}

Macro system holds the power to create languages and fill them with syntax.
But like all great power, it can be used for good, also for evil.  And so
arises the question, a question that will trap our mind until it finds the
balance.

In this final subseciton, we will uncover the mask of @racket[and] and
@racket[or].  They are not functions but macros.  Thus you can not pass them
as an argument to a higher-order function, since they are not functions at
all.  The reason why they are defined as macros instead of functions is
deeper.  In the following discussion, we focus on @racket[and], the case for
@racket[or] is similar.  Below is a function definition that can do what
@racket[and] can:

@racketblock[
(define (andf . bs)
  (match bs
    [(list) true]
    [(list b) b]
    [(list b bs ...) (if b (andf bs) false)] ) )
]

This function looks correct.  The problem with it is that before entering the
function body, all arguments have already been evaluated.  This is due to how
Racket evaluates an application: arguments to a function are always evaluated
before the evaluation of the function body.  For regular functions, this
evaluate strategy is demanded, since it will save repeated evaluation is the
arguments have to be substituted for several occurences of their corresponding
parameters inside the function body, improving the efficiency of the
computation.  But for @racket[andf], it causes inefficiency because it
conflicts with the so-called @emph{short-circuit} evaluation for
@racket[andf]: whenever we find an argument to @racket[andf] evaluates to
@racket[false], we can stop and return @racket[false] immediately,
disregarding all other arguments, no matter how many they are.  Now comes the
conflict: Racket's evaluation strategy requires all arguments to a function
being evaluated; short-circuit evaluation suggests that all arguments should
only be evaluated if necessary.  Instead of changing the regular evaluation
strategy for regular function application, which is the common case, Racket  
takes away the function status of @racket[andf] and turn it into a macro:

@racketblock[
(define-syntax and
  (syntax-rules ()
    [(and) true]
    [(and e) e]
    [(and e es ...) (if e (and es ...) false) ] ) )
]

Since a macro takes its argument expressions unevaluated, the conflict
disappears, and it can embrace the benefit of short-circuit evaluation.  On
the other hand, it also means @racket[and] can no longer be used in place
where first-class function status is required.  In particular, @racket[and]
can never see the light of run-time again.

@exercise[] Give the macro definition of @racket[or].

We leave this usage of macros to you to decide whether it is use or abuse.
The general advice is this: whenever you want to build abstractions, try
functions first; only if one of situations described above occurs, should you
try macros instead.  Remember, macros are powerful, so powerful if you abuse
them, you may shoot yourself in the foot.

