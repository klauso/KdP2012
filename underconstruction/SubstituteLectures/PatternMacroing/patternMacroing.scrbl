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
most deeply nested element, is @litchar{()}, @litchar{()} and the dot
@litchar{.} preceeding it can be completely ommited, so the last example can
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
to @racket["10"], @racket["J"], @racket["Q"] and @racket["K"]).  Apparently we
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
Because they are not.  If we say this whole header is a pattern, does it
remind you of something?  Yes, pattern matching, though is no longer on data
structures but on code structures.  The names that labelling holes in the
template are also called pattern variables.  Ellipses in the pattern act
similarly.  But be aware that you must make sure that if a name in the pattern
is followed (immediately or not) by some ellipses, exactly the same number of
ellipses must follow the name (immediately or not) in the template.
Otherwise, you will get an error complaining that ellipses are missing.  Check
yourself that our macro definition above satisfies this requirement.  To see
how it could fail, just delete one ellipsis in the pattern or the template to
make them not match.

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
definitions inside @racket[begin].

@subsection{Abbreviating Coding Patterns}

Not all code repetitions are as plain as that is show in the previous
subsection.  Sometimes we find that what we are repeating actually appears in
a structural form.  One typical example is nested @tt{if}-@tt{else}
statements found in C-family languages.  In programs written in these
languages, you can easily find code of the following code structure.

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

Programmers notice these kinds of code structures.  They call them coding
patterns or coding idioms.  They collect them into their coding dictionaries
and practice using them whenever possible.  One day when they become seasoned,
they teach these idioms or patterns to their disciples, and so on.  This is
after all how knowledge is usually conveyed.  But it is clear that the
recurence of these coding patterns is definitely a form of repetition.  Why
should we be forced to repeat them?  Why does not the programming language
provide some more convenient constructs to ease our coding task?  The answer
is that language designers cannot predict these coding patterns beforehand.
They emerge in our coding practice.  The crux of the problem is either the
programming language does not provide any or any sophisticated enough way that
allows the programmer to abbreviate or abstract over these coding patterns.
Lisp-family languages do, via their powerful macro systems.  So does Racket.

You have been using @racket[cond] for quite some programming tasks.  It proves
convenient.  But so far you have been told that it is a primitive control
construct. @note{@racket[cond] is indeed provided as a primitive control
construct in original Lisp.  But it is not in Scheme, neither in Racket.}  It
is actually not.  The primitive control construct to form a conditional
expression in Racket is @racket[if].  Now suppose, Racket does not provide
@racket[cond] but only @racket[if] out of the box, what would happen?  Does it
mean that you have to, as programmers of other languages do, identify nested
@racket[if] expressions as a coding pattern and fron now on write every
multi-conditional expression that way?  Fortunately not.  Racket allows us to
define our own @racket[cond] as a macro and use it as if it is provided out of
box.  Actually @racket[cond] is a pre-defined macro in Racket.

To see how @racket[cond] can be defined as a macro, let's start again with a
template.  The template should cover the coding pattern. So it must consists
of nested @racket[if] sub-templates.

@specform[
(if test-expr1
    then-expr1
    (if test-expr2
        then-expr2
        ... ) )
]

But the problem is that we cannot know in advance how deep the nesting could
be.  In particular the ellipsis seems 
 
@racket[cond], @racket[let]

@subsection{Extending Language Syntax}

@racket[when], @racket[do-while-else]

@subsection{The Use and Abuse of Macros}

@racket[and], @racket[or]

