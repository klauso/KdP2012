#lang scribble/manual

@(require "scribbleUtilities.rkt")

@title[#:version ""]{Pattern Matching}

When manipulating data, we almost always need to access their components (if
any).  This is exactly why our design recipe recommends that when you define a
function that manipulates some kind of data, compose a template containing
code fragments that extract the components of the input data.  The essential
of these code fragments, the parts that actually perform the extraction, are
the so-called @italic{accessor functions}, or simply @italic{accessors}.  The
disadvantage of using accessors is that it often results in more verbose and
thus less readable code, especially when the data is very complex.  To handle
complex data more gracefully, we need @italic{pattern matching}.

Pattern matching is a technique that allows us to examine the form of data.
Through the examination, we gain access to the components (if any) of data.
This is achieved by constructing a pattern with the form of some expected
data, but with the interesting components replaced by variables.  These
variables can be mentioned in an expression that comes together with the
pattern.  The form of the pattern determines the form of the data that can
match.  When they match, the variables in the pattern are bound to the
corresponding components in the data; and the expression associated to the
pattern can be evaluated in the scope of these bindings.  Otherwise, the
expression is not evaluated.  Thus a pattern serves as a guard of its
associated expression.  Usually there are other pattern-expression pairs.  In
case one pattern fails to match, other pattern-expression pairs are tried.

Using pattern matching, we can write neat code, code that is clean, clear and
concise.  Racket supports pattern matching on a huge set of data.  We will
cover the essential, including matching literals and matching data structures,
both built-in and user-defined.  Then we will uncover the magic behind
functions like @racket[+] that can accept an arbitrary number of arguments.
As you may guess, it has something to with pattern matching, but on a limited
form.

@section{Matching Literals}

@subsection{Motivating Example 1 --- Debitalization}

We want to @italic{debitalize} the world to see the @italic{truth}!

In the bital world, @italic{bit} (short for @italic{binary digit}) is the
smallest information unit.  A bit can be either @bold{0} or @bold{1}.  In the
logical world, @italic{truth} is the most fundamental concepts.  A truth value
can be either @bold{true} or @bold{false}.  Despite their superficial
difference, these two worlds are so closely related, especially when we
interpret @bold{0} as @bold{false} and @bold{1} as @bold{true}.  This is what
we mean by @italic{debitalization}: translating a bit into a truth value,
@bold{0} to @bold{false} and @bold{1} to @bold{true}.

We want to write a Racket program to do the translation.  Bit can be
represented just as number @racket[0] or @racket[1]; truth can be represented
by boolean @racket[false] or @racket[true].  Now we can define a function that
debitalizes a bit.

@#reader scribble/comment-reader
(racketblock
;; debit : number -> boolean
;; debitalizes a bit, via equality test
(define (debit b)
  (cond [(= b 0) false]
        [(= b 1) true] ) )
)

In addition to the one-to-one mapping between bit and truth, there are also
correspondences between the fundamental operators in both worlds.  In the
bital world, these are the @bold{NOT}, @bold{AND} and @bold{OR} operators.
@bold{NOT} negates an input bit, inverting @bold{0} to @bold{1} and input
@bold{1} to @bold{0}.  @bold{AND} conjoins two input bits, resulting @bold{1}
if and only if both inputs are @bold{1}.  @bold{OR} disjoins two input bits,
returning @bold{0} if and only if both inputs are @bold{0}.  If you substitute
@bold{false} for @bold{0} and @bold{true} for @bold{1} in the preceeding
description, you can see it specifies exactly the logical ¬, ∧ and ∨ operators
on truth values.  According to the specification for the three bit operators,
we can easily define three Racket functions that act the same.

@#reader scribble/comment-reader
(racketblock
;; NOT : number -> number
;; negates a bit, via pattern matching
;; ...

;; AND : number number -> number
;; conjoins two bits, via equality test
(define (AND b1 b2)
  (cond [(and (= b1 0) (= b2 0)) 0]
        [(and (= b1 0) (= b2 1)) 0]
        [(and (= b1 1) (= b2 0)) 0]
        [(and (= b1 1) (= b2 1)) 1] ) )

;; OR : number number -> number
;; disjoins two bits, via equality test
;; ...
)

@exercise[]  Supply the definitions for @racket[NOT] and @racket[OR].

With these three functions, we can simulate any arbitrary complicated
operations in the bital world.  After we get the final result of the
simulation, we can readily see the truth by invoking the @racket[debit]
function to translate the result.  For example,

@racketblock[
(debit (NOT (OR (AND 0 1) (AND 1 0))))
]

Let us pause for a moment to have a look at the functions we have defined.
They all involve @italic{equality} tests.  In particular, we have to write
down these equality tests explicitly.  In @racket[debit], they are just a few.
But in @racket[AND], there seems too many than necessary.  Indeed, we can
rewrite @racket[AND] as follows:

@#reader scribble/comment-reader
(racketblock
;; AND : number number -> number
;; conjoins two bits, via equality test
(define (AND b1 b2)
  (cond [(= b1 0) 0]
        [(= b1 1) (cond [(= b2 0) 0]
                        [(= b2 1) 1] ) ] ) )

;; OR : number number -> number
;; disjoins two bits, via equality test
;; ...
)

@exercise[]  Rewrite your @racket[OR] definition if you also use too many equality
tests.

In this version, we have decreased the number of equality tests by half.
Though, can we do better?  In other words, can we write even less but still
maintain the behavior of the function.  The answer is positive.  With pattern
matching, we can indeed write less but say the same.  Let us see how
@racket[debit] would be defined using the @racket[match] form provided by
Racket.

@#reader scribble/comment-reader
(racketblock
;; debit : number -> boolean
;; debitalizes a bit, via pattern matching
(define (debit b)
  (match b
    [0 false]
    [1 true] ) )
)

You see instead of a @racket[cond]-expression, we now have a
@racket[match]-expression.  A @racket[match]-expression is similar to a
@racket[cond]-expression in structure except that:

@itemlist[#:style 'ordered
@item{It also takes another expression.  In the two examples both are the
variable @racket[b].  But in general any expression is allowed. }
@item{The left of a clause (enclosed in brackets @litchar|{[}| @litchar|{]}|)
is now a @italic{pattern} instead of a boolean expression. }
]

A pattern renders the form of some data.  For the moment, a pattern can be any
Racket literal, like a boolean, a number, a character, a string, etc.  Later
we will see a pattern can also describle a data structure.  The right part of
a clause can be any expression, as with that in a @racket[cond]-expression.

A @racket[match]-expression first evaluates the expression it takes, then
tries to match the value of the expression against the pattern of each clause
in sequence.  On the first pattern it matches, the expression corresponding to
the pattern is evaluated and its result is returned as the result of the whole
@racket[match]-expression.  The behavior is similar to that of a
@racket[cond]-expression.

Basically, when a pattern is a literal, you can think of pattern matching as
equality tests.  But rather than say "is equal to", we say "matches".  For
example, you can read the @racket[match]-expression as "@racket[match] the
value of expression @racket[b] against @racket[0], if it matches, return
@racket[false]; otherwise, @racket[match] it against @racket[1], if it
matches, return @racket[true]."  Now comes the question, what if it also fails
to match @racket[1]?  Then Racket throws out an error.  For example, try to
evaluate @racket[(debit 2)].

In general, when you construct a @racket[match]-expression, you should make
sure that you take all possible cases into account and provide at least one
pattern that covers each case, just as you did before with a
@racket[cond]-expression.

@exercise[]  Write the pattern-matching version of @racket[NOT] using the
@racket[match] form.

A @racket[match]-expression is like any other expression.  That is, it can
appear anywhere a normal expression is expected.  In particular, it can appear
as the expression associated to a pattern inside another
@racket[match]-expression.  In this case, we say we have @italic{nested}
@racket[match]-expressions, just as we can have nested
@racket[cond]-expressions.  @racket[AND] can be rewritten using nested
@racket[match]-expressions.

@#reader scribble/comment-reader
(racketblock
;; AND : number number -> number
;; conjoins two bits, via pattern matching
(define (AND b1 b2)
  (match b1
    [0 0]
    [1 (match b2
         [0 0]
         [1 1] ) ] ) )

;; OR : number number -> number
;; disjoins two bits, via pattern matching
;; ...
)

@exercise[]  Write the pattern-matching version of @racket[OR].

@section{Matching Data Structures}

@subsection{Matching Built-in Data Structures}

Debitalizing a single bit is just the first step.  Massive information flows
in the bital world like streams.  When faced with a bit stream, we'd better
have a function that can translate it into a truth stream.  We can represent a
bit stream as a list of bits, for example, @racket[(list 0 1 1 0)].  Then on
top of @racket[debit], we can easily define a new function that can debitalize
a bit stream.

@#reader scribble/comment-reader
(racketblock
;; debits : (listof number) -> (listof boolean)
;; debitalizes a bit stream, via isomorphism test
(define (debits bs)
  (cond [(empty? bs) bs]
        [else (cons (debit (first b))
                    (debits (rest bs)) ) ] ) )
)

Correspondingly, in the bital world, there must be operators that can
manipulate bit streams.  Naturally they are based on the three basic
operators.  Each of them operates on a bit stream or two bit streams bit by
bit.  Thus they are also called bit-wise operators.  Similar to the case of
debitalization, functions simulating them can also be defined on top of those
functions that simulate single-bit operators.

@#reader scribble/comment-reader
(racketblock
;; NOTs : (listof number) -> (listof number)
;; bit-wise negates a bit stream, via isomorphism test
;; ...

;; ANDs : (listof number) (listof number) -> (listof number)
;; bit-wise conjoins two bit streams, via isomorphism test
(define (ANDs bs1 bs2)
  (cond [(or (empty? bs1) (empty? bs2)) empty]
        [else (cons (AND (first bs1) (first bs2))
                    (ANDs (rest bs1) (rest bs2)) ) ] ) )

;; ORs : (listof number) (listof number) -> (listof number)
;; bit-wise disjoins two bit streams, via isomorphism test
;; ...
)

@exercise[]  Supply the definitinos for @racket[NOTs] and @racket[ORs].

These definitions are all defined by recursion on the built-in list data
structure by the so-called @italic{isomorphism} tests.  Two data structures
are said to be @italic{isomorphic} if they have the same form.  Again, the
body of @racket[debits] looks still not that bad.  But the body of
@racket[ANDs] is cluttered by the accessor functions that decompose the list.
One way to avoid this clutter is by moving the decompositions into the binding
part of a @racket[let]-expression as shown below.

@#reader scribble/comment-reader
(racketblock
;; ANDs : (listof number) (listof number) -> (listof number)
;; bit-wise conjoins two bit streams, via isomorphism test
(define (ANDs bs1 bs2)
  (cond [(or (empty? bs1) (empty? bs2)) empty]
        [else (let [(b1 (first bs1))
                    (bs1 (rest bs1))
                    (b2 (first bs2))
                    (bs2 (rest bs2)) ]
                (cons (AND b1 b2)
                      (ANDs bs1 bs2) ) ) ] ) )
)

@exercise[]  Rewrite @racket[debits] and @racket[ORs] in the same way.

This version is clearer but feels heavy.  Here comes is the same question that
whether we can do it by saying less.  Yes, pattern matching helps again.
Racket supports pattern matching on all built-in data structures, like pairs,
lists, vectors, etc.  Let us first see how the simpler @racket[debits] can be
rewritten using pattern matching.

@#reader scribble/comment-reader
(racketblock
;; debits : (listof number) -> (listof boolean)
;; debitalize a bit stream, via isomorphism test
(define (debits bs)
  (match bs
    [(list) bs]
    [(list b bs ...) (cons (debit b)
                           (debits bs) ) ] ) )
)

Things new in the @racket[match]-expression are the patterns: @racket[(list)]
and @racket[(list b bs ...)].  You see the syntax for patterns in Racket tries
to mimic the actual form of the expected data as much as possible.  This is
exactly the meaning of @italic{isomorphim} we explained above.  But be aware
that patterns in Racket are never evaluated.  They serve only as
specifications for the form of some expected data.  So the pattern
@racket[(list)] is not evaluated, nor is the pattern @racket[(list b bs ...)].
The former happens to be a valid expression (when evaluated it will return an
empty list).  It specifies that only an empty list can match.  The latter is
even not a valid expression.  It says only a non-empty list can match; and in
particular, when a non-empty list matches, its first element and its remaining
elements (as a list) will be bound respectively to the variable @racket[b] and
@racket[bs] for use in the expression @racket[(cons (debit b) (debits bs))].
An ellipsis @racket[...] is allowed in a list pattern and usually follows a
variable.  It indicates that the variable preceeding it can match a list of
zero or more elements.  For example, the pattern @racket[(list x ...)] can
match any list (empty or not), and the list will be bound to the variable
@racket[x].  In the pattern @racket[(list b bs ...)], since there is another
variable @racket[b] before @racket[bs ...], together the whole pattern
specifies that it can match a list of at least one element.  All in all, the
whole @racket[match]-expression can be understood in terms of the version
using @racket[cond] to test isomorphism and @racket[let] to bind the
components extracted by the relevant accessor functions.

You may have noticed that a variable in a pattern seems to be able to match
anything.  The reason is that a variable has no structure.  It acts as a
placeholder.  It is used only to name some components inside some matching
data.  In particular, if a variable appears alone as a pattern (not inside any
data structure), matching will succeed immediately with whatever bound to the
variable.  Be careful with its powerful matching ability.  If you accidentally
placed a single variable pattern before any other pattern, matching can never
reach other patterns.  For example, in the following function,

@racketblock[
(define (always-no order)
  (match order
    [whatever "No, Sir!"]
    ["Do it!" "Yes, Sir!"] ) )
]

you will never reply "Yes, Sir!" because @racket[whatever] the @racket[order]
is, you reply immediately "No, Sir!".  This is dangerous!  So make sure that
is what you intended to do.  Sometimes, this kind of behavior is required.  In
particular, you may just want a dummy variable to match anything but ignore
that thing.  For this purpose, Racket provides a special identifier, the
underscore @litchar|{_}|, usually called a @italic{wildcard}.  It acts as a
pattern variable, that is, it can match anything, but it does no binding.
Anything that matches is ignored.  For example, we can use it to define the
function that returns the length of a list as follows:

@racketblock[
(define (len l)
  (match l
    [(list) 0]
    [(list _ xs ...) (+ 1 (len xs)) ] ) )
]

Since we do not care about what an element is, in particular, we we will not
mention any element in the expression @racket[(+ 1 (len x))], we use the
underscore in the pattern.  When an underscore @litchar|{_}| is used alone,
not in any data structure, it plays the role of a default clause, just like an
@racket[else] clause in a @racket[cond]-expression, for example:

@racketblock[
(define (bit? b)
  (match b
    [0 "yes"]
    [1 "yes"]
    [_ "no"] ) )
]

One more thing to bear in mind is that the identifier @racket[list] appearing
in the head position of a list pattern is not a function name.  This can be
seen as an explanation for the claim that patterns are never evaluated.  We
will soon see patterns for other data structures.  In these patterns, there
are always one or more these kind of special names.  A good way to understand
their roles is by thinking of them as literals.  It is important not to
confuse them with variables in a pattern.  They are used for identifying a
particular type of data, thus @racket[list] for a list.

@exercise[]  Rewrite @racket[NOTs] using pattern matching.

As before, @racket[match]-expressions can be nested.  Back to our
debitalization example, we can rewrite @racket[ANDs] using nested
@racket[match]-expressions.

@#reader scribble/comment-reader
(racketblock
;; ANDs : (listof number) (listof number) -> (listof number)
;; bit-wise conjoins two bit streams, via isomorphism test
(define (ANDs bs1 bs2)
  (match bs1
    [(list) bs1]
    [(list b1 bs1 ...)
     (match bs2
       [(list) bs2]
       [(list b2 bs2 ...) (cons (AND b1 b2)
                                (ANDs bs1 bs2) ) ] ) ] ) )
)

@exercise[]  Rewrite @racket[ORs] in the same way.

Since list is such an important data structure.  Racket supports rich patterns
around it.  Here are more examples.

@eg[
(match (list 1 2)
  [(list 0 1) "one"]
  [(list 1 2) "two"] )
]

@eg[
(match (list 1)
  [(list x) (+ x 1)]
  [(list x y) (+ x y) ] )
]

@eg[
(match (list 1 2)
  [(list x) (+ x 1)]
  [(list x y) (+ x y)] )
]

@eg[
(match (list 2 2)
  [(list x x) "equal"]
  [_ "not equal"] )
]

@eg[
(match (list 2 3)
  [(list x x) "equal"]
  [_ "not equal"] )
]

The last two examples show that Racket even supports the so-called
@italic{non-linear} patterns.  In these kind of patterns, variables can appear
multiple (so @italic{non-linear}) times rather than just once (@italic{linear}
time).  In the two examples, the pattern variable @racket[x] appears twice.
But be aware that using the same name for two different things is usually the
origin of confusion.  You would better avoid it.

The ellipsis @racket[...] in Racket is a powerful utility to build very
general patterns.  Here are two examples.

@eg[
(match (list 1 2 2 2 3 4)
  [(list 1 2 ... 3 4) "ones and two"]
  [else "other"] )
]

@eg[
(match (list 1 2 2 2 3 4)
  [(list 1 x ... 3 4) x]
  [else "other"] )
]

Try to play with it to see how expressive you can be.  Constructing patterns
is fun and art.  The philosophy of pattern matching is accessing data by
constructing pattern instead of defining accessors.  Sometimes, a clever
pattern is all what you need to get access to the interesting components of a
complex data structure.  Be constructive!  Be creative!

@subsection{Matching User-defined Data Structures}

In addition to built-in data structures, Racket also support pattern matching
on user-defined data structures via using @racket[struct] or
@racket[define-struct].  We will illustrate its usage through a second
motivating example, poker cards.

@subsubsection{Motivating Example 2 --- Poker}

Every poker card has a rank and a suit.  The rank of a card is one of the
numbers from @bold{2} to @bold{10}, or one of the symbols @bold{A}
(@bold{Ace}), @bold{J} (@bold{Jack}), @bold{Q} (@bold{Queen}) and @bold{K}
(@bold{King}).  The suit of a card is one of the the symbols ♠ (@bold{Spade}),
♥ (@bold{Heart}), ♦ (@bold{Diamond}), and ♣ (@bold{Club}).

We can represent a poker card as a @racket[struct].

@#reader scribble/comment-reader
(racketblock
(define-struct card (rank suit))
;; rank : "2", "3", "4", "5", "6", "7", "8",
;;        "9", "10", "J", "Q", "K", "A"
;; suit : "♠", "♥", "♦", "♣"
)

For simplicity, we use string uniformly for a rank, insted of a mixture of
numbers and strings.  A suit is also represented as a string.

Let us first define a function that echoes the name of a rank.

@#reader scribble/comment-reader
(racketblock
;; rank-name : string -> string
;; echoes the name of a rank
(define (rank-name r)
  (match r
    ["A" "Ace"]
    ["J" "Jack"]
    ["Q" "Queen"]
    ["K" "King"]
    [_   r] ) )
)

This time, we define the function directly by pattern matching on the input
string.  Imagine what you would have to do without pattern matching.  Note
that how the underscore @litchar|{_}| is effectively used to match all other
ranks, that is numbers.  Since these numbers are in string forms, we return
them immediately.

@exercise[]  Define a function @racket[suit-name] that echoes the name of a suit.

A poker card is usually called by first saying its rank name, followed by the
word "@bold{of}" and then the plural form of its suit name.  For example, the
card 🂡 will be called "@bold{Ace of Spades}", the card 🃊 will be called
"@bold{10 of Diamonds}", and so on.  Fortunately the plural form for all the
four suit names are simply formed by appending an "@bold{s}".  We want to
write a function that echoes the name of a card.  With the aid of
@racket[rank-name] and @racket[suit-name] we can easily define it.  We will
first do it in the old way, that is, by invoking the accessor functions to
access the rank and suit of a card.  Here is the definition.

@#reader scribble/comment-reader
(racketblock
;; card-name : card -> string
;; echoes the name of a card
(define (card-name c)
  (string-append (rank-name (card-rank c))
                 " of "
                 (suit-name (card-suit c))
                 "s" ) )
)

Or we can first use @racket[let] to name the extracted components and rewrite
it like this.

@#reader scribble/comment-reader
(racketblock
;; card-name : card -> string
;; echoes the name of a card
(define (card-name c)
  (let [(r (card-rank c))
        (s (card-suit c)) ]
    (string-append (rank-name r)
                   " of "
                   (suit-name s)
                   "s" ) ) )
)

Having tasted the benefits of pattern matching, we are eager to see how it
will be done using pattern matching.  Here it is.

@#reader scribble/comment-reader
(racketblock
;; card-name : card -> string
;; echoes the name of a card
(define (card-name c)
  (match c
    [(struct card (r s))
     (string-append (rank-name r)
                    " of "
                    (suit-name s)
                    "s" ) ] ) )
)

We see again how the pattern @racket[(struct card (r s))] tries to mimic the
form of the @racket[card] data structure.  In this pattern, @racket[struct]
acts as a literal, indicating that the pattern is a user-defined data
structure pattern; @racket[card] is similar to the role of @racket[list] in a
list pattern, thus also like a literal, indicating that the actual data
structure expected is a @racket[card], not something else, say a
@racket[posn].  Two variables @racket[r] and @racket[s] hold the place for the
rank and suit of a card respectively.  As before, when a card matches, they
will be bound to the rank and suit of the card respectively for later use.
This example shows the way to construct a pattern for a user-defined data
structure.

@exercise[]  Revise functions that manipulate other data structures you have
defined, for example, @racket[posn], using pattern matching.

As a last example, we define a function that determines if two cards are the
same.  But this time we first pair the two cards and then do pattern matching
on them both in one place.

@#reader scribble/comment-reader
(racketblock
;; same-cards? : card card -> boolean
;; determines if two cards are the same
(define (same-cards? c1 c2)
  (match (cons c1 c2)
    [(cons (struct card (r1 s1))
           (struct card (r2 s2)) )
     (and (string=? r1 r2)
          (string=? s1 s2) ) ] ) )
)

Note that the expression to @racket[match] is now a @racket[cons]-ed pair of
the two input cards @racket[c1] and @racket[c2].  Since the expression is a
pair of two @racket[card]s, our pattern also need to a pair pattern, with two
sub-patterns for @racket[card].  This is exactly what @racket[(cons (struct
card (r1 s1)) (struct card (r2 s2)))] specifies.  Mind that in the pattern
@racket[cons] acts as a literal, indicating a pair pattern.

It seems we are trying to make things more complicated than necessary.  Also
this pairing and then de-pairing also introduces some extra overheads.  So why
are we bothering this intead of using nested @racket[match]-expressions.  The
reason is that sometimes, such nesting could go vera deep, which makes the
code less readable.  For @racket[same-cards?], it is still not that obvious
since it only accepts two arguments.  We only need to nest one
@racket[match]-expression inside another.

@#reader scribble/comment-reader
(racketblock
;; same-cards? : card card -> boolean
;; determines if two cards are the same
(define (same-cards? c1 c2)
  (match c1
    [(struct card (r1 s1))
     (match c2
       [(struct card (r2 s2)) (and (string=? r1 r2)
                                   (string=? s1 s2) ) ] ) ] ) )
)

But imagine that we now want to define a function that takes three cards and
determine if all of them are the same.  Then we have to nest three
@racket[match]-expressions.  If you still think it managable, imagine four,
five or even more.  In these cases the advantage of our technique manifests,
even though it means we have to sacrifice some efficiency.  For example, we
can define the function that determines the sameness among three cards as
follows:

@#reader scribble/comment-reader
(racketblock
;; same-3-cards? : card card card -> boolean
;; determines if three cards are the same
(define (same-3-cards? c1 c2 c3)
  (match (list c1 c2 c3)
    [(list (struct card (r1 s1))
           (struct card (r2 s2))
           (struct card (r3 s3)) )
     (and (string=? r1 r2) (string=? r2 r3)
          (string=? s1 s2) (string=? s2 s3) ) ] ) )
)

This time we construct a list from the three input cards and use the list
pattern instead.  The transitivity of @racket[string=?] guarantees that once
two different pairs of strings are equal, all the three strings are equal.

@exercise[]  Rewrite the pattern-matching version of @racket[AND], @racket[OR],
@racket[ANDs] and @racket[ORs], do not use nested @racket[match]-expressions.

@exercise[]  Read the Wikipedia article
@hyperlink["http://en.wikipedia.org/wiki/List_of_poker_hands"]{List of poker
hands}.  Suppose we represent a hand as a list of 5 cards, for each poker hand
category, write a function that tests whether a hand of cards belong to that
category.  You can find those functions that you may use to accomplish the
task in the file
@hyperlink["https://github.com/klauso/KdP2012/tree/master/underconstruction/card.rkt"]{card.rkt}.

@section{Matching Argument Lists}

The last examples in the previous section shows that sometimes we want the
arguments to a function collected in some data structure so that we can handle
them, for example, do pattern matching on them, in one place.  Of course, we
can do this by ourselves, as we did in those examples.  But it would be better
if it is supported out of the box.  Racket does support it.  In Racket, when a
function is passed in its arguments, it implicitly receives the arguments as a
list.  This is how a function sees its arguments under the hood.  Racket
provides ways that allow us to open the hood: getting the argument list.
Through this explicit approach, we can handle the arguments collectively
rather than individually.

@subsection{An Explicit Way to Apply a Function}

So far, we apply a function by simply enclosing the function and its arguments
in a pair of parentheses @litchar|{(}| @litchar|{)}|.  For example, we write
@racket[(+ 1 2 3)], @racket[(length (list 1 2 3))], and so on.  As an
alternative, Racket also provides a function @racket[apply] that can do the
same job.  It accepts a function you want to apply and a list of arguments you
want to apply the function to, and, as its name suggests, apply the function
to the arguments collected in the list.  For example, instead of @racket[(+ 1
2 3)], we can write @racket[(apply + (list 1 2 3))].  As you can see, since we
want to apply @racket[+] to the arguments @racket[1], @racket[2] and
@racket[3], we first collect them in a list @racket[(list 1 2 3)] and then
feed the list together with the function @racket[+] to the function
@racket[apply].  The two expressions, @racket[(+ 1 2 3)] and @racket[(apply +
(list 1 2 3))] have the same effect.  That is, they both evaluates to
@racket[6].

Rewriting @racket[(length (list 1 2 3))] using @racket[apply] is a little
tricky.  It is easy to put it like @racket[(apply length (list 1 2 3))].  But
this is wrong.  Recall that @racket[length] accepts a @italic{single} argument
which must be some list and returns the length of that input list.  Since the
second argument to the function @racket[apply] is supposed to be a list that
contains all the arguments we want to feed to the function we want to apply,
we should first put the single argument to @racket[length] in a list.  That
is, if we want to apply @racket[length] to the list @racket[(list 1 2 3)], we
should first put this list in a list, @racket[(list (list 1 2 3))], then
supply it with @racket[length] to @racket[apply].  Thus the correct answer is
@racket[(apply length (list (list 1 2 3)))].  Again, evaluating both
expressions should give the same result, @racket[3].

This more explicit way to apply a function seems redundant.  Indeed for most
senarios, you do not need it and you are even encouraged not to use it.  If
you want to apply a function, do it in the usual, more direct way.  So, write
@racket[(+ 1 2 3)] instead of @racket[(apply + (list 1 2 3))], @racket[(length
(list 1 2 3))] instead of @racket[(apply length (list (list 1 2 3)))], for the
sake of readability.  Though, this indirect way to apply a function by
invoking @racket[apply] on the function and the argument list, fancy or
clumsy, has its use.  It is one essential part of the magic behind variadic
functions, those that can accept an arbitrary number of arguments.

@subsection{The Magic behind Variadic Functions}

You have been curious about how @racket[+] can accept an arbitrary number of
arguments for long.  These kind of functions are called variadic functions.
Their existence contributes a lot to the brevity of Racket expressions.
Suppose that @racket[+] can only accept two arguments, to add three numbers,
say @racket[1], @racket[2] and @racket[3], we have to write it (in one
possible way) as @racket[(+ 1 (+ 2 3))].  If this still looks not bad, imagine
how many plus signs and parentheses you have to write to add ten numbers.  But
with @racket[+] being variadic, to add ten numbers, say from @racket[1] to
@racket[10], we simply write @racket[(+ 1 2 3 4 5 6 7 8 9 10)].  The question
is of course how it works.  And more importantly, can we define a function
like this too?  The answer is a certain "yes".

The idea is to identify the sequence of arguments fed to a function as a list.
Since a list could be arbitrarily long, it means the arguments could be
arbitrarily many.  With this identification, the eseential turns out to be ---
when defining a function, we need a mechanism, one that would allow us to get
hold of the list of arguments fed to the function when it is applied (in the
usual direct way).  The simplest way in programming to get hold of something
is by naming the thing.  That said, we need only a varible.  The problem is
the ordinary syntax for function definition is no longer sufficient for this
task.  To see the point, let us see the header of an example function
definition (what it does actually is of little concern here).

@#reader scribble/comment-reader
(racketblock
(define (f x y)
  ; body
  )
)

Apparently the function @racket[f] can accept three arguments since following
its name are three variables @racket[x], @racket[y].  Without loss of
generality, let us further assume that the function @racket[f] expectes two
numbers as arguments.  Suppose @racket[f] is applied to numbers @racket[1]
and@racket[2], again in the usual direct way, that is, @racket[(f 1 2)].
According to our knowledge, each variable will be bound to one argument in
order, so @racket[x] to @racket[1] and @racket[y] to @racket[2].  Now it is
clear that there is no way in this old syntax that would allow us to express
that binding one varible to the whole list of arguments.  In particular,
inserting an extra variable in any valid position to the header does not work
since the resulting definition

@#reader scribble/comment-reader
(racketblock
(define (f x y z)
  ; body
  )
)

defines a function that can accept three arguments.  When it is applied, one
variable still gets bound to one argument correspondingly.  What we need is a
special syntax that allows us to specify a variable intended to be bound to
the whole argument list when the function is applied.  The Racket syntax to do
so is by prefixing this particular variable a dot @litchar|{.}|.  So we write
it like this:

@#reader scribble/comment-reader
(racketblock
(define (f . xs)
  ; body
  )
)

The function @racket[f] defined thus can accept an arbitrary number of
arguments.  In particular, we know that when it is applied, all the arguments
fed to it will be collected in a list and the variable @racket[xs] will be
bound to this argument list.  With this knowledge, in the body of the function
definition we can case analyze @racket[xs] and do things accordingly.

Now we have all the tools in hand to define a variadic function.  Let us
define a function that sums an arbitrary number of numbers.  This is indeed
what @racket[+] does.  But in order to show you how you can do the same thing,
let us again suppose that @racket[+] can only add two numbers.  The definition
of @racket[sum] goes as follows.

@#reader scribble/comment-reader
(racketblock
;; sum : number ... -> number
;; sums an arbitrary number of numbers
(define (sum . ns)
  (cond [(empty? ns) 0]
        [else (+ (first ns)
                 (apply sum (rest ns)) ) ] ) )
)

You see we do case analysis on the argument list.  If it is empty, we return
@racket[0].  If it is not, we first take out the first argument of the list,
then apply @racket[sum] recursively to the rest of the argument list, and
finally @racket[+] the first number to the result of the result returned from
the recursive application.  The surprise is that the recursive application is
done by invoking @racket[apply], instead of applying @racket[sum] to the rest
of the argument list directly.  A moment's thought reveals that @racket[(sum
(rest ns))] indeed would not work.  Because what it actually does is supplying
the rest of the argument list, which is a list of numbers, as a single
argument to @racket[sum].  This is not what @racket[sum] expects.  Every
argument to @racket[sum] must be a number, instead of a list of numbers.  To
see more clearly why using @racket[apply] works, let us see a simple example,
say @racket[(sum 1 2 3)].  First, @racket[ns] will be bound to the the whole
argument list @racket[(list 1 2 3)]; then the list is checked to see if it is
empty, since apparently not, we reach the @racket[else] branch; substituting
in the result of @racket[(rest ns)] gives @racket[(apply sum (list 2 3))].
According to our description of @racket[apply], this expression has the same
effect as @racket[(sum 2 3)], which is what we want.

If you have appreciated the benefits of pattern matching, you should be
tempted to rewrite @racket[sum] using @racket[match].

@exercise[]  Write a function @racket[product] that calculates the product of an
arbitrary number of numbers using @racket[match].  Assume the function
@racket[*] can only accept two arguments.

At the end of the previous section, we hinted that sometimes we want to avoid
nested pattern matching for the sake of readability when manipulating several
data.  In particular, we showed how we could do that by first collecting these
data in in a pair or a list, and then accordingly provide a pair or list
pattern containing sub-patterns corresponding these data.  In that way, we can
do pattern matching in one place.  Our examples there particularly concerned
collecting arguments.  Now that we see that we do not have to do this argument
collection ourselves, we can rewrite, for example, @racket[same-cards?] like
this:

@#reader scribble/comment-reader
(racketblock
;; same-cards? : card card -> boolean
;; determines if two cards are the same
(define (same-cards? . cs)
  (match cs
    [(list (struct card (r1 s1))
           (struct card (r2 s2)) )
     (and (string=? r1 r2)
          (string=? s1 s2) ) ] ) )
)

@exercise[]  Rewrite @racket[AND], @racket[OR] and @racket[same-3-cards?] in a
similar way.

You can also put as many variables before the dot @litchar|{.}| in a function
header.  (But only one variable is allowed after it.)  For example,

@#reader scribble/comment-reader
(racketblock
(define (f x y . z)
  ; body
  )
)

will define a function that accepts at least two arguments.  The first two
arguments are mandatory and will be bound to @racket[x] and @racket[y]
respectively.  Then zero or more arguments can follow.  They will be collected
in a list and bound to @racket[z].  Since the list can be empty, these
arguments can be seen as optional.  Below is a simple exmaple showing its
usage.

@#reader scribble/comment-reader
(racketblock
;; greet-person : string string ... -> string
;; greets persons
(define (greet-person g . ps)
  (match ps
    [(list) (string-append g "!")]
    [(list p) (string-append g ", " p "!")]
    [(list p ps ...)
     (string-append g ", " p "! "
                    (apply greet-person (cons g ps)) ) ] ) )
)

We can use this function to produce greetings for an arbitrary number of
persons, for example, @racket[(greet-person "Hi" "Kos" "Tir" "Day")].  Notice
that in the recursive application, @racket[(apply greet-person (cons g ps))],
we need to @racket[cons] @racket[g] into @racket[ps], otherwise in the
recursive calls, @racket[greet-person] will no longer receive the first
argument it expects.  Try to see what will happen with the example if we
accidentally write just @racket[(apply greet-person ps)].

Though allowing variables before the dot @litchar|{.}| does offer some
convenience.  We can achieve the same effect without inserting any variable
before the dot @litchar|{.}|, as shown below.

@#reader scribble/comment-reader
(racketblock
(define (f . xs)
  (match xs
    [(list x y z ...) ; body
    ] ) )
)

The point is that we can see this feature as a limited form of pattern
matching.

