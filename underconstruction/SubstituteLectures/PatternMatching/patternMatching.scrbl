#lang scribble/manual

@(require "scribbleUtilities.rkt")

@title{Pattern Matching}

When manipulating data, we almost always need to access their components (if any).
This is exactly why our design recipe recommends that when you define a function
that manipulates some kind of data, compose a template containing code fragments
that extract the components of the input data.  The essential of these code fragments,
the parts that actually perform the extraction, are the so-called accessor functions,
or simply accessors.  The disadvantage of using accessors is that it often results in
more verbose and thus less readable code, especially when the data is very complex.
To handle complex data, we need pattern matching.

Pattern matching is a technique that allows us to examine the form of data. Through
the examination, we gain access to the components (if any) of data.  This is achieved
by constructing a pattern with the form of some expected data, but with the interesting
components replaced by variables.  These variables can be mentioned in an expression
that comes together with the pattern.  Together with a pattern almost always comes an
expression that can mention these variables in the pattern.  The form of the pattern
determines only data with the same form can match the pattern.  When they match, the
variables in the pattern are bound to the corresponding components in the data; and then
the expression associated to the pattern can be evaluated in the scope of these bindings.
Otherwise, the expression is not evaluated.  Thus a pattern serves as a guard of its
associated expression.  Usually there are other pattern-expression pairs.  In case one
pattern fails to match, other pattern-expression pairs are tried.

Using pattern matching, we can write neat code, code that is clean, clear and concise.
Racket supports pattern matching on a huge set of data.  We will cover the essential,
including matching literals and matching data structures, both built-in and user-defined.
Then we will uncover the magic behind functions like @racket[+] that can accept an
arbitrary number of arguments.  As you may guess, it has something to with pattern
matching, but on a limited form.

@section{Matching Literals}

@subsection{Motivating Example 1 --- Debitalization}

We want to @italic{debitalize} the world to see the @italic{truth}!

In the bital world, bit is the smallest information unit.  A bit can be either 0 or 1.
In the logical world, truth is the most fundamental concepts.  A truth value can be either
"true" or "false".  Despite their superficial difference, these two worlds are so closely
related, especially when we interpret 0 as "false" and 1 as "true".  This is what we mean
by "debitalization": translating a bit into a truth value, 0 to "false" and 1 to "true".

We want to write a Racket program to do the translation.  Bit can be represented just as
number @racket[0] or @racket[1]; truth can be represented by boolean @racket[false] or
@racket[true].  Now we can define a function that debitalizes a bit.

@#reader scribble/comment-reader
(racketblock
;; debit : number -> boolean
;; debitalizes a bit, via equality test
(define (debit b)
  (cond [(= b 0) false]
        [(= b 1) true] ) )
)

In addition to the one-to-one mapping between bit and truth, there are also correspondences
between the fundamental operations in both worlds.  In the bital world, these are the NOT,
AND and OR operations.  NOT negates an input bit, inverting 0 to 1 and input 1 to 0.  AND
conjoins two input bits, resulting 1 if and only if both inputs are 1.  OR disjoins two
input bits, returning 0 if and only if both inputs are 0.  If you substitute "false" for 0
and "true" for 1 in the preceeding description, you can see it specifies exactly the logical
¬, ∧ and ∨ operations on truth values.  According to the specification for the three bit
operations, we can easily define three Racket functions that act the same.

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

@bold{Exercise.}  Supply the definitions for @racket[NOT] and @racket[OR].

With these three functions, we can simulate any arbitrary complicated operations in the bital
world.  After we get the final result of the simulation, we can readily see the truth by
invoking the @racket[debit] function to translate the result.  For example,

@racketblock[
(debit (NOT (OR (AND 0 1) (AND 1 0))))
]

Let us pause for a moment to have a look at the functions we have defined.  They all involve
equality tests.  In particular, we have to write down these equality tests explicitly.  In
@racket[debit], they are just a few.  But in @racket[AND], there seems too many than necessary.
Indeed, we can rewrite @racket[AND] as follows:

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

@bold{Exercise.}  Rewrite your @racket[OR] definition if you also use too many equality tests.

In this version, we have decreased the number of equality tests by half.  Though, can we do
better?  In other words, can we write even less but still maintain the behavior of the
function.  The answer is positive.  With pattern matching, we can indeed write less but say
the same.  Let us see how @racket[debit] would be defined using the @racket[match] form
provided by Racket.

@#reader scribble/comment-reader
(racketblock
;; number -> boolean
;; debitalizes a bit, via pattern matching
(define (debit b)
  (match b
    [0 false]
    [1 true] ) )
)

You see instead of a @racket[cond]-expression, we now have a @racket[match]-expression.  A
@racket[match]-expression is similar to a @racket[cond]-expression in structure except
@itemlist[
@item{It also takes another expression.  In the two examples both are the variable @racket[b].
      But in general any expression is allowed. }
@item{The left of a clause (enclosed in brackets) is now a pattern instead of a boolean
      expression.  For the moment, a pattern can be any Racket literal, like a boolean, a
      number, a character, a string, etc.  Later we will see a pattern can also be a data
      structure. }
]
The rest is similar.  The right part of a clause can also be any expression.

A @racket[match]-expression first evaluates the expression it takes under examination, then
tries to match the value of the expression against the pattern of each clause in sequence.
On the first pattern it matches, the expression corresponding to the pattern is evaluated and
its result is returned as the result of the whole @racket[match]-expression.  The behavior
is similar to that of a @racket[cond]-expression.

Basically, when a pattern is a literal, you can think of pattern matching as equality tests.
But rather than say "is equal to", we say "matches".  For example, you can read the
@racket[match]-expression as "@racket[match] the value of expression @racket[b] against
@racket[0], if it matches, return @racket[false]; otherwise, @racket[match] it against
@racket[1], if it matches, return @racket[true]."  Now comes the question, what if it also
fails to match @racket[1]?  Then Racket throws out an error.  For example, try to evaluate
@racket[(debit 2)].

In general, when you construct a @racket[match]-expression, you should make sure that you
take all possible cases into account and provide at least one pattern that covers each case,
just as you did before with a @racket[cond]-expression.

@bold{Exercise.}  Write the pattern-matching version of @racket[NOT] using the
@racket[match] form.

A @racket[match]-expression is like any other expression.  That is, it can appear anywhere a
normal expression is expected.  In particular, it can appear as the expression associated to
a pattern inside another @racket[match]-expression.  In this case, we say we have a nested
@racket[match]-expression, as we can have nested @racket[cond]-expression.  @racket[AND] can
can be rewritten using nested @racket[match]-expressions.

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

@bold{Exercise.}  Write the pattern-matching version of @racket[OR].

@section{Matching Data Structures}

@subsection{Matching Built-in Data Structures}

Debitalizing a single bit is just the first step.  Massive information flows in the bital
world like streams.  When faced with a bit stream, we'd better have a function that can
translates it into a truth stream.  We can represent a bit stream as a list of bits, for
example, @racket['(0 1 1 0)].  Then on top of @racket[debit], we can easily define a new
function that can debitalize a bit stream.

@#reader scribble/comment-reader
(racketblock
;; debits : (listof number) -> (listof boolean)
;; debitalize a bit stream, via isomorphism test
(define (debits bs)
  (cond [(empty? bs) bs]
        [else (cons (debit (first b))
                    (debits (rest bs)) ) ] ) )
)

Correspondingly, in the bital world, there must be operations that can manipulate bit
streams.  Naturally they are based on the three basic opreations.  Each of them operates
on a bit stream or two bit streams bit by bit.  Thus they are also called bit-wise
operations.  Similar to the case of debitalization, functions simulating them can also
defined on top of those functions that simulate single-bit operations.

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

@bold{Exercise.}  Supply the definitinos for @racket[NOTs] and @racket[ORs].

These definitions are all defined by recursion on the built-in list data structure by the
so-called isomorphism tests.  Two data structures are said to be isomorphic if they have
the same form.  Again, the body of @racket[debits] looks still not that bad.  But the body
of @racket[ANDs] is cluttered by the accessor functions that decompose the list.  One way
to avoid this clutter is by moving the decompositions into the binding part of a
@racket[let]-expression as shown below.

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

@bold{Exercise.}  Rewrite @racket[debits] and @racket[ORs] in the same way.

This version is clearer but feels heavy.  Here comes is the same question that whether we
can do it by saying less.  Yes, pattern matching helps again.  Racket supports pattern
matching all built-in data structures, like pairs, lists, vectors, etc.  Let us first see
how the simpler @racket[debits] can be rewritten using pattern matching.

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

Things new in the @racket[match]-expression are the patterns: @racket[(list)] and
@racket[(cons b bs)].  You see the syntax for patterns in Racket tries to mimic the actual
form of the expected data as much as possible.  This is exactly the meaning of "isomorphim"
we explained above.  But be aware that patterns in Racket are never evaluated.  They serve
only as specifications for the form of some expected data.  So the pattern @racket[(list)]
is not evaluated, nor is the pattern @racket[(list b bs ...)].  The former happens to be
a valid expression (when evaluated it will return an empty list) but specifies that only
an empty list can match.  The latter is even not a valid expression.  It says only a
non-empty list can match; and in particular, when a non-empty list matches, its first
element and its remaining elements (as a list) will be bound respectively to the variable
@racket[b] and @racket[bs] for use in the expression @racket[(cons (debit b) (debits bs))].
An ellipsis @racket[...] is allowed in a list pattern and usually follows a variable.  It
indicates that the variable preceeding it can match a list of zero or more elements.  For
example, the pattern @racket[(list x ...)] can match any list (empty or not), and the list
will be bound to the variable @racket[x].  In the pattern @racket[(list b bs ...)], since
there is another variable @racket[b] before @racket[bs ...], together the whole pattern
specifies that it can match a list of at least one element.  All in all, the whole
@racket[match]-expression can be understood in terms of the version using @racket[cond] to
test isomorphism and @racket[let] to bind the components extracted by the relevant accessor
functions.

You may have noticed that a variable in a pattern seems to be able to match anything.  The
reason is that a variable has no structure.  It acts as a placeholder.  It is used only to
name some components inside a matching data.  In particular, if a variable appears alone as
a pattern (not inside any data structure), the match will succeed immediately with whatever
bound to the variable.  Be careful with its powerful matching ability.  If you accidentally
misplaced a single variable pattern before any other pattern, matching can never reach other
patterns.  For example, in the following function,

@racketblock[
(define (always-no order)
  (match order
    [whatever "No, Sir!"]
    ["Do it!" "Yes, Sir!"] ) )
]

you will never reply "Yes, Sir!" because @racket[whatever] the @racket[order] is, you reply
immediately "No, Sir!".  It is dangerous!  So make sure that is what you intended to do.
Sometimes, this kind of behavior is required.  For this purpose, Racket provides a special
identifier, the underscore @racket[_].  It acts as a pattern variable, that is, it can match
anything, but it does no binding.  Anything that matches are ignored.  For example, we can
use it to define the function that returns the length of a list as follows:

@racketblock[
(define (len l)
  (match l
    [(list) 0]
    [(list _ xs ...) (+ 1 (len xs)) ] ) )
]

Since we do not care about what an element is, in particular, we we will not mention any
element in the expression @racket[(+ 1 (len x))], we use the underscore in the pattern.

One more thing to bear in mind is that the identifiers @racket[list] appearing in list
patterns are not function names, this can be seen as an explanation for the claim that
patterns are never evaluated.  A good way to understand their roles is by thinking of them
as keywords.  It is important to distinghuish them from variables in a pattern.  They are
used for identifying a particular type of data, thus @racket[list] for a list.

@bold{Exercise.}  Rewrite @racket[NOTs] using pattern matching.

As before, @racket[match]-expressions can be nested.  Back to our debitalization example,
we can rewrite @racket[ANDs] using nested @racket[match]-expressions.

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

@bold{Exercise.}  Rewrite @racket[ORs] in the same way.

Since list is such an important data structure.  Racket supports rich patterns around it.
Here are more examples.

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

The last example shows that Racket even supports the so-called non-linear patterns.  But
be aware that using the same names for two different things is usually the origin of
confusion.  You would better avoid it.

The ellipsis @racket[...] in Racket is a powerful utility to build very general patterns.
Here are two examples.

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

Try to play with it to see how expressive you can be to construct patterns.  Constructing
patterns is fun and art.  Sometimes, a clever pattern is all what you need to get access
to the interesting components of a complex data structure.  Be creative!

@subsection{Matching User-defined Data Structures}

In addition to built-in data structures, Racket also support pattern matching on user-defined
data structures defined using @racket[struct] or @racket[define-struct].  We will illustrate
its usage through a second motivating example, playing cards.

@subsubsection{Motivating Example 2 --- Playing Cards}

Every playing card has a rank and a suit.  The rank of a card is one of the numbers from 2 to
10, or one of the symbols A (Ace), J (Jack), Q (Queen) and K (King).  The suit of a card is
one of the the symbols ♠ (Spade), ♥ (Heart), ♦ (Diamond), and ♣ (Club).

We can represent a playing card as a @racket[struct].

@#reader scribble/comment-reader
(racketblock
(define-struct card (rank suit))
;; rank : "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"
;; suit : "♠", "♥", "♦", "♣"
)

For simplicity, we use string uniformly for a rank, insted of a mixture of numbers and strings.
A suit is also represented as a string.

Let us first define a function that echoes the name of a rank.

@#reader scribble/comment-reader
(racketblock
;; rank-name : string -> string
;; returns the name of a rank
(define (rank-name r)
  (match r
    ["A" "Ace"]
    ["J" "Jack"]
    ["Q" "Queen"]
    ["K" "King"]
    ["W" "White"]
    [_   r] ) )
)

This time, we define the function directly by pattern matching on the input string.  Imagine
what you would have to do without pattern matching.  Note that how the underscore @racket[_]
is effectively used to match all other ranks, that is numbers.  Since these numbers are in
string forms, we return them immediately.

@bold{Exercise.}  Define a function @racket[suit-name] that echoes the name of a suit.

A playing card is usually called by first saying its rank name, followed by the word "of" and
then the plural form of its suit name.  For example, the card 🂡 will be called "Ace of Spades",
the card 🃊 will be called "10 of Diamonds", and so on.  Fortunately the plural form for all
the four suit names are simply formed by appending an "s".  We want to write a function that
echoes the name of a card.  With the aid of @racket[rank-name] and @racket[suit-name] we can
easily define it.  We will first do it in the old way, that is, by invoking the accessor
functions to access the rank and suit of a card.  Here is the definition.

@#reader scribble/comment-reader
(racketblock
;; card-name : card -> string
;; returns the name of a card
(define (card-name c)
  (string-append (rank-name (card-rank r))
                 " of "
                 (suit-name (card-suit s))
                 "s" ) )
)

Or we can first use @racket[let] to name the extracted components and rewrite it like this.

@#reader scribble/comment-reader
(racketblock
;; card-name : card -> string
;; returns the name of a card
(define (card-name c)
  (let [(r (card-rank c))
        (s (card-suit c)) ]
    (string-append (rank-name r)
                   " of "
                   (suit-name s)
                   "s" ) ) )
)

Having tasted the benefits of pattern matching, we are eager to see how it will be done using
pattern matching.  Here it is.

@#reader scribble/comment-reader
(racketblock
(define (card-name c)
  (match c
    [(struct card (r s))
     (string-append (rank-name r)
                    " of "
                    (suit-name s)
                    "s" ) ] ) )
)

We see again how the pattern @racket[(struct card (r s))] tries to mimic the form of the
@racket[card] data structure.  In this pattern, @racket[struct] is a keyword as usual,
indicating that the pattern is a user-defined data structure pattern; @racket[card] is similar
to the role of @racket[list] in a list pattern, indicating that the actual data structure
expected is a @racket[card], not something else, say a @racket[posn].  Two variables @racket[r]
and @racket[s] hold the place for the rank and suit of a card respectively.  As before, when
a card matches, they will be bound to the rank and suit of the card respectively for later use.
This example shows the way to construct a pattern for a user-defined data structure.

@bold{Exercise.}  Construct a pattern for the @racket[posn] data structure or some others you
have seen or made.

As a last example, we define a functino that determines if two cards are the same.  But this
time we first pair the two cards and then do pattern matching on them both in one place.

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

Note that the expression to @racket[match] is now a @racket[cons]-ed pair of the two input
cards @racket[c1] and @racket[c2].  Since the expression is a pair of two @racket[card]s,
our pattern also need to a pair pattern, with two sub-patterns for @racket[card].  This is
exactly what @racket[(cons (struct card (r1 s1)) (struct card (r2 s2)))] specifies.  Mind
that in the pattern @racket[cons] acts as a keyword, indicating a pair pattern.

It seems we are trying to make things more complicated than necessary.  Also this pairing
and then de-pairing also introduces some extra overheads.  So why are we bothering this
way intead of using nested @racket[match]-expressions.  The reason is that sometimes, the
nesting could be extreemely deep, which makes the code less readable.  For
@racket[same-cards?], it is still not that obvious since it only accepts two arguments.  We
only need to nest one @racket[match]-expression inside another.

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

But imagine that we now want to define a function that take three cards and determine if
all of them are the same, then we have to nest three @racket[match]-expressions.  If you
still think it managable, imagine four, five or even more.  In these cases the advantage
manifests, even though it means we have to sacriface some efficiency.  For example, we
can define the function that determines the sameness among three cards as follows.

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

This time we construct a list from the three input cards and use the list pattern instead.
The transitivity of @racket[string=?] guarantees that once two different pairs of strings
are equal, all the three strings are equal.

@bold{Exercise.}  Rewrite the pattern-matching version of @racket[AND], @racket[OR],
@racket[ANDs] and @racket[ORs] do not use nested @racket[match]-expressions.

@section{Matching Argument List}

The last examples in the previous section shows that sometimes we want the arguments to a
function collected in some data structure so that we can handle them, for example, do pattern
matching on them, in one place.  Of course, we can do this by ourselves, as we did in those
examples.  But it would be better if it is supported out of the box.  Racket does support it.
In Racket, when a function is passed in its arguments, it implicitly receives the arguments as
a list.  This is how a function sees its arguments under the hood.  Racket provides ways that
allow us to open the hood and get the arguments as a list.  Then we can handle the arguments
collectively rather than individually.

