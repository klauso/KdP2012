#lang scribble/manual

@(require scribble/eval)
@(require lang/htdp-advanced)

@title{Pattern Matching}

When manipulating data, we almost always needs to access their components (if any).
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
;; negates a bit, via equality test
)

@#reader scribble/comment-reader
(racketblock
;; AND : number number -> number
;; conjoins two bits, via equality test
(define (AND b1 b2)
  (cond [(and (= b1 0) (= b2 0)) 0]
        [(and (= b1 0) (= b2 1)) 0]
        [(and (= b1 1) (= b2 0)) 0]
        [(and (= b1 1) (= b2 1)) 1] ) )
)

@#reader scribble/comment-reader
(racketblock
;; OR : number number -> number
;; disjoins two bits, via equality test
)

As an exercise, you are encouraged to supply the definition for @racket[NOT] and @racket[OR].

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
)

Try to rewrite your @racket[OR] definition if you also use too many equality tests.

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

(1) It also takes another expression.  In the two examples both are the variable @racket[b].
But in general any expression is allowed.

(2) The left of a clause (enclosed in brackets) is now a pattern instead of a boolean
expression.  For the moment, a pattern can be any Racket literal, like a boolean, a number,
a character, a string, etc.  Later we will see a pattern can also be a data structure.

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

Now try to write the pattern-matching version of @racket[NOT] using the @racket[match] form.

A @racket[match]-expression is like any other expression.  That is, it can appear anywhere a
normal expression is expected.  In particular, it can appear as the expression associated to
a pattern inside another @racket[match]-expression.  In this case, we say we have a nested
@racket[match]-expression, as we can have nested @racket[cond]-expression.  @racket[AND] can
can be rewritten using a nested @racket[match]-expression.

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
)

Try to write the pattern-matching version of @racket[OR] using a nested
@racket[match]-expression.

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
on a bit stream or two bit streams bit on bit.  Thus they are also called bit-wise
operations.  Similar to the case of debitalization, functions simulating them can also
defined on top of those functions that simulate single-bit operations.

@#reader scribble/comment-reader
(racketblock
;; NOTs : (listof number) -> (listof number)
;; bit-wise negates a bit stream, via isomorphism test
)

@#reader scribble/comment-reader
(racketblock
;; ANDs : (listof number) (listof number) -> (listof number)
;; bit-wise conjoins two bit streams, via isomorphism test
(define (ANDs bs1 bs2)
  (cond [(or (empty? bs1) (empty? bs2)) empty]
        [else (cons (AND (first bs1) (first bs2))
                    (ANDs (rest bs1) (rest bs2)) ) ] ) )
)

@#reader scribble/comment-reader
(racketblock
;; ORs : (listof number) (listof number) -> (listof number)
;; bit-wise disjoins two bit streams, via isomorphism test
)

Again, @racket[NOTs] and @racket[ORs] are left to you as an exercise.

These definitions are all defined by recursion on the built-in list data structure.

@subsection{Matching User-defined Data Structures}

Every poker card has a rank and a suit.  The rank of a card is one of the
numbers from 2 to 10, or one of the symbols A (Ace), J (Jack), Q (Queen) and
K (King).  The suit of a card is one of the the symbols ♠ (Spade), ♥ (Heart),
♦ (Diamond), and ♣ (Club).

We can represent a poker card as a @racket[struct].

@racketblock[
(define-struct card (suit rank))
;; suit : 'Spade, 'Heart, 'Diamond, 'Club, 'Joker
;; rank : 'A, 2, 3, 4, 5, 6, 7, 8, 9, 10, 'J, 'Q, 'K
]

Let us first define a function that echoes the name of the suit on a card.

@racketblock[
(define (suit-name s)
  (cond [(symbol=? s '♠) 'Spade]
        [(symbol=? s '♥) 'Heart]
        [(symbol=? s '♦) 'Diamond]
        [(symbol=? s '♣) 'Club]      
        ) )
]

For rank, let us define a function that tells us the type of the rank, either
@racket[number?] or @racket[symbol?].

@racketblock[
(define (rank-type r)
  (case r
    [(2 3 4 5 6 7 8 9 10) r]
    [(A) 'Ace]
    [(J) 'Jack]
    [(Q) 'Queen]
    [(K) 'King] ) )
]
    





@racketblock[
(define (AND b1 b2)
  (match b1
    [0 0]
    [1 (match b2
         [0 0]
         [1 1] ) ] ) )
]

@racketblock[
(define (AND b1 b2)
  (match (cons b1 b2)
    [(cons 0 0) 0]
    [(cons 0 1) 0]
    [(cons 1 0) 0]
    [(cons 1 1) 1] ) )
]

@racketblock[
(define (OR b1 b2)
  (match (cons b1 b2)
    [(cons 0 0) 0]
    [(cons 0 1) 1]
    [(cons 1 0) 1]
    [(cons 1 1) 1] ) )
]


@racketblock[
;; listof number -> listof boolean
;; debitalize a bit stream, via pattern matching
(define (debits bs)
  (match bs
    [(list) bs]
    [(list b bs ...) (cons (debit b) (debits bs))] ) )
]

@racketblock[
(define (NOTs bs)
  (match bs
    [(list) bs]
    [(list b bs ...) (cons (NOT b) (NOTs bs))] ) )
]

@racketblock[
(define (ANDs bs1 bs2)
  (match bs1
    [(list) bs1]
    [(list b1 bs1 ...)
     (match bs2
       [(list) bs2]
       [(list b2 bs2 ...) (cons (AND b1 b2) (ANDs bs1 bs2))] ) ] ) )
]

A less verbose way to do it.

@racketblock[
(define (ANDs bs1 bs2)
  (match (cons bs1 bs2)
    [(cons (list) _) bs1]
    [(cons _ (list)) bs2]
    [(cons (list b1 bs1 ...) (list b2 bs2 ...))
     (cons (AND b1 b2) (ANDs bs1 bs2)) ] ) )
]



