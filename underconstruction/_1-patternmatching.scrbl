#lang scribble/manual

@title{Pattern Matching}

@section{Motivating Examples}

@subsection{Debitalization}

We want to @italic{debitalize} the world to see the @italic{truth}!  Since bit
is the smallest information unit in the bital world, we debitalize it first.

@racketblock[
(define (debit b)
  (cond [(= b 0) 'F]
        [(= b 1) 'T] ) )
]

Though we do not have to destruct the bital world, we'd better understand how
it works, to make sure that we will not get something other than @racket[0] or
@racket[1].  The most basic functional units are the INV, AND, and OR gates.

@racketblock[
(define (INV b)
  (cond [(= b 0) 1]
        [(= b 1) 0] ) )
]

@racketblock[
(define (AND b1 b2)
  (cond [(and (= b1 0) (= b2 0)) 0]
        [(and (= b1 0) (= b2 1)) 0]
        [(and (= b1 1) (= b2 0)) 0]
        [(and (= b1 1) (= b2 1)) 1] ) )
]

An OR gate can be similarly defined.

Debitalizing a single bit is just the first step.  Massive information flows in
the bital world like streams.  Those logic gates in the bital world all can handle
bit streams.

@racketblock[
(define (INVs bs)
  (cond [(empty? bs) bs]
        [else (cons (INV (first bs))
                    (INVs (rest bs)) ) ] ) )
]

@racketblock[
(define (ANDs bs1 bs2)
  (cond [(or (empty? bs1) (empty? bs2)) empty]
        [else (cons (AND (first bs1) (first bs2))
                    (ANDs (rest bs1) (rest bs2)) ) ] ) )
]

So does an OR gate.

Accordingly, we must also be able to @italic{debitalize} a bit stream.

@racketblock[
(define (debits bs)
  (cond [(empty? bs) bs]
        [else (cons (debit (first b))
                    (debit (rest bs)) ) ] ) )
]

@subsection{Poker}

@racketblock[
(define-struct card (shape color value))
; shape: 'heart, 'diamond, 'spade, 'club, 'wild
; color: 'red, 'black
; value: 'A, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'J, 'Q, 'K
]

@section{Pattern Matching}

@subsection{Matching Literals}

@racketblock[
(define (debit b)
  (match b
    [0 'F]
    [1 'T] ) )
]

@racketblock[
(define (INV b)
  (match b
    [0 1]
    [1 0] ) )
]

@subsection{Matching Data Structures}

@subsubsection{Matching built-in data structures}

@racketblock[
(define (debits bs)
  (match bs
    [(list) bs]
    [(list b bs ...) (cons (debit b) (debits bs))] ) )
]

@subsubsection{Matching user-defined data structures}
