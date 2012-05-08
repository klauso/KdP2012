#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Bedeutung von BSL}

@margin-note{Dieser Teil des Skripts basiert teilweise auf [HTDP/2e] Kapitel 3}

@racketblock[
program ::= def-or-expr ...

def-or-expr ::= definition
              | expr
              | test-case
 	 	 	 	 
definition = (define (name variable variable ...) expr)
           | (define name expr)
           | (define-struct name (name ...))

expr = (name expr expr ...)
     | (cond [expr expr] ... [expr expr])
     | (cond [expr expr] ... [else expr])
     | name
     | number
     | string
     | image
       
val = (make-c val-1 ... val-N)
    | number
    | string
    | image
]

Via desugaring: and/or, else-branch in cond, if-expression
Not explained: tests

(name val-1 ... val-n) @step body[x-1 := val-1, ..., x-n := val-n] falls (define (name x-1 ... x-n) body) im Kontext

(cond [false expr-1] [expr-2 expr-3] ...) @step (cond [expr-2 expr-3] ...)

(cond [true expr-1] ...) @step expr-1

