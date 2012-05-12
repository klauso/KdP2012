#lang scribble/manual
@(require scribble/eval)
@(require scribble/core)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
@(require scribble/bnf)
@(require scribble/decode)
   
@title[#:version ""]{Bedeutung von BSL}

@margin-note{Dieser Teil des Skripts basiert teilweise auf [HTDP/2e] Kapitel 3}

@section{Kontextfreie Grammatiken}


@section{Syntax von BSL}

@(define open (litchar "("))
@(define close (litchar ")"))
@(define lb (litchar "["))
@(define rb (litchar "]"))

@(define (mv s)
       (make-element #f  (list (make-element 'italic s))))

@BNF[(list @nonterm{program} @kleenestar[@nonterm{def-or-expr}])
     (list @nonterm{def-or-expr} @BNF-alt[@nonterm{definition} @nonterm{expr}])
     (list @nonterm{definition} 
         @BNF-seq[open @litchar{define} open @nonterm{name} @kleeneplus[@nonterm{variable}] close @nonterm{expr} close]
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{expr} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])
     (list @nonterm{expr}
         @BNF-seq[open @nonterm{name} @kleeneplus[@nonterm{expr}] close]
         @BNF-seq[open @litchar{cond} @kleeneplus[@BNF-group[@BNF-seq[lb @nonterm{expr} @nonterm{expr} rb ] ]] close]
         @BNF-seq[open @litchar{cond} @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{expr} @nonterm{expr} rb]]] lb @litchar{else} @nonterm{expr} rb close  ]
         @BNF-seq[open @litchar{if} @nonterm{expr} @nonterm{expr} @nonterm{expr} close]
         @BNF-seq[open @litchar{and} @nonterm{expr} @kleeneplus[@nonterm{expr}] close]
         @BNF-seq[open @litchar{or} @nonterm{expr} @kleeneplus[@nonterm{expr}] close]
         @nonterm{name}
         @nonterm{number}
         @nonterm{boolean}
         @nonterm{string}
         @nonterm{image})]

@section{Auswertungspositionen}


@BNF[(list @nonterm{E} 
      "[]"
      @BNF-seq[open @nonterm{name} @kleenestar[@nonterm{val}] @nonterm{E} @kleenestar[@nonterm{expr}]  close]
      @BNF-seq[open @litchar{cond} lb @nonterm{E} @nonterm{expr} rb @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{expr} @nonterm{expr} rb]]] close]
      @BNF-seq[open @litchar{and} @nonterm{E} @kleeneplus[@nonterm{expr}] close]
      @BNF-seq[open @litchar{and} @litchar{true} @nonterm{E} close]
      @BNF-seq[open @litchar{or} @nonterm{E} @kleeneplus[@nonterm{expr}] close]
      @BNF-seq[open @litchar{or} @litchar{false} @nonterm{E} close])]

Falls @mv{expr-1} @step @mv{expr-2}, dann @mv{E[expr-1]} @step @mv{E[expr-1]}.

     
@section{Syntax der Laufzeit-Entitäten}

@BNF[ (list @nonterm{val}
         @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @nonterm{name})) @kleenestar[@nonterm{val}] @litchar{>}]
         @nonterm{number}
         @nonterm{boolean}
         @nonterm{string}
         @nonterm{image})
      (list @nonterm{ctx}
         @kleenestar[@nonterm{ctxelmt}])
      (list @nonterm{ctxelmt}
         @BNF-seq[open @litchar{define} open @nonterm{name} @kleeneplus[@nonterm{variable}] close @nonterm{expr} close]
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{val} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])]
         

Not covered: test cases, lambda, quotes, characters, library imports

Via desugaring: else-branch in cond, if-expression
Not explained: tests

@section{Bedeutung von Programmen}

@section{Bedeutung von Ausdrücken}
@subsection{Bedeutung von Funktionsaufrufen}

Falls @BNF-seq[open @litchar{define} open @mv{name} @mv{variable-1} "..." @mv{variable-n} close @nonterm{expr} close] im Kontext, @linebreak[]
dann @BNF-seq[open @mv{name} @mv{val-1} "..." @mv{val-n} close] @step @mv{exp}[@mv{variable-1} := @mv{val-1} ... @mv{variable-n} := @mv{val-n}]


Falls @mv{name} eine primitive Funktion @mv{f} ist und @italic{f(val-1,...,val-n)=val}, @linebreak[]
dann @BNF-seq[open @mv{name} @mv{val-1} "..." @mv{val-n} close] @step @mv{val}. 

@subsection{Bedeutung von Variablen}

Falls @BNF-seq[open @litchar{define} @mv{name} @mv{val} close] im Kontext, @linebreak[]
dann @mv{name} @step @mv{val}. 

@subsection{Bedeutung konditionaler Ausdrücke}

@BNF-seq[open @litchar{cond} lb @litchar{true} @mv{expr} rb "..." close] @step @mv{expr}
@BNF-seq[open @litchar{cond} lb @litchar{false} @mv{expr-1} rb lb @mv{expr-2}  @mv{expr-3} rb "..." close] @step @BNF-seq[open @litchar{cond} lb @mv{expr-2}  @mv{expr-3} rb "..." close]

@subsection{Bedeutung boolscher Ausdrücke}

@BNF-seq[open @litchar{and} @litchar{true}  @litchar{true} close] @step @litchar{true}

@BNF-seq[open @litchar{and} @litchar{true}  @litchar{false} close] @step @litchar{false}

@BNF-seq[open @litchar{and} @litchar{false} "..." close] @step @litchar{false}

@BNF-seq[open @litchar{and} @litchar{true} @mv{expr-1} @mv{expr-2} "..." close] @step @BNF-seq[open @litchar{and} @mv{expr-1} @mv{expr-2} "..." close]

@BNF-seq[open @litchar{or} @litchar{false}  @litchar{true} close] @step @litchar{true}

@BNF-seq[open @litchar{or} @litchar{false}  @litchar{false} close] @step @litchar{false}

@BNF-seq[open @litchar{or} @litchar{true} "..." close] @step @litchar{false}

@BNF-seq[open @litchar{or} @litchar{false} @mv{expr-1} @mv{expr-2} "..." close] @step @BNF-seq[open @litchar{or} @mv{expr-1} @mv{expr-2} "..." close]


@subsection{Bedeutung von Strukturkonstruktoren und Selektoren}

Falls @BNF-seq[open @litchar{define-struct} @mv{name} open @mv{name-1} "..." @mv{name-n} close close] im Kontext, @linebreak[]
dann @BNF-seq[open  @(make-element #f (list @litchar{make-} @mv{name})) @mv{val-1} "..." @mv{val-n} close] @step
@BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name})) @mv{val-1} "..." @mv{val-n} @litchar{>}].

Falls @BNF-seq[open @litchar{define-struct} @mv{name} open @mv{name-1} "..." @mv{name-n} close close] im Kontext, @linebreak[]
dann @BNF-seq[open @(make-element #f (list @mv{name} @litchar{-} @mv{name} "-" @mv{i})) 
                      @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name})) @mv{val-1} "..." @mv{val-n} @litchar{>}]] @step @mv{val-i}

                                                                                                                                         
@BNF-seq[open @(make-element #f (list @mv{name} @litchar{?})) @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name}))  "..." @litchar{>}] close] @step @litchar{true} 

Falls @mv{val} nicht @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name}))  "..." @litchar{>}],
dann @BNF-seq[open @(make-element #f (list @mv{name} @litchar{?})) @mv{val} close] @step @litchar{false} 


           

