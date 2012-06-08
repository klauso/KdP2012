#lang scribble/manual
@(require scribble/eval)
@(require scribble/core)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
@(require scribble/bnf)
@(require scribble/decode
          scribble/html-properties
          scribble/latex-properties)
   
@(define inbox-style
    (make-style "InBox"
                (list (make-css-addition "inbox.css")
                      (make-tex-addition "inbox.tex"))))

@title[#:version ""]{Bedeutung von ISL+}


Damit sieht die Grammatik unserer Kernsprache wie folgt aus. Die Grammatik für Werte @nonterm{v} bleibt unverändert.
@(define open (litchar "("))
@(define close (litchar ")"))
@(define lb (litchar "["))
@(define rb (litchar "]"))
@(define (mv s)
       (make-element #f  (list (make-element 'italic s))))


@BNF[(list @nonterm{definition} 
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{e} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])
     (list @nonterm{e}
         @BNF-seq[open @nonterm{e} @kleeneplus[@nonterm{e}] close]
         @BNF-seq[open @litchar{lambda} open @kleeneplus[@nonterm{name}] close @nonterm{e} close]
         @BNF-seq[open @litchar{local} lb @kleeneplus[@nonterm{definition}] rb @nonterm{e} close]
         @BNF-seq[open @litchar{cond} @kleeneplus[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb ] ]] close]
         @BNF-seq[open @litchar{and} @nonterm{e} @kleeneplus[@nonterm{e}] close]
         @nonterm{name}
         @nonterm{v}
         )
     (list @nonterm{v}
         @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @nonterm{name})) @kleenestar[@nonterm{v}] @litchar{>}]
         @BNF-seq[open @litchar{closure} open @litchar{lambda} open @kleeneplus[@nonterm{name}] close @nonterm{e} close @nonterm{env} close]
         @nonterm{number}
         @nonterm{boolean}
         @nonterm{string}
         @nonterm{image})          ]

     
@section{Werte und Umgebungen}

@BNF[(list @nonterm{env}
         @kleenestar[@nonterm{env-element}])
      (list @nonterm{env-element}
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{v} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])]
         

@section{Auswertungspositionen und die Kongruenzregel}



@BNF[(list @nonterm{E(@mv{env})} 
      @litchar{[]}
      @BNF-seq[open @litchar{local} lb @nonterm{env} rb @nonterm{E(@mv{env})} close]
      @BNF-seq[open @litchar{local} lb @nonterm{env} open @litchar{define} @nonterm{name} @nonterm{E} close @kleenestar[@nonterm{definition}] rb @nonterm{e} close]
      @BNF-seq[open @kleenestar[@nonterm{v}] @nonterm{E} @kleenestar[@nonterm{e}]  close]
      @BNF-seq[open @litchar{cond} lb @nonterm{E} @nonterm{e} rb @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]] close]
      @BNF-seq[open @litchar{and} @nonterm{E} @kleeneplus[@nonterm{e}] close]
      @BNF-seq[open @litchar{and} @litchar{true} @nonterm{E} close]
)]

Steht für:

In der Umgebung @mv{env} gilt:
@BNF-seq[open @litchar{local} lb @mv{env'} rb @mv{e} close] @step 
@BNF-seq[open @litchar{local} lb @mv{env'} rb @mv{e'} close], falls
 in der Umgebung @mv{env'} ++ @mv{env} gilt: @mv{e} @step @mv{e'}.  

In der Umgebung @mv{env} gilt:
@BNF-seq[open @litchar{local} lb @mv{env'} open @litchar{define} @nonterm{name} @mv{e} close "..." rb "..." close] @step 
@BNF-seq[open @litchar{local} lb @mv{env'} open @litchar{define} @nonterm{name} @mv{e'} close "..." rb "..." close],
falls in der Umgebung @mv{env'} ++ @mv{env} gilt: 
@mv{e} @step @mv{e'}.  

In der Umgebung @mv{env} gilt:
@BNF-seq[open @mv{v} "..." @mv{e} "..." close] @step
@BNF-seq[open @mv{v} "..." @mv{e'} "..." close],
falls in der Umgebung @mv{env}  gilt:
@mv{e} @step @mv{e'}.  

usw.

@section{Reduktion von Ausdrücken}


@elem[#:style inbox-style]{ 
@italic{(FUN): }In der Umgebung @mv{env} gilt:
@BNF-seq[open open @litchar{closure} @litchar{<} @litchar{lambda} open @mv{x-1} "..." @mv{x-n} close @mv{e} close @mv{v-1} "..." @mv{v-n} close @mv{env} @litchar{>}] @step 
@BNF-seq[
open @litchar{local}         
         open @litchar{local} lb open @litchar{define} @mv{x-1} @mv{v-1} close "..." open @litchar{define} @mv{x-n} @mv{v-n} close rb @mv{e}]},
fallse @BNF-seq[open @litchar{define} open @mv{name} @mv{name-1} "..." @mv{name-n} close @mv{e} close] in @mv{env}.

@racketblock[
((closure (lambda (x-1 ... x-n) e) env) v-1 ... v-n)             
]

@step

@racketblock[
(local env
  (local [(define x-1 v-1)
          ...
          (define x-n v-n)]
    e))
]



@italic{(PRIM): }@elem[#:style inbox-style]{
@BNF-seq[open @mv{name} @mv{v-1} "..." @mv{v-n} close] @step @mv{v},
falls @mv{name} eine primitive Funktion @mv{f} ist und @italic{f(v-1,...,v-n)=v}. 
}

@subsection{Bedeutung von Variablen}

Variablen werden ausgewertet, indem sie in der Umgebung nachgeschlagen werden:

@elem[#:style inbox-style]{
@italic{(VAR): }Falls @BNF-seq[open @litchar{define} @mv{name} @mv{v} close] in der Umgebung, 
dann @mv{name} @step @mv{v}. 
}

@subsection{Bedeutung konditionaler Ausdrücke}
Konditionale Ausdrücke werden ausgewertet, wie schon in @secref{kondsem} beschrieben. Gemäß
der Definition des Auswertungskontextes wird stets nur der erste Bedingungsausdruck ausgewertet.
Je nachdem ob dieser @racket[true] oder @racket[false] ergibt, wird auf den Ergebnisausdruck
oder den um die fehlgeschlagene Bedingung gekürzten @racket[cond] Ausdruck reduziert:

@elem[#:style inbox-style]{
@italic{(COND-True): }@BNF-seq[open @litchar{cond} lb @litchar{true} @mv{e} rb "..." close] @step @mv{e}}

@elem[#:style inbox-style]{
@italic{(COND-False): }@BNF-seq[open @litchar{cond} lb @litchar{false} @mv{e-1} rb lb @mv{e-2}  @mv{e-3} rb "..." close] @step @BNF-seq[open @litchar{cond} lb @mv{e-2}  @mv{e-3} rb "..." close]
}

@subsection{Bedeutung boolscher Ausdrücke}

Die Definition der Auswertung boolscher Ausdrücke wertet die Bedingungen nur soweit wie nötig aus.
Insbesondere wird die Auswertung abgebrochen, sobald einer der Ausdrücke @racket[false] ergibt.

Die ersten beiden Reduktionsregeln sind erforderlich, um zu überprüfen, dass alle Argumente
boolsche Werte sind; andernfalls hätten die beiden Regeln zu @BNF-seq[open @litchar{and} @litchar{true}  @mv{v} close] @step @mv{v}
zusammengefasst werden können. Insgesamt benötigen wir für boolsche Ausdrücke die folgenden vier Regeln:

@elem[#:style inbox-style]{
@italic{(AND-1): }@BNF-seq[open @litchar{and} @litchar{true}  @litchar{true} close] @step @litchar{true}}

@italic{(AND-2): }@elem[#:style inbox-style]{@BNF-seq[open @litchar{and} @litchar{true}  @litchar{false} close] @step @litchar{false}}

@italic{(AND-3): }@elem[#:style inbox-style]{@BNF-seq[open @litchar{and} @litchar{false} "..." close] @step @litchar{false}}

@italic{(AND-4): }@elem[#:style inbox-style]{@BNF-seq[open @litchar{and} @litchar{true} @mv{e-1} @mv{e-2} "..." close] @step @BNF-seq[open @litchar{and} @mv{e-1} @mv{e-2} "..." close]}



@subsection{Bedeutung von Strukturkonstruktoren und Selektoren}

Strukturdefinitionen definieren drei Arten von Funktionen: Konstruktoren wie @racket[make-posn], Selektoren wie @racket[posn-x] und 
Prädikate wie @racket[posn?]. Zu jeder dieser drei Arten benötigen wir eine Reduktionsregel.

Konstruktoren erzeugen Instanzen einer Struktur. Dies gelingt, wenn eine Struktur des gleichen Namens
in der Umgebung zu finden ist, und diese so viele Felder wie der Konstruktor Parameter hat. Dies 
bringt uns zu folgender Regel:

@elem[#:style inbox-style]{
@italic{(STRUCT-make): }Falls @BNF-seq[open @litchar{define-struct} @mv{name} open @mv{name-1} "..." @mv{name-n} close close] in der Umgebung, 
dann @BNF-seq[open  @(make-element #f (list @litchar{make-} @mv{name})) @mv{v-1} "..." @mv{v-n} close] @step
@BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name})) @mv{v-1} "..." @mv{v-n} @litchar{>}].
}

Selektoraufrufe werden reduziert, indem in der Umgebung die Strukturdefinition nachgeschlagen wird, um
den Namen des Feldes auf die Argumentposition des Konstruktoraufrufs abzubilden. Dann wird der entsprechende
Wert des Feldes zurückgegeben:

@elem[#:style inbox-style]{
@italic{(STRUCT-select): }Falls @BNF-seq[open @litchar{define-struct} @mv{name} open @mv{name-1} "..." @mv{name-n} close close] in der Umgebung,
dann @BNF-seq[open @(make-element #f (list @mv{name} @litchar{-} @mv{name} "-" @mv{i})) 
                      @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name})) @mv{v-1} "..." @mv{v-n} @litchar{>}] close] @step @mv{v-i}
}

Bei Prädikaten wird geschaut, ob es sich beim Argument des Prädikats um eine Strukturinstanz der in Frage stehenden Struktur
handelt oder nicht, und je nachdem @racket[true] bzw. @racket[false] zurückgegeben:

@elem[#:style inbox-style]{
@italic{(STRUCT-predtrue): }@BNF-seq[open @(make-element #f (list @mv{name} @litchar{?})) @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name}))  "..." @litchar{>}] close] @step @litchar{true} }

@elem[#:style inbox-style]{
@italic{(STRUCT-predfalse): }Falls @mv{v} nicht @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name}))  "..." @litchar{>}],
dann @BNF-seq[open @(make-element #f (list @mv{name} @litchar{?})) @mv{v} close] @step @litchar{false} 
}

@section{Reduktion am Beispiel}

Betrachten Sie folgendes Programm, dessen Bedeutung wir Schritt für Schritt mit Hilfe
der Auswertungsregeln ermitteln werden:

@racketblock[
(define-struct s (x y))             
(define (f x) (cond [(< x 1) (/ x 0)]
                    [true (+ x 1)]
                    [true x]))
(define c (make-s 5 (+ (* 2 3) 4)))
(f (s-x c))]

@itemlist[
         
@item{Gemäß @italic{(PROG)} starten wir mit der leeren Umgebung @mv{env} = leer.
Das erste Programmelement ist eine Strukturdefinition, daher ist gemäß @italic{(PROG)}
die Umgebung im nächsten Schritt @mv{env} = @racket[(define-struct s (x y))].}

@item{Das nächste Programmelement ist eine Funktionsdefinition, daher ist gemäß 
      @italic{(PROG)} die Umgebung im nächsten Schritt  
@mv{env} = @racketblock[
(define-struct s (x y))                                
(define (f x) (cond [(< x 1) (/ x 0)]
                    [true (+ x 1)]
                    [true x]))]
}

@item{Das nächste Programmelement ist eine Variablendefinition. Gemäß @italic{(PROG)}
müssen wir also zunächst @racket[(make-s 5 (+ (* 2 3) 4))] auswerten:
@itemlist[
@item{@mv{e} = @racket[(make-s 5 (+ (* 2 3) 4))] zerfällt in
@mv{E} = @racket[(make-s 5 (+ (unsyntax (litchar "[]")) 4))] und @mv{e-1} = @racket[(* 2 3)].
Gemäß @italic{(PRIM)} gilt @mv{e-1} @step @racket[6]; gemäß @italic{(KONG)} gilt daher 
@mv{e} @step @racket[(make-s 5 (+ 6 4))].}


@item{@mv{e} = @racket[(make-s 5 (+ 6 4))] zerfällt in
@mv{E} = @racket[(make-s 5 (unsyntax (litchar "[]")))] und @mv{e-1} = @racket[(+ 6 4)].
Gemäß @italic{(PRIM)} gilt @mv{e-1} @step @racket[10]; gemäß @italic{(KONG)} gilt daher 
@mv{e} @step @racket[(make-s 5 10)].}

@item{@racket[(make-s 5 10)] @step @racket[<make-s 5 10>] gemäß @italic{(STRUCT-make)}.}
]
Gemäß  @italic{(PROG)} ist unsere neue Umgebung daher nun @mv{env} = @racketblock[
(define-struct s (x y))             
(define (f x) (cond [(< x 1) (/ x 0)]
                    [true (+ x 1)]
                    [true x]))
(define c <make-s 5 10>)]
}

@item{Das letzte Programmelement ist ein Ausdruck, den wir gemäß @italic{(PROG)} in der 
aktuellen Umgebung auswerten:
@itemlist[
@item{@mv{e} = @racket[(f (s-x c))] zerfällt in 
@mv{E} = @racket[(f (s-x (unsyntax (litchar "[]"))))] und @mv{e-1} = @racket[c].
Gemäß @italic{(VAR)} gilt @racket[c] @step @racket[<make-s 5 10>]; gemäß @italic{(KONG)} gilt daher 
@mv{e} @step @racket[(f (s-x <make-s 5 10>))].}

@item{@mv{e} = @racket[(f (s-x <make-s 5 10>))] zerfällt in 
@mv{E} = @racket[(f (unsyntax (litchar "[]")))] und @mv{e-1} = @racket[(s-x <make-s 5 10>)].
Gemäß @italic{(STRUCT-select)} gilt @mv{e-1} @step @racket[5]; gemäß @italic{(KONG)} gilt daher 
@mv{e} @step @racket[(f 5)].}

@item{@racket[(f 5)] @step @racket[(cond [(< 5 1) (/ 5 0)] [true (+ 5 1)] [true 5])] gemäß @italic{(FUN)}.}

@item{@mv{e} = @racket[(cond [(< 5 1) (/ 5 0)] [true (+ 5 1)] [true 5])] zerfällt in 
@mv{E} = @racket[(cond [(unsyntax (litchar "[]")) (/ 5 0)] [true (+ 5 1)] [true 5])] und @mv{e-1} = @racket[(< 5 1)].
Gemäß @italic{(PRIM)} gilt @mv{e-1} @step @racket[false]; gemäß @italic{(KONG)} gilt daher 
@mv{e} @step @racket[(cond [false (/ 5 0)] [true (+ 5 1)] [true 5])].}

@item{@racket[(cond [false (/ 5 0)] [true (+ 5 1)] [true 5])] @step @racket[(cond [true (+ 5 1)] [true 5])] gemäß @italic{(COND-False)}.}

@item{@racket[(cond [true (+ 5 1)] [true 5])] @step @racket[(+ 5 1)] gemäß @italic{(COND-True)}.}

@item{@racket[(+ 5 1)] @step @racket[6] gemäß @italic{(PRIM)}.}
]}]



@section{Bedeutung von Daten und Datendefinitionen}
Datendefinitionen haben auf das Programmverhalten keinen Einfluss, da sie in Form eines Kommentars 
definiert werden. Dennoch können wir ihnen eine präzise Bedeutung geben, die hilft, ihre
Rolle zu verstehen.

Hierzu ist es wichtig, das @italic{Datenuniversum} eines Programms zu verstehen. Das Datenuniversum
umfasst alle Daten, die in einem gegebenen Programm potentiell vorkommen können. Welche Werte
das sind, wird durch unsere Grammatik für Werte, @nonterm{v}, oben beschrieben. Allerdings können
nicht alle Werte, die durch @nonterm{v} beschrieben werden, in einem Programm vorkommen, sondern
nur diese, für die die benutzen Strukturen auch wirklich im Programm definiert sind.

Beispiel: Ein Programm enthält die Strukturdefinitionen

@racketblock[
(define-struct circle (center radius))
(define-struct rectangle (corner-ul corner-dr))]

Das Datenuniversum für dieses Programm umfasst alle Werte der Basistypen, aber auch alle
Strukturinstanzen, die sich auf Basis dieser Strukturdefinitionen bilden lassen, also zum Beispiel
@racket[<make-circle 5 6>] aber auch:

@racketblock[<make-circle <make-circle <make-rectangle 5 <make-rectangle true "asdf">> 77> 88>]

Das Datenuniversum sind also alle Werte, die sich durch die Grammatik von @nonterm{v} bilden lassen, eingeschränkt
auf die Strukturen, die in dem Programm definiert sind.

Eine Strukturdefinition erweitert also das Datenuniversum um neue Werte, nämlich alle Werte, in denen
mindestens einmal diese Struktur verwendet wird.

Eine Datendefinition, auf der anderen Seite, erweitert nicht das Datenuniversum. Eine Datendefinition
definiert eine @italic{Teilmenge} des Datenuniversums.

Beispiel:

@#reader scribble/comment-reader
(racketblock
; a Posn is a structure: (make-posn Number Number)
)

@racket[<make-posn 3 4 >] ist ein Element der definierten Teilmenge, aber @racket[<make-posn true "x" >]
oder @racket[<make-posn <make-posn 3 4> 5>] sind es nicht.

Eine Datendefinition beschreibt im Allgemeinen eine kohärente Teilmenge des Datenuniversums. Funktionen
können durch ihre Signatur deutlich machen, welche Werte des Datenuniversums sie als Argumente akzeptieren
und welche Ergebnisse sie produzieren.

@section{Refactoring und Schliessen durch Gleichungen}

Dieser Abschnitt ist noch nicht fertig ausgearbeitet; im Moment können Sie ihn ignorieren oder, falls sie
"mutig" sind, mal unverbindlich einen Blick riskieren :-)

@subsection{Refactoring von Ausdrücken}

Wir hatten in Abschnitt @secref{semanticsofexpressions} vorgestellt, wie man auf Basis der Reduktionsregeln
eine Gleichheitsrelation auf Ausdrücken definieren kann. 
Diese Gleichheiten können zum Refactoring von Programmen verwendet
werden - also Programmänderungen, die nicht die Bedeutung verändern aber die Struktur des Programms verbessern.
Außerdem können sie verwendet werden, um Eigenschaften seines Programmes herzuleiten, zum Beispiel
dass die Funktion @racket[overlaps-circle] aus dem vorherigen Kapitel kommutativ ist, also 
@racket[(overlaps-circle c1 c2)] @equiv @racket[(overlaps-circle c2 c1)].

Die Gleichheitsrelation aus Abschnitt @secref{semanticsofexpressions} war allerdings zu klein für viele
praktische Zwecke, denn sie erfordert beispielsweise, dass wir Funktionsaufrufe nur dann auflösen können,
wenn alle Argumente Werte sind.

BSL hat jedoch eine bemerkenswerte Eigenschaft, die es uns erlaubt, eine viel mächtigere Gleichheitsrelation
zu definieren: Es ist für das Ergebnis eines Programms nicht von Bedeutung, in welcher Reihenfolge Ausdrücke ausgewertet
werden. Insbesondere ist es nicht notwendig, vor einem Funktionsaufruf die Argumente auszuwerten; man kann auch
einfach die Argumentausdrücke verwenden.

Die Idee wird durch folgenden, allgemeineren Auswertungskontext ausgedrückt:

@BNF[(list @nonterm{E} 
      @litchar{[]}
      @BNF-seq[open @nonterm{name} @kleenestar[@nonterm{e}] @nonterm{E} @kleenestar[@nonterm{e}]  close]
      @BNF-seq[open @litchar{cond} @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]] lb @nonterm{E} @nonterm{e} rb @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]]  close]
      @BNF-seq[open @litchar{cond} @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]] lb @nonterm{e} @nonterm{E} rb @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]]  close]      
      @BNF-seq[open @litchar{and} @kleenestar[@nonterm{e}] @nonterm{E} @kleenestar[@nonterm{e}] ]
)]
Zusammen mit der folgenden Kongruenzregel für unsere Gleichheitsrelation, drückt dieser Auswertungskontext aus, 
dass überall "gleiches mit gleichem" ersetzt werden darf:
@elem[#:style inbox-style]{
@italic{(EKONG): }Falls @mv{e-1} @equiv @mv{e-2}, dann @mv{E[e-1]} @equiv @mv{E[e-2]}.
}      
 
Eine Gleichheitsrelation sollte natürlich eine Äquivalenzrelation --- also reflexiv, kommutativ und transitiv --- sein:
@elem[#:style inbox-style]{
@italic{(EREFL): }@mv{e} @equiv @mv{e}.
}      

@elem[#:style inbox-style]{
@italic{(EKOMM): }Falls @mv{e1} @equiv @mv{e2}, dann @mv{e2} @equiv @mv{e1}.
}      

@elem[#:style inbox-style]{
@italic{(ETRANS): }Falls @mv{e-1} @equiv @mv{e-2} und @mv{e-2} @equiv @mv{e-3}, dann @mv{e-1} @equiv @mv{e-3}.
}      

@elem[#:style inbox-style]{
@italic{(ERED): }Falls @mv{e-1} @step @mv{e-2} dann @mv{e-1} @equiv @mv{e-2}.
}


@elem[#:style inbox-style]{
@italic{(EFUN): }Falls @BNF-seq[open @litchar{define} open @mv{name} @mv{name-1} "..." @mv{name-n} close @mv{e} close] in der Umgebung, @linebreak[]
dann @BNF-seq[open @mv{name} @mv{e-1} "..." @mv{e-n} close] @equiv @mv{e}[@mv{name-1} := @mv{e-1} ... @mv{name-n} := @mv{e-n}]}


@elem[#:style inbox-style]{
@italic{(EAND-1): }@BNF-seq[open @litchar{and} @litchar{true}  @litchar{true} close] @equiv @litchar{true}}

@italic{(EAND-3): }@elem[#:style inbox-style]{@BNF-seq[open @litchar{and} "..." @litchar{false} "..." close] @equiv @litchar{false}}

Einen kleinen Hackenfuss gibt es allerdings doch noch. Man würde sich von einer Gleichheitsrelation für Programme wünschen, dass folgende Eigenschaft
gilt: Falls @mv{e-1} @equiv @mv{e-2} und @mv{e-1} @multistep @mv{v}, dann auch @mv{e-2} @multistep @mv{v}. Diese Eigenschaft gilt jedoch
nicht, weil es sein kann, dass @mv{e-1} terminiert aber @mv{e-2} nicht. 

Beispiel: Betrachten Sie folgendes Programm:

@racketblock[
(define (f x) (f x))
(define (g x) 42)
(g (f 1))
]

Da @racket[(f 1)] @step @racket[(f 1)], terminiert die Berechnung des Arguments für @racket[g] nicht, und gemäß der Kongruenzregel
gilt damit @racket[(g (f 1))] @step @racket[(g (f 1))], daher terminiert die Berechnung des Ausdrucks @racket[(g (f 1))] @step @racket[(g (f 1))] nicht.
Auf der anderen Seite gilt jedoch gemäß @italic{(EFUN)} @racket[(g (f 1))] @equiv 42. Man muss daher bei der Verwendung der Gleichheitsregeln
berücksichtigen, dass diese das Terminierungsverhalten des Programms verändern können.

Es gilt jedoch folgende etwas schwächere Eigenschaft, die wir ohne Beweis aufführen:

Falls @mv{e-1} @equiv @mv{e-2} und @mv{e-1} @multistep @mv{v-1} und @mv{e-2} @multistep @mv{v-2}, dann @mv{v1} = @mv{v2}.

Wenn also @mv{e-1} und @mv{e-2} gleich sind und beide terminieren, dann ist der Wert, der herauskommt, gleich.



@subsection{Refactoring von Datentypen}

Für Produkttypen, Summentypen und Funktionstypen gelten folgende Gleichungen.
Wir sagen, Typ a ist @italic{isomorph} zu Typ b, falls a @equiv b gemäß 
der unten stehenden Regeln.

Um die Analogie zur Algebra zu verdeutlichen, schreiben wir den
Funktionstyp a -> b als b@superscript{a}. Der Typ 2 steht für einen
beliebigen Typen mit 2 Elementen (wie zum Beispiel Boolean), der Typ 1 
steht für einen Typ mit einem Element, und so weiter.

a*1 @equiv a

a+0 @equiv a

1+1 @equiv 2 bei tagged union

1+1 @equiv 1 bei untagged union

a+b @equiv b+a

a*b @equiv b*a

(a*b)*c @equiv a*(b*c)

(a+b)+c @equiv a+(b+c)

a+a @equiv 2*a bei tagged union

a+a @equiv a   bei untagged union

a*a @equiv a^2

a^(b*c) @equiv (a^b)^c @equiv (a^c)^b (curry/uncurry)

Isomorph bedeutet, dass es eine bijektive Abbildung zwischen den Typen gibt, die
"strukturerhaltend" ist. Sei a @equiv b und seien a->b und b->a die Bijektionen zwischen
den Typen. "Strukturerhaltend" heißt informell, dass man jedes Programm, welches den
Typ a verwendet, systematisch so transformieren kann, dass die Bijektion auch auf
das transformierte Programm übertragbar ist. 
