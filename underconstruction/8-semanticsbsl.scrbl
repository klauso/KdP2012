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


In diesem Kapitel werden wir die Bedeutung (fast) aller Sprachkonstrukte von BSL zusammenfassen und formal definieren.

Dies geschieht in zwei Schritten: Wir werden zunächst die @italic{Syntax} der Sprache definieren. 
Die Syntax definiert, welche Texte BSL-Programme sind. Die Syntax wird in Form einer @italic{Grammatik}
definiert. Die Grammatik sagt nicht nur, welche Texte BSL-Programme sind, sondern zerlegt ein BSL-Programm in 
seine Teile, genau so wie eine Grammatik für natürliche Sprache einen Satz in Teile wie Subjekt, Prädikat und Objekt zerlegt.

Im zweiten Schritt definieren wir für die grammatikalisch korrekten BSL-Programme, was diese bedeuten. 
Die Bedeutung legen wir durch die Definition von Reduktionsschritten fest, mit denen BSL Programme zu Werten
ausgewertet werden können (sofern kein Fehler auftritt und sie terminieren). 

Wir haben bereits in den Abschnitten
@secref{semanticsofexpressions}, @secref{semanticsoffundefs}, @secref{kondsem} und @secref{semanticsofvardefs}
diese Reduktionsschritte für die meisten Sprachkonstrukte definiert. Wir werden hier diese Reduktionsschritte
anhand der formalen Syntaxdefinition nochmal präzisieren. Außerdem werden wir nun auch definieren, welche Bedeutung
Strukturen haben.

Es gibt verschiedene Möglichkeiten, die Bedeutung eine Programmiersprache zu definieren; die, die wir benutzen,
nennt man @italic{Reduktionssemantik} oder @italic{strukturelle operationelle Semantik} oder @italic{Plotkin Semantik} 
(nach Gordon Plotkin). Für die Formalisierung der Auswertungspositionen, von denen wir in den vorherigen Kapiteln
gesprochen haben,  verwenden wir sogenannte @italic{Auswertungskontexte}, 
die 1989 von Matthias Felleisen und Robert Hieb vorgeschlagen wurde. Das alles hört sich für einen Programmieranfänger
vielleicht angsteinflößend an, aber Sie werden sehen, dass es nicht so kompliziert ist wie es sich anhört :-)

@section{Kontextfreie Grammatiken}
Bisher haben wir nur informell beschrieben, wie BSL Programme aussehen. Mit Hilfe einer @italic{Grammatik} kann 
man diese informelle Beschreibung präzise und prägnant darstellen. Es gibt viele unterschiedliche Arten von Grammatiken.
Im Bereich der Programmiersprachen verwendet man meistens sogenannte @italic{kontextfreie} Grammatiken. Diese 
und andere Grammatikformalismen werden in der Vorlesung "Theoretische Informatik" im Detail behandelt; wir werden
Grammatiken hier nur soweit besprechen, wie es zum Verständnis der Definitionen erforderlich ist. 

Es gibt unterschiedliche Notationen für kontextfreie Grammatiken. Wir verwenden die sogenannte EBNF --- die Erweiterte Backus Naur Form.

Hier direkt ein Beispiel einer Grammatik für Zahlen:
@BNF[
  (list @nonterm{Zahl} 
          @nonterm{PositiveZahl}
          @(make-element #f (list @litchar{-} @nonterm{PositiveZahl})))
  (list @nonterm{PositiveZahl}
          @nonterm{GanzeZahl}
          @nonterm{KommaZahl})
  (list @nonterm{GanzeZahl}
          @BNF-seq[@nonterm{ZifferNichtNull} @kleenestar[@nonterm{Ziffer}]]
          @litchar{0})
  (list @nonterm{Kommazahl}
          @BNF-seq[@nonterm{GanzeZahl} @litchar{.} @kleeneplus[@nonterm{Ziffer}]])
  (list @nonterm{ZifferNichtNull}
        @BNF-alt[@litchar{1} @litchar{2} @litchar{3} @litchar{4} @litchar{5} @litchar{6} @litchar{7} @litchar{8} @litchar{9}])
  (list @nonterm{Ziffer}
        @BNF-alt[@litchar{0} @nonterm{ZifferNichtNull}])]

Beispiele für Texte, die der @nonterm{Zahl} Definition dieser Grammatik entsprechen, sind: @litchar{0}, @litchar{420}, @litchar{-87}, @litchar{3.1416}, @litchar{-2.09900}.

Beispiele für Texte, die nicht der @nonterm{Zahl} Definition dieser Grammatik entsprechen, sind: @litchar{007}, @litchar{-.65}, @litchar{13.}, @litchar{zwölf}, @litchar{111Nonsense222}.

Die mit spitzen Klammern markierten Bezeichner wie @nonterm{Zahl} heißen @italic{Nichtterminale}; 
die farblich markierten Symbole wie @litchar{3} oder @litchar{.} heißen @italic{Terminalsymbole}. Eine Klausel wie die ersten beiden Zeilen der obigen Grammatik heißt @italic{Produktion}.
Eine Produktion besteht aus einem Nichtterminal auf der linken Seite der Definition und auf der rechten Seite aus einer 
Menge von Alternativen, die durch das Symbol | voneinander getrennt werden. Zu jedem Nichtterminal gibt es genau eine Produktion.

Zu jedem Nichtterminal kann man eine Menge von @italic{Ableitungsbäumen} bilden. 
Ein Ableitungsbaum entsteht durch das Ersetzen der Nichtterminale in einer der Alternativen der dazugehörigen Produktion durch Ableitungsbäume für diese Nichtterminale.
Die Konstruktion der Ableitungsbäume ist also ein rekursiver Prozess.  Der Prozess stoppt dort, wo man eine Alternative wählt, die nur aus Terminalsymbolen bestehen.
Falls ein Nichtterminal durch ein Sternchen oder ein Pluszeichen markiert wird, so wie  @kleenestar[@nonterm{Ziffer}] oder @kleeneplus[@nonterm{Ziffer}] oben,
so bedeutet dies 0 oder mehr Wiederholungen (für @kleenestar[]) bzw. 1 oder mehr Wiederholungen (für @kleeneplus[]) des Nichtterminals.

Jeder Ableitungsbaum steht für einen Text (häufig @italic{Wort} oder @italic{Satz} genannt), 
nämlich die Sequenz der Terminalsymbole, die in dem Baum vorkommen, von links nach rechts im Baum abgelesen. Die durch eine Grammatik definierte Sprache ist 
die Menge aller Worte, für die man Ableitungsbäume bilden kann.

Hier einige Beispiele für Ableitungsbäume des Nichtterminals @nonterm{Zahl} und die Worte, die sie repräsentieren.
Wir stellen die Bäume der Einfachheit halber durch Einrückung des Textes dar. Da damit die Bäume um 90 Grad gegenüber der Standarddarstellung gedreht sind,
müssen die Terminalsymbole von oben nach unten (statt von links nach rechts) abgelesen werden.

Der Ableitungsbaum für @litchar{0} ist:
                               

@nonterm{Zahl} @linebreak[]
 @hspace[2] @nonterm{PositiveZahl} @linebreak[]
   @hspace[4] @nonterm{GanzeZahl} @linebreak[]
    @hspace[6] @litchar{0} @linebreak[]

Der Ableitungsbaum für @litchar{420} ist: 

@nonterm{Zahl} @linebreak[]
  @hspace[2] @nonterm{PositiveZahl} @linebreak[]
     @hspace[4] @nonterm{GanzeZahl} @linebreak[]
        @hspace[6] @nonterm{ZifferNichtNull} @linebreak[]
          @hspace[8] @litchar{4} @linebreak[]
        @hspace[6] @nonterm{Ziffer} @linebreak[]
          @hspace[8] @nonterm{ZifferNichtNull} @linebreak[]
            @hspace[10] @litchar{2} @linebreak[]
        @hspace[6] @nonterm{Ziffer} @linebreak[]
          @hspace[8]@litchar{0}


@section{Syntax von BSL}
Nach diesen Vorarbeiten können wir nun präzise die Syntax von BSL durch eine kontextfreie Grammatik definieren.
Diese Syntax ist vollständig bis auf folgende Sprachfeatures, die wir noch nicht behandelt haben: Definition von Funktionen durch lambda-Ausdrücke, 
Quoting, Zeichendaten, Bibliotheksimporte. Außerdem werden in der Grammatik Aspekte, die für die Bedeutung der Sprache irrelevant
sind, außer Acht gelassen, zum Beispiel Kommentare, Zeilenumbrüche und Leerzeichen. Aus diesem Grund bezeichnet man Grammatiken
wie die folgende für BSL häufig als die @italic{abstrakte Syntax} einer Programmiersprache, im Unterschied zur @italic{konkreten Syntax}, die
auch Aspekte wie Kommentare und Zeilenumbrüche umfasst. Analog dazu werden Ableitungsbäume, wie sie oben beschrieben wurden, im
Kontext abstrakter Syntax häufig als @italic{abstrakte Syntaxbäume} (@italic{abstract syntax trees (AST)} bezeichnet.

@(define open (litchar "("))
@(define close (litchar ")"))
@(define lb (litchar "["))
@(define rb (litchar "]"))

@(define (mv s)
       (make-element #f  (list (make-element 'italic s))))

@BNF[(list @nonterm{program} @kleenestar[@nonterm{def-or-expr}])
     (list @nonterm{def-or-expr} @BNF-alt[@nonterm{definition} @nonterm{e}])
     (list @nonterm{definition} 
         @BNF-seq[open @litchar{define} open @nonterm{name} @kleeneplus[@nonterm{name}] close @nonterm{e} close]
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{e} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])
     (list @nonterm{e}
         @BNF-seq[open @nonterm{name} @kleeneplus[@nonterm{e}] close]
         @BNF-seq[open @litchar{cond} @kleeneplus[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb ] ]] close]
         @BNF-seq[open @litchar{cond} @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]] lb @litchar{else} @nonterm{e} rb close  ]
         @BNF-seq[open @litchar{if} @nonterm{e} @nonterm{e} @nonterm{e} close]
         @BNF-seq[open @litchar{and} @nonterm{e} @kleeneplus[@nonterm{e}] close]
         @BNF-seq[open @litchar{or} @nonterm{e} @kleeneplus[@nonterm{e}] close]
         @nonterm{name}
         @nonterm{number}
         @nonterm{boolean}
         @nonterm{string}
         @nonterm{image})]

Das Nichtterminal @nonterm{program} steht für die Syntax ganzer Programme; @nonterm{def-or-expr} für Definitionen oder Ausdrücke,
@nonterm{definition} für Funktions/Variablen/Strukturdefinitionen und @nonterm{e} für Ausdrücke.

Die geschweiften Klammern um Teilsequenzen wie in @kleeneplus[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb ] ]] dienen dazu,
um den  @kleenestar[] oder @kleeneplus[] Operator auf eine ganze Sequenz von Terminalsymbolen und Nichtterminalen anzuwenden und nicht nur
auf ein einzelens Nichtterminal. In diesem Beispiel bedeutet es, dass 1 oder mehr Vorkommen von @BNF-seq[lb @nonterm{e} @nonterm{e} rb ]
erwartet werden.

Die Produktionen für einige Nichtterminale, deren genaue Form nicht interessant ist, wurden in der Grammatik ausgelassen: 
@nonterm{name} steht für die zugelassenen Bezeichner für Funktionen, Strukturen und Variablen. @nonterm{number}
steht für die zugelassenen Zahlen. @nonterm{boolean} steht für @racket[true] oder @racket[false]. @nonterm{string} steht
für alle Strings wie @racket["asdf"]. Das Nichtterminal @nonterm{image} steht für Bilder im Programmtext (Bildliterale) wie @ev[rocket].

@section{Auswertungspositionen}


@BNF[(list @nonterm{E} 
      "[]"
      @BNF-seq[open @nonterm{name} @kleenestar[@nonterm{v}] @nonterm{E} @kleenestar[@nonterm{e}]  close]
      @BNF-seq[open @litchar{cond} lb @nonterm{E} @nonterm{e} rb @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]] close]
      @BNF-seq[open @litchar{and} @nonterm{E} @kleeneplus[@nonterm{e}] close]
      @BNF-seq[open @litchar{and} @litchar{true} @nonterm{E} close]
      @BNF-seq[open @litchar{or} @nonterm{E} @kleeneplus[@nonterm{e}] close]
      @BNF-seq[open @litchar{or} @litchar{false} @nonterm{E} close])]

Falls @mv{e-1} @step @mv{e-2}, dann @mv{E[e-1]} @step @mv{E[e-2]}.

     
@section{Syntax der Laufzeit-Entitäten}

@BNF[ (list @nonterm{v}
         @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @nonterm{name})) @kleenestar[@nonterm{v}] @litchar{>}]
         @nonterm{number}
         @nonterm{boolean}
         @nonterm{string}
         @nonterm{image})
      (list @nonterm{ctx}
         @kleenestar[@nonterm{ctx-elmt}])
      (list @nonterm{ctx-elmt}
         @BNF-seq[open @litchar{define} open @nonterm{name} @kleeneplus[@nonterm{name}] close @nonterm{e} close]
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{v} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])]
         



Via desugaring: else-branch in cond, if-expression
Not explained: tests

@section{Bedeutung von Programmen}

@section{Bedeutung von Ausdrücken}
@subsection{Bedeutung von Funktionsaufrufen}

Falls @BNF-seq[open @litchar{define} open @mv{name} @mv{name-1} "..." @mv{name-n} close @nonterm{e} close] im Kontext, @linebreak[]
dann @BNF-seq[open @mv{name} @mv{v-1} "..." @mv{v-n} close] @step @mv{exp}[@mv{name-1} := @mv{v-1} ... @mv{name-n} := @mv{v-n}]


Falls @mv{name} eine primitive Funktion @mv{f} ist und @italic{f(val-1,...,val-n)=val}, @linebreak[]
dann @BNF-seq[open @mv{name} @mv{v-1} "..." @mv{v-n} close] @step @mv{v}. 

@subsection{Bedeutung von Variablen}

Falls @BNF-seq[open @litchar{define} @mv{name} @mv{v} close] im Kontext, @linebreak[]
dann @mv{name} @step @mv{v}. 

@subsection{Bedeutung konditionaler Ausdrücke}

@BNF-seq[open @litchar{cond} lb @litchar{true} @mv{e} rb "..." close] @step @mv{e}
@BNF-seq[open @litchar{cond} lb @litchar{false} @mv{e-1} rb lb @mv{e-2}  @mv{e-3} rb "..." close] @step @BNF-seq[open @litchar{cond} lb @mv{e-2}  @mv{e-3} rb "..." close]

@subsection{Bedeutung boolscher Ausdrücke}

@BNF-seq[open @litchar{and} @litchar{true}  @litchar{true} close] @step @litchar{true}

@BNF-seq[open @litchar{and} @litchar{true}  @litchar{false} close] @step @litchar{false}

@BNF-seq[open @litchar{and} @litchar{false} "..." close] @step @litchar{false}

@BNF-seq[open @litchar{and} @litchar{true} @mv{e-1} @mv{e-2} "..." close] @step @BNF-seq[open @litchar{and} @mv{e-1} @mv{e-2} "..." close]

@BNF-seq[open @litchar{or} @litchar{false}  @litchar{true} close] @step @litchar{true}

@BNF-seq[open @litchar{or} @litchar{false}  @litchar{false} close] @step @litchar{false}

@BNF-seq[open @litchar{or} @litchar{true} "..." close] @step @litchar{false}

@BNF-seq[open @litchar{or} @litchar{false} @mv{e-1} @mv{e-2} "..." close] @step @BNF-seq[open @litchar{or} @mv{e-1} @mv{e-2} "..." close]


@subsection{Bedeutung von Strukturkonstruktoren und Selektoren}

Falls @BNF-seq[open @litchar{define-struct} @mv{name} open @mv{name-1} "..." @mv{name-n} close close] im Kontext, @linebreak[]
dann @BNF-seq[open  @(make-element #f (list @litchar{make-} @mv{name})) @mv{v-1} "..." @mv{v-n} close] @step
@BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name})) @mv{v-1} "..." @mv{v-n} @litchar{>}].

Falls @BNF-seq[open @litchar{define-struct} @mv{name} open @mv{name-1} "..." @mv{name-n} close close] im Kontext, @linebreak[]
dann @BNF-seq[open @(make-element #f (list @mv{name} @litchar{-} @mv{name} "-" @mv{i})) 
                      @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name})) @mv{v-1} "..." @mv{v-n} @litchar{>}]] @step @mv{v-i}

                                                                                                                                         
@BNF-seq[open @(make-element #f (list @mv{name} @litchar{?})) @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name}))  "..." @litchar{>}] close] @step @litchar{true} 

Falls @mv{v} nicht @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name}))  "..." @litchar{>}],
dann @BNF-seq[open @(make-element #f (list @mv{name} @litchar{?})) @mv{v} close] @step @litchar{false} 


           

