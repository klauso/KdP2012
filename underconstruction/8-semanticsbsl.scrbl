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

@section{Wieso?}
Die meisten Programmierer dieser Welt programmieren, ohne dass sie jeweils eine formale Definition der Bedeutung ihrer
Programmiersprache gesehen haben. Insofern ist die Frage berechtigt, wieso wir uns dies "antun".

Hierzu ist zu sagen, dass viele Programmierer die Programmiersprache, die sie verwenden, nicht wirklich verstehen.
Dies führt zu einer Methodik, in der statt systematischem Programmentwurf einfach so lange am Programm "herumgedoktort" 
wird, bis es "läuft". Ein Programm durch Ausführen und Tests zu validieren ist zwar sinnvoll, aber dennoch kann dies
nicht den gedanklichen Prozess ersetzen, wie ein Programm ablaufen muss, damit zu jeder Eingabe die korrekte Ausgabe 
produziert wird. Dazu ist es unumgänglich, dass Sie genau verstehen, was der Code bedeutet, den Sie gerade programmiert haben.

Wir möchten, dass Sie prinzipiell in der Lage sind, ihre Programme auf einem Blatt Papier auszuführen und exakt vorherzusagen, 
was ihr Code bewirkt. Auch wenn Ihnen der Umgang mit den eher theoretischen Konzepten dieses Kapitels vielleicht am Anfang
schwerstellt, glauben wir, dass Ihnen dieses Kapitel helfen kann, ein besserer und effektiverer Programmierer zu werden.

Davon abgesehen werden Sie sehen, dass die theoretischen Konzepte, die sie in diesem Kapitel kennenlernen, eine Eleganz haben, die 
es allein aus diesem Grund wert macht, sie zu studieren.

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
         @nonterm{v})
     (list @nonterm{v}
         @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @nonterm{name})) @kleenestar[@nonterm{v}] @litchar{>}]
         @nonterm{number}
         @nonterm{boolean}
         @nonterm{string}
         @nonterm{image})]

Das Nichtterminal @nonterm{program} steht für die Syntax ganzer Programme; @nonterm{def-or-expr} für Definitionen oder Ausdrücke,
@nonterm{definition} für Funktions/Variablen/Strukturdefinitionen, @nonterm{e} für Ausdrücke und @nonterm{v} für Werte.

Die geschweiften Klammern um Teilsequenzen wie in @kleeneplus[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb ] ]] dienen dazu,
um den  @kleenestar[] oder @kleeneplus[] Operator auf eine ganze Sequenz von Terminalsymbolen und Nichtterminalen anzuwenden und nicht nur
auf ein einzelens Nichtterminal. In diesem Beispiel bedeutet es, dass 1 oder mehr Vorkommen von @BNF-seq[lb @nonterm{e} @nonterm{e} rb ]
erwartet werden.

Die Produktionen für einige Nichtterminale, deren genaue Form nicht interessant ist, wurden in der Grammatik ausgelassen: 
@nonterm{name} steht für die zugelassenen Bezeichner für Funktionen, Strukturen und Variablen. @nonterm{number}
steht für die zugelassenen Zahlen. @nonterm{boolean} steht für @racket[true] oder @racket[false]. @nonterm{string} steht
für alle Strings wie @racket["asdf"]. Das Nichtterminal @nonterm{image} steht für Bilder im Programmtext (Bildliterale) wie @ev[rocket].

Die Werte der Form @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @nonterm{name})) @kleenestar[@nonterm{v}] @litchar{>}] dienen
dazu, Instanzen von Strukturen zu repräsentieren. Sie dürfen in BSL nicht direkt im Original-Programmtext vorkommen, aber sie werden
während der Reduktion erzeugt und in das Programm eingefügt.

@section{Die BSL Kernsprache}
Wenn man die Bedeutung einer Sprache definiert, möchte man normalerweise, dass diese Definition so kurz wie möglich ist, denn nur dann
kann ein Benutzer sie leicht verstehen und Schlussfolgerungen ziehen.

Aus diesem Grund identifizieren wir eine Untersprache der BSL, die bereits ausreichend ist, um alle Programme zu schreiben,
die man auch in BSL schreiben kann. Der einzige Unterschied ist, dass man an einigen Stellen vielleicht etwas umständlicheres
schreiben muss. 

Wir haben bereits ein intellektuelles Werkzeug kennengelernt, um Kernsprachenelemente von eher unwichtigem Beiwerk zu unterscheiden,
nämlich den syntaktischen Zucker. Im Abschnitt @secref{kondsem} haben wir gesehen, dass es nicht notwendig ist, @racket[if] Ausdrücke 
und innerhalb von @racket[cond] Ausdrücken den @racket[else] Operator zu unterstützen, weil man diese Sprachfeatures leicht mit dem einfachen 
@racket[cond] Ausdruck simulieren kann. Die in @secref{kondsem} angegebenen Transformationen betrachten wir daher als die @italic{Definition}
dieser Sprachfeatures und betrachten daher im folgenden nur noch die Kernsprache, in die diese Transformationen abbilden.

@margin-note{Recherchieren Sie, was die @italic{de Morgan'schen Regeln} sind, falls ihnen die Transformation nicht klar ist.}
Die Syntax oben enthält auch spezielle Syntax für die logischen Funktionen @racket[and] und @racket[or], weil deren Argumente anders ausgewertet
werden als die Argumente normaler Funktionen. Allerdings ist es in unserer Kernsprache nicht nötig, beide Funktionen zu betrachten, da @racket[or] 
mit Hilfe von @racket[and] und @racket[not] ausgedrückt werden kann. Wir "entzuckern" @racket[(or e-1 ... e-n)] also zu @racket[(not (and (not e-1) ... (not e-n)))].

Man könnte versuchen, auch @racket[and] noch wegzutransformieren und durch einen @racket[cond] Ausdruck zu ersetzen: @racket[(and e-1 e-2)]
wird transformiert zu @racket[(cond [e-1 e-2])]. Zwar simuliert dies korrekt die Auswertungsreihenfolge, aber diese Transformation ist nicht adäquat für
das in DrRacket implementierte Verhalten, wie folgendes Beispiel illustriert:

@ex[(and true 42)]

@ex[(cond [true 42])]


Damit sieht die Grammatik unserer Kernsprache wie folgt aus. Die Grammatik für Werte @nonterm{v} bleibt unverändert.

@BNF[(list @nonterm{program} @kleenestar[@nonterm{def-or-expr}])
     (list @nonterm{def-or-expr} @BNF-alt[@nonterm{definition} @nonterm{e}])
     (list @nonterm{definition} 
         @BNF-seq[open @litchar{define} open @nonterm{name} @kleeneplus[@nonterm{name}] close @nonterm{e} close]
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{e} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])
     (list @nonterm{e}
         @BNF-seq[open @nonterm{name} @kleeneplus[@nonterm{e}] close]
         @BNF-seq[open @litchar{cond} @kleeneplus[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb ] ]] close]
         @BNF-seq[open @litchar{and} @nonterm{e} @kleeneplus[@nonterm{e}] close]
         @nonterm{name}
         @nonterm{v}
         )]

     
@section{Werte und Umgebungen}

Was bedeuten nun Programme in der Sprache, deren Syntax oben definiert wurde? Die Bedeutung eines Ausdrucks wollen wir modellieren
als Sequenz von Reduktionsschritten, die am Ende zu einem Wert führt (oder mit einem Fehler abbricht oder nicht terminiert).

Werte haben wir bereits oben durch die Grammatik definiert. Alle Konstanten wie @racket[true], @racket[42] oder @racket["xyz"] sind also Werte. Außerdem sind Instanzen von Strukturen Werte;
die Werte aller Felder der Struktur müssen ebenfalls Werte sein. Also ist beispielsweise @racket[<make-posn 3 4>] ein Wert.
Wir modellieren Strukturen so, dass Ausdrücke wie @racket[(make-posn 3 (+ 2 2))] zu diesem Wert ausgewertet werden --- hier ist
also der Ausdruck der @racket[make-posn] aufruft (mit runden Klammern) von dem Wert @racket[<make-posn 3 4>] (mit spitzen Klammern) 
zu unterscheiden.

Sofern in Ausdrücken Funktionen, Konstanten, oder Strukturen benutzt werden, kann die Auswertung eines Ausdrucks nicht im "luftleeren" Raum
stattfinden, sondern man muss die @italic{Umgebung} (@italic{Environment}, im folgenden als @italic{env} abgekürzt) kennen, 
in dem der Ausdruck ausgewertet wird, um Zugriff auf die dazugehörigen Definitionen
zu haben. Vom Prinzip her ist die Umgebung einfach der Teil des Programms bis zu dem Ausdruck, der gerade ausgewertet wird. Allerdings 
werden Variablendefinitionen ja auch ausgewertet (siehe @secref{semanticsofvardefs}). Dies bringen wir durch folgende Definition
zum Ausdruck. Beachten Sie, dass im Unterschied zur Grammatik von BSL Variablendefinitionen die Form 
@BNF-seq[open @litchar{define} @nonterm{name} @nonterm{v} close] und nicht @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{e} close] haben.

@BNF[(list @nonterm{env}
         @kleenestar[@nonterm{env-element}])
      (list @nonterm{env-element}
         @BNF-seq[open @litchar{define} open @nonterm{name} @kleeneplus[@nonterm{name}] close @nonterm{e} close]
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{v} close]
         @BNF-seq[open @litchar{define-struct} @nonterm{name} open @kleeneplus[@nonterm{name}] close close])]
         
Ein Umgebung besteht also aus einer Sequenz von Funktions-, Variablen- oder Strukturdefinitionen, wobei
der Ausdruck in Variablendefinitionen bereits zu einem Wert ausgewertet wurde.


@section{Auswertungspositionen und die Kongruenzregel}

Im Abschnitt @secref{semanticsofexpressions} haben wir das erste Mal über Auswertungspositionen und die Kongruenzregel gesprochen.
Die Auswertungspositionen markieren, welcher Teil eines Programms als nächstes ausgewertet werden soll. Die Kongruenzregel
sagt, dass man Unterausdrücke in Auswertungspositionen auswerten und das Ergebnis der Auswertung wieder in den ganzen Ausdruck einbauen darf.

Betrachten wir als Beispiel den Ausdruck @racket[(* (+ 1 2) (+ 3 4))]. Der Unterausdruck @racket[(+ 1 2)] befindet sich in Auswertungsposition
und kann zu @racket[3] ausgewertet werden. Gemäß der Kongruenzregel kann ich den Gesamtausdruck also zu @racket[(* 3 (+ 3 4))] reduzieren.

Wir werden Auswertungspositionen und die Kongruenzregel durch einen @italic{Auswertungskontext} formalisieren. Ein Auswertungskontext ist
eine Grammatik für Programme, die ein "Loch", @litchar{[]}, enthalten. Die Grammatik ist so strukturiert, dass jedes Element
der definierten Sprache genau ein "Loch" enthält.

@BNF[(list @nonterm{E} 
      @litchar{[]}
      @BNF-seq[open @nonterm{name} @kleenestar[@nonterm{v}] @nonterm{E} @kleenestar[@nonterm{e}]  close]
      @BNF-seq[open @litchar{cond} lb @nonterm{E} @nonterm{e} rb @kleenestar[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb]]] close]
      @BNF-seq[open @litchar{and} @nonterm{E} @kleeneplus[@nonterm{e}] close]
      @BNF-seq[open @litchar{and} @litchar{true} @nonterm{E} close]
)]

Hier einige Beispiele für Auswertungskontexte: 

@racketblock[(* (unsyntax (litchar "[]")) (+ 3 4))]

@racketblock[(posn-x (make-posn 14 (unsyntax (litchar "[]"))))]

@racketblock[(and true (and (unsyntax (litchar "[]")) x))]

Dies sind alles @italic{keine} Auswertungskontexte:

@racketblock[(* (+ 3 4) (unsyntax (litchar "[]")) )]

@racketblock[(posn-x (make-posn 14 17))]

@racketblock[(and true (and x (unsyntax (litchar "[]"))))]


Das "Loch" in diesen Ausdrücken steht genau für die Unterausdrücke in einem Programm, die in Auswertungsposition sind. Wir verwenden die Definition für Werte, @nonterm{v},
von oben, um zu steuern, dass die Auswertung der Argumente in Funktionsaufrufen von links nach rechts erfolgt. 

Bisher (in Abschnitt @secref{semanticsofexpressions} und @secref{semanticsoffundefs}) hatten wir die Auswertungspositionen so definiert,
dass man bei Funktionsaufrufen die Argumente in beliebiger Reihenfolge auswerten kann. Die Auswertungskontexte wie oben definiert legen 
diese Reihenfolge fest, nämlich von links nach rechts. Wir werden später mehr zu diesem Unterschied sagen.

Sei @mv{E} ein Auswertungskontext. Wir verwenden die Schreibweise @mv{E}[@mv{e}], um das "Loch" in dem Auswertungskontext durch einen Ausdruck @mv{e} zu ersetzen.

Beispiel: Wenn @mv{E} = @racket[(* (unsyntax (litchar "[]")) (+ 3 4))], dann ist @mv{E}[@racket[(+ 1 2)]] = @racket[(* (+ 1 2) (+ 3 4))].

Mit Hilfe dieser Schreibweise können wir nun die Kongruenzregel so definieren:

@elem[#:style inbox-style]{
@italic{(KONG): }Falls @mv{e-1} @step @mv{e-2}, dann @mv{E[e-1]} @step @mv{E[e-2]}.
}      

Wir schreiben die Auswertungsregeln generell so auf, dass wir jeder Regel einen Namen geben. Diese Regel heißt @italic{(KONG)}.

Beispiel: Betrachten wir den Ausdruck @mv{e} = @racket[(* (+ 1 2) (+ 3 4))]. Diesen können wir zerlegen in einen Auswertungskontext 
@mv{E} = @racket[(* (unsyntax (litchar "[]")) (+ 3 4))] und einen Ausdruck @mv{e-1} = @racket[(+ 1 2)], so dass @mv{e} = @mv{E}[@mv{e-1}].
Da wir @mv{e-1} reduzieren können, @racket[(+ 1 2)] @step @racket[3], können wir auch dank @italic{(KONG)} @mv{e} reduzieren zu @mv{E}[@racket[3]] = @racket[(* 3 (+ 3 4))].

 
@section{Nichtterminale und Metavariablen - Keine Panik!}
In der Kongruenzregel von oben stehen Namen wie @mv{e-1} und @mv{e-2} für beliebige Ausdrücke und @mv{E} für einen beliebigen Auswertungskontext.

Im Allgemeinen verwenden wir die Konvention, dass der Name @mv{x} und Varianten wie @mv{x-1} und @mv{x-2} für beliebige Worte des Nichtterminals @nonterm{x} steht 
(zum Beispiel für @nonterm{x} = @nonterm{e} oder @nonterm{x} = @nonterm{v}). Derart verwendete Bezeichner wie @mv{v-1} oder @mv{e-2} nennt man auch @italic{Metavariablen},
weil sie nicht Variablen von BSL sind, sondern Variablen sind, die für Teile von BSL Programmen stehen.

Wenn wir Nonterminale als Mengen interpretieren (nämlich die Menge der Worte für die es Ableitungsbäume gibt), so könnten wir 
die Regel von oben auch so aufschreiben:

Für alle @mv{e-1}∈@nonterm{e} und alle @mv{e-2}∈@nonterm{e} und alle @mv{E}∈@nonterm{E} : Falls @mv{e-1} @step @mv{e-2}, dann @mv{E[e-1]} @step @mv{E[e-2]}.

Da dies die Regeln aber viel länger macht, verwenden wir die hier beschriebene Konvention.

@section{Bedeutung von Programmen}

Gemäß unserer Grammatik besteht ein Programm aus einer Sequenz von Definitionen und Ausdrücken.
Die Auswertungsregel für Programme nennen wir @italic{(PROG)}:
                                                      
@elem[#:style inbox-style]{
@italic{(PROG): }Ein Programm wird von links nach rechts ausgeführt und startet mit der leeren Umgebung. Ist das nächste Programmelement eine Funktions- oder Strukturdefinition, so wird
diese Definition in die Umgebung aufgenommen und die Ausführung mit dem nächsten Programmelement in der erweiterten Umgebung fortgesetzt. Ist das nächste Programmelement
ein Ausdruck, so wird dieser gemäß der unten stehenden Regeln in der aktuellen Umgebung zu einem Wert ausgewert. Ist das nächste Programmelement 
eine Variablendefinition @racket[(define x e)], so wird in der aktuellen Umgebung zunächst @racket[e] zu einem Wert @racket[v] ausgewertet und dann
@racket[(define x v)] zur aktuellen Umgebung hinzugefügt.}

Beispiel: Das Programm ist:

@racketblock[
(define (f x) (+ x 1))
(define c (f 5))
(+ c 3)]

Im ersten Schritt wird @racket[(define (f x) (+ x 1))] zur (leeren) Umgebung hinzugefügt. Die Konstantendefinition wird in der Umgebung 
@racket[(define (f x) (+ x 1))] ausgewertet zu @racket[(define c 6)] und dann dem Kontext hinzugefügt. Der Ausdruck @racket[(+ c 3)]
wird schliesslich in der Umgebung 
@racketblock[
(define (f x) (+ x 1))
(define c 6)]
ausgewertet.


@section{Bedeutung von Ausdrücken}
Jeder Ausdruck wird in einer Umgebung @mv{env} ausgewertet, wie sie im vorherigen Abschnitt definiert wurde. Um die Notation nicht zu überladen,  werden wir @mv{env}
nicht explizit zu jeder Reduktionsregel dazuschreiben sondern als implizit gegeben annehmen. Die Auswertung wird, wie aus 
Abschnitt @secref{semanticsofexpressions} bekannt, in Form von Reduktionsregeln der Form @mv{e-1} @step @mv{e-2} definiert. 
Ein Ausdruck @mv{e} wird ausgewertet, indem er solange reduziert wird, bis ein Wert herauskommt: @mv{e} @step @mv{e-1} @step ... @step @mv{v}.

Ein Fehler während der Auswertung äußert sich darin, dass die Reduktion "steckenbleibt", also wir bei einem Ausdruck ankommen,
der kein Wert ist und der nicht weiter reduziert werden kann.


@subsection{Bedeutung von Funktionsaufrufen}
Funktionen werden unterschiedlich ausgeführt je nachdem ob der Funktionsname eine primitive Funktion oder eine in der Umgebung definierte Funktion ist.
Im ersten Fall wird die primitive Funktion auf den Argumenten ausgewertet. Ist dies erfolgreich, so kann auf das Result reduziert werden.
Ist dies nicht erfolgreich, so kann der Ausdruck nicht reduziert werden.

Ist die Funktion hingegen in der Umgebung definiert, so wird der Aufruf zum Body der Funktionsdefintion reduziert,
wobei vorher alle Parameternamen durch die aktuellen Parameterwerte ersetzt werden. Dies ist die Bedeutung der
Notation  @mv{e}[@mv{name-1} := @mv{v-1} ... @mv{name-n} := @mv{v-n}].

Die Reduktionsregeln sind also:

@elem[#:style inbox-style]{
@italic{(FUN): }Falls @BNF-seq[open @litchar{define} open @mv{name} @mv{name-1} "..." @mv{name-n} close @mv{e} close] in der Umgebung, @linebreak[]
dann @BNF-seq[open @mv{name} @mv{v-1} "..." @mv{v-n} close] @step @mv{e}[@mv{name-1} := @mv{v-1} ... @mv{name-n} := @mv{v-n}]}

@italic{(PRIM): }@elem[#:style inbox-style]{Falls @mv{name} eine primitive Funktion @mv{f} ist und @italic{f(v-1,...,v-n)=v}, @linebreak[]
dann @BNF-seq[open @mv{name} @mv{v-1} "..." @mv{v-n} close] @step @mv{v}. 
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
                      @BNF-seq[@litchar{<}  @(make-element #f (list @litchar{make-} @mv{name})) @mv{v-1} "..." @mv{v-n} @litchar{>}]] @step @mv{v-i}
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
