#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-intermediate))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
@(require scribble/bnf)
   
@title[#:version ""]{DRY: Abstraktion überall!}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 6}

Wir haben bereits in Abschnitt @secref{dry} das "Don't Repeat Yourself" Prinzip
kennengelernt: Gute Programme sollten keine Wiederholungen enthalten
und nicht redundant sein. Wir haben gelernt, dass wir diese Redundanz
durch verschiedene @italic{Abstraktionsmechanismen} beseitigen können.

In diesem Abschnitt werden wir zunächst diese bereits bekannten Abstraktionsmechanismen
nochmal Revue passieren lassen. Allerdings sind diese Mechanismen nicht
für alle Arten von Redundanz geeignet. Wir werden in diesem Kapitel daher weitere
mächtige Abstraktionsmechanismen kennenlernen. Hierzu brauchen wir neue Sprachkonzepte,
die Sie bitte durch das Umschalten auf den "Zwischenstufe mit Lambda" Sprachlevel aktivieren.

Unsere Programmiersprache wird hierdurch erstmal komplexer. Am Ende des Kapitels werden wir sehen,
dass es allerdings eine so mächtige Art der Abstraktion gibt, dass wir damit viele
andere Abstraktionsarten damit subsumieren können. Dies wird die Sprache konzeptuell wieder
vereinfachen.

@section{Abstraktion von Konstanten}
Die wohl einfachste Art der Abstraktion ist anwendbar, wenn es in einem Programm an
mehreren Stellen Ausdrücke gibt, die konstant sind, also immer zum gleichen Wert auswerten.
Der Abstraktionsmechanismus, den wir in diesem Fall verwenden können, ist die Definition
von Variablen. Diese Möglichkeit haben wir bereits ausführlich im Abschnitt @secref{dry} besprochen.

@section{Funktionale Abstraktion}
Häufig gibt es in einem Programm die Situation, dass an vielen Stellen Ausdrücke
vorkommen, die nicht gleich sind, aber sich nur in einigen Stellen in den verwendeten Werten
unterscheide.

Beispiel:

@#reader scribble/comment-reader
(racketblock
; (list-of String) -> Boolean
; does l contain "dog"
(define (contains-dog? l)
  (cond
    [(empty? l) false]
    [else
     (or
      (string=? (first l) "dog")
      (contains-dog?
       (rest l)))]))



; (list-of String) -> Boolean
; does l contain "cat"
(define (contains-cat? l)
  (cond
    [(empty? l) false]
    [else
     (or
      (string=? (first l) "cat")
      (contains-cat?
       (rest l)))]))
)     

Die Ausdrücke in den beiden Funktionsbodies sind bis auf den String @racket["dog"]
beziehungsweise @racket["cat"] gleich.

Gute Programmierer sind zu faul, viele ähnliche Ausdrücke und Funktionen zu schreiben.
Für die Elimination dieser Art von Redundanz ist @italic{funktionale Abstraktion} geeignet:

@#reader scribble/comment-reader
(racketblock
; String (list-of String) -> Boolean
; to determine whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))
)     

Falls gewünscht, können @racket[contains-dog?] und @racket[contains-cat?] auf Basis
dieser Funktion wieder hergestellt werden:

@#reader scribble/comment-reader
(racketblock
; (list-of String) -> Boolean
; does l contain "dog"
(define (contains-dog? l)
  (contains? "dog" l))
  
; (list-of String) -> Boolean
; does l contain "cat"
(define (contains-cat? l)
  (contains? "cat" l))
)     
Alternativ dazu können auch die Aufrufer von @racket[contains-dog?] und
@racket[contains-cat?] so modifiziert werden, dass stattdessen @racket[contains?] verwendet 
wird.

Wenn Sie einmal @racket[contains?] definiert haben, wird es nie wieder nötig sein,
eine Funktion ähnlich wie die erste Variante von @racket[contains-dog?] zu definieren.

@section{Funktionen als Funktionsparameter}

Betrachten Sie folgenden Code:

@#reader scribble/comment-reader
(racketblock
; (list-of Number) -> Number
; adds all numbers in l
(define (add-numbers l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
         (add-numbers (rest l)))]))


; (list-of Number) -> Number
; multiplies all numbers in l
(define (mult-numbers l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
         (mult-numbers (rest l)))]))
)     

In diesem Beispiel haben wir eine ähnliche Situation wie bei @racket[contains-dog?] und
@racket[contains-cat?]: Beide Funktionen unterscheiden sich nur an zwei Stellen:
a) Im @racket[empty?] Fall wird einmal @racket[0] und einmal @racket[1] zurückgegeben.
b) Im anderen Fall wird einmal addiert und das andere mal multipliziert.

Der Fall a) ist völlig analog zu @racket[contains-dog?] und
@racket[contains-cat?] lösbar, indem wir diesen Wert als Parameter übergeben.

Eine andere Situation haben wir im Fall b). Hier unterscheiden sich die Funktionen
nicht in einem Wert, sondern im Namen einer aufgerufenen Funktion! Hierzu brauchen wir
eine neue Art der Abstraktion, nämlich die Möglichkeit, Funktionen als Parameter an andere
Funktionen zu übergeben.

Wenn wir mal einen Augenblick lang dieses Problem ignorieren und einfach auf gut Glück 
das offensichtliche tun:

@#reader scribble/comment-reader
(racketblock
(define (op-numbers op z l)
  (cond
    [(empty? l) z]
    [else
     (op (first l)
         (op-numbers (rest l) z))]))

(define (add-numbers l) (op-numbers + 0 l))
(define (mult-numbers l) (op-numbers * 1 l))
)

... so stellen wir fest, dass wir tatsächlich über Funktionen genau so abstrahieren
können wie über Werte! Wir können also Funktionen als Parameter übergeben und diese
Parameter an der ersten Position eines Funktionsaufrufs verwenden!

Das entscheidende an @racket[op-numbers] ist nicht, dass @racket[add-numbers]
und @racket[mult-numbers] nun Einzeiler sind. Der entscheidende Punkt ist, dass
wir nun eine sehr mächtige abstrakte Funktion geschaffen haben, die wir nun
universell auch für viele andere Dinge benutzen können.

Zum einen können wir feststellen, dass @racket[op-numbers] überhaupt keinen Code
mehr enthält, der spezifisch für Zahlen ist. Das deutet darauf hin, dass wir die Funktion
auch mit Listen anderer Werte verwenden können. Der Name @racket[op-numbers] ist daher
irreführend und wir benennen die Funktion um:

@block[
(define (op-elements op z l)
  (cond
    [(empty? l) z]
    [else
     (op (first l)
         (op-elements op z (rest l)))]))
]

Zum anderen können wir als Parameter für @racket[op] nicht nur primitive, sondern
auch beliebige selber definierte Funktionen übergeben.

Schauen wir uns an einigen Beispielen an, was man mit @racket[op-elements] alles machen
kann:

Wir können Zahlen aufsummieren:
@ex[(op-elements + 0 (list 5 8 12))]

Wir können Strings zusammenhängen:
@ex[(op-elements string-append "" (list "ab" "cd" "ef"))]

Wir können Bilder komponieren:

@ex[(op-elements beside empty-image (list (circle 10 "solid" "red") 
                                          (rectangle 10 10 "solid" "blue") 
                                          (circle 10 "solid" "green")))]


Wir können eine Liste kopieren, was alleine nicht sehr nützlich ist:

@ex[(op-elements cons empty (list 5 8 12 2 9))]

...aber zeigt, dass wir mit leichten Variationen davon andere interessante
Dinge tun können. Beispielsweise können wir zwei Listen aneinanderhängen:

@block[
(define (append-list l1 l2)
  (op-elements cons l2 l1))]

@ex[(append-list (list 1 2) (list 3 4))]

Wir können aus einer Liste von Listen eine Liste machen:

@ex[(op-elements append-list empty (list (list 1 2) (list 3 4) (list 5 6)))]

Und schliesslich, als etwas fortgeschrittenes Beispiel, können wir mit @racket[op-elements] eine Liste 
von Zahlen sortieren. Zu diesem Zweck benötigen wir eine Hilfsfunktion, die 
eine Zahl in eine Liste sortierter Zahlen einfügt:

@#reader scribble/comment-reader
(block
; A (sorted-list-of Number) is a (list-of Number) which is sorted by "<"

; Number (sorted-list-of Number) -> (sorted-list-of Number)
; inserts x into a sorted list xs
(check-expect (insert 5 (list 1 4 9 12)) (list 1 4 5 9 12))
(define (insert x xs)
  (cond [(empty? xs) (list x)]
        [(cons? xs) (if (< x (first xs))
                        (cons x xs)
                        (cons (first xs) (insert x (rest xs))))]))
)
@margin-note{Diese Sortierfunktion wird in der Algorithmik @italic{insertion sort} genannt.}
Und schon können wir aus @racket[insert] und @racket[op-elements] eine Sortierfunktion bauen:

@ex[(op-elements insert empty (list 5 2 1 69 3 66 55))]

@margin-note{Die Funktion
@racket[op-elements] ist übrigens so nützlich, dass sie auch als primitive Funktion zur 
Verfügung gestellt wird, und zwar mit dem Namen @racket[foldr].}
Es ist nicht so wichtig, wenn sie dieses letzte Beispiel nicht verstehen. Es soll lediglich demonstrieren,
was man gewinnt, wenn man durch Abstraktion wiederverwendbare Funktionen definiert. 

Übrigens geht es beim Entwurf von Funktionen wie @racket[op-elements] nicht nur um die Wiederverwendung
von Code, sondern auch um die "Wiederverwendung" von Korrektheitsargumenten. Betrachten Sie beispielsweise
die Frage, ob ein Programm terminiert oder vielleicht eine Endlosschleife enthält. Dies ist einer der
häufigsten Fehler in Programmen und bei Schleifen, wie bei den Beispielen oben, immer ein potentielles Problem.
Wenn wir einmal zeigen, dass @racket[op-elements] terminiert, so wissen wir, dass alle Schleifen, die wir
mit Hilfe von @racket[op-elements] bilden, auch terminieren (sofern die Funktion, die wir als Argument 
übergeben, terminiert). Wenn wir nicht @racket[op-elements] benutzen würden, müssten wir diese Überlegung
jedes mal aufs neue anstellen.

@section{Abstraktion in Signaturen, Typen und Datendefinitionen}

@subsection{Abstraktion in Signaturen}
Redundanz kann auch in Signaturen auftreten, und zwar in dem Sinne, dass es viele Signaturen für den gleichen
Funktionsbody geben kann.


Betrachten wir zum Beispiel die Funktion @racket[second], die das zweite Element aus einer Liste von Strings berechnet:

@#reader scribble/comment-reader
(racketblock
; (list-of String) -> String
(define (second l) (first (rest l)))
)

Hier ist eine Funktion, die das zweite Element aus einer Liste von Zahlen berechnet:

@#reader scribble/comment-reader
(racketblock
; (list-of Number) -> Number
(define (second l) (first (rest l)))
)

Die Funktionsdefinitionen sind bis auf die Signatur identisch. Wir könnten, um den Code nicht zu duplizieren,
mehrere Signaturen zu der gleichen Funktion schreiben:

@#reader scribble/comment-reader
(racketblock
; (list-of String) -> String
; (list-of Number) -> Number
(define (second l) (first (rest l)))
)

Offensichtlich ist dies aber keine sehr gute Idee, weil wir die Liste der Signaturen erweitern müssen, immer
wenn die Funktion mit einem neuem Elementtyp verwendet werden soll. Es ist auch nicht möglich, alle Signaturen
hinzuschreiben, denn es gibt unendlich viele, z.B. folgende unendliche Sequenz von Signaturen:

@#reader scribble/comment-reader
(racketblock
; (list-of String) -> String
; (list-of (list-of String)) -> (list-of String)
; (list-of (list-of (list-of String))) -> (list-of (list-of String))
; (list-of (list-of (list-of (list-of String)))) -> (list-of (list-of (list-of String)))
; ...
)

Wir haben jedoch bereits informell einen Abstraktionsmechanismus kennengelernt, mit dem wir diese Art von
Redundanz eliminieren können: Typvariablen. Eine Signatur mit Typvariablen steht implizit für alle möglichen
Signaturen, die sich ergeben, wenn man die Typvariablen durch Typen ersetzt. Wir kennzeichnen Namen
als Typvariablen, indem wir den Namen der Typvariablen in eckige Klammern vor die Signatur setzen. In der
Signatur können wir dann die Typvariable verwenden. Hier ist das Beispiel von oben mit Typvariablen:

@#reader scribble/comment-reader
(racketblock
; [X] (list-of X) -> X
(define (second l) (first (rest l)))
)

Der Typ, durch den eine Typvariable ersetzt wird, darf zwar beliebig sein, aber er muss für alle Vorkommen der
Typvariablen der gleiche sein. So ist zum Beispiel diese Signatur nicht äquivalent zur vorherigen, sondern falsch:
@#reader scribble/comment-reader
(racketblock
; [X Y] (list-of X) -> Y
(define (second l) (first (rest l)))
)
Der Grund ist, dass die Signatur auch für konkrete Signaturen wie 

@#reader scribble/comment-reader
(racketblock
; (list-of Number) -> String
)

steht, aber dies ist keine gültige Signatur für @racket[second].

@subsection{Signaturen für Argumente, die Funktionen sind}
Wir haben in diesem Kapitel die Möglichkeit eingeführt, Funktionen als Parameter an andere Funtionen zu übergeben. 
Wir wissen aber noch nicht, wie wir Signaturen solcher Funktionen beschreiben.

Dieses Problem lösen wir dadurch, dass wir es zulassen, Signaturen als Typen zu verwenden. Betrachten wir beispielsweise
eine Funktion, die ein Bild mit einem Kreis kombiniert, aber die Art, wie es kombiniert werden soll, zu einem
Parameter der Funktion macht:

@block[
(define (add-circle f img)
  (f img (circle 10 "solid" "red")))]

Beispielsweise kann diese Funktion so verwendet werden:

@ex[(add-circle beside (rectangle 10 10 "solid" "blue"))]

oder so:

@ex[(add-circle above (rectangle 10 10 "solid" "blue"))]

Die Signatur von @racket[add-circle] können wir nun so beschreiben:

@#reader scribble/comment-reader
(racketblock
; (Image -> Image) Image -> Image
(define (add-circle f img)
  (f img (circle 10 "solid" "red")))
)

Diese Signatur sagt aus, dass der erste Parameter eine Funktion mit der Signatur Image -> Image sein muss.
Wir verwenden also nun Signaturen als Typen oder, anders gesagt, wir unterscheiden nicht
mehr zwischen Signaturen und Typen.

Funktionen können nicht nur Funktionen als Argumente bekommen, sondern auch Funktionen als Ergebnis zurückliefern.
Beispiel:


@#reader scribble/comment-reader
(block
; Color -> Image
(define (big-circle color) (circle 20 "solid" color))

; Color -> Image
(define (small-circle color) (circle 10 "solid" color))

; String -> (Color -> Image)
(define (get-circle-maker size)
  (cond [(string=? size "big") big-circle]
        [(string=? size "small") small-circle]))
)

Die Funktion @racket[get-circle-maker] können wir nun aufrufen und sie liefert eine Funktion zurück.
Das bedeutet, dass wir einen Aufruf von @racket[get-circle-maker] an der @italic{ersten} Position
eines Funktionsaufrufs haben können. Bisher stand an dieser Position immer der Name einer Funktion
oder (seit wir Funktionen als Parameter kennen) Namen von Funktionsparametern.

Beispiel:

@ex[((get-circle-maker "big") "cyan")]

Beachten Sie in dem Beispiel die Klammersetzung: Dieser Aufruf ist ein Funtionsaufruf einer Funktion 
mit einem Parameter. Die Funktion, die wir aufrufen, ist @racket[(get-circle-maker "big")] und ihr
Parameter ist @racket["cyan"]. Dies ist also etwas völlig anderes als der ähnlich aussehende, in diesem
Beispiel aber unsinnige Ausdruck @racket[(get-circle-maker "big" "cyan")].

@subsection{Funktionen höherer Ordnung}
Funktionstypen können beliebig verschachtelt werden. Nehmen wir beispielsweise an, wir haben
noch andere Funktionen mit der gleichen Signatur wie @racket[add-circle], zum Beispiel @racket[add-rectangle].
Eine Funktion, die wir mit einer dieser Funktionen parametrisieren können, ist zum Beispiel:

@#reader scribble/comment-reader
(block
; ((Image Image -> Image) Image -> Image) Image Image -> Image
(define (add-two-imgs-with f img1 img2)
  (above (f beside img1) (f beside img2)))
)
Diese Funktion können wir jetzt mit Funktionen wie @racket[add-circle] parametrisieren:
@ex[(add-two-imgs-with add-circle (circle 10 "solid" "green") (circle 10 "solid" "blue"))]

Funktionen, in deren Signatur nur ein einziger Pfeil vorkommt (also Funktionen, die keine
Funktionen als Parameter haben oder zurückgeben), heißen in diesem Kontext auch @italic{Funktionen erster Ordnung}
oder @italic{first-order functions}. Beispiele für Funktionen erster Ordnung sind @racket[circle]
oder @racket[cons]. Funktionen, die Funktionen als Parameter bekommen oder
als Ergebnis zurückgeben, nennt man auch @italic{Funktionen höherer Ordnung} oder @italic{higher-order functions}.
Beispiele für Funktionen höherer Ordnung sind @racket[add-circle] und @racket[add-two-imgs-with].

Manchmal unterscheidet man bei Funktionen höherer Ordnung noch, was die maximale Schachtelungstiefe
der Funktionspfeile in den Argumenten einer Funktion ist. Beispielsweise nennt man @racket[add-circle] eine @italic{Funktion
zweiter Ordnung} und @racket[add-two-imgs-with] eine @italic{Funktion dritter Ordnung}. 

In der alltäglichen Programmierung sind Funktionen zweiter Ordnung sehr häufig, aber Funktionen von 
drittem und höherem Grad werden recht selten gebraucht.

@subsection{Polymorphe Funktionen höherer Ordnung}
Auch polymorphe Funktionen können Funktionen höherer Ordnung sein. Wir haben bereits die Funktion 
@racket[op-elements] oben kennengelernt. Schauen wir uns an, mit welchen Typen
sie in den Beispielen verwendet wird.

Im Beispiel @racket[(op-elements + 0 (list 5 8 12))] benötigen wir für @racket[op-elements] offensichtlich
die Signatur
@#reader scribble/comment-reader
(racketblock
; (Number Number -> Number) Number (list-of Number) -> Number
)
während wir für @racket[(op-elements string-append "" (list "ab" "cd" "ef"))] die Signatur
@#reader scribble/comment-reader
(racketblock
; (String String -> String) String (list-of String) -> String
)
erwarten. Diese Beispiele suggerieren folgende Verallgemeinerung durch Typvariablen:

@#reader scribble/comment-reader
(racketblock
; [X] (X X -> X) (list-of X) X -> X
(define (op-elements op z l)
  (cond
    [(empty? l) z]
    [else
     (op (first l)
         (op-elements op z (rest l)))]))
)

Allerdings passt diese Signatur nicht zu allen Beispielen von oben.
Beispielsweise benötigen wir für das @racket[(op-elements cons empty (list 5 8 12 2 9))]
die Signatur

@#reader scribble/comment-reader
(racketblock
; (Number (list-of Number) -> (list-of Number)) (list-of Number) (list-of Number) -> (list-of Number)
)

Es gibt keine Ersetzung von X durch einen Typ in der Signatur oben, die diese Signatur erzeugt.
Wenn wir uns die Funktionsdefinition etwas genauer hinschauen, können
wir allerdings auch noch eine allgemeinere Signatur finden, nämlich diese:

@#reader scribble/comment-reader
(racketblock
; [X Y] (X Y -> Y) (list-of X) Y -> Y
(define (op-elements op z l)
  (cond
    [(empty? l) z]
    [else
     (op (first l)
         (op-elements op z (rest l)))]))
)

Die Signatur ist allgemeiner, weil sie für mehr konkrete Signaturen (ohne Typvariablen) steht 
und ist ausreichend für alle Beispiele, die wir betrachet haben.

@subsection{Abstraktion in Datendefinitionen}
Bei generischen Datentypen wie solchen für Listen oder Bäume haben wir bereits gesehen, dass man
in diesem Fall nicht redundante Datendefinitionen wie folgt schreiben möchte:

@#reader scribble/comment-reader
(racketblock
; a List-of-String is either
; - empty
; - (cons String List-of-String)

; a List-of-Number is either
; - empty
; - (cons Number List-of-Number)
)

Ähnlich wie bei Funktionssignaturen abstrahieren wir über die Unterschiede mit Hilfe von Typvariablen:

@#reader scribble/comment-reader
(racketblock
; a (list-of X) is either
; - empty
; - (cons X (list-of X))
)

Im Unterschied zu Funktionssignaturen verzichten wir bei Datendefinitionen darauf, die verwendeten Typvariablen
separat zu kennzeichnen (wie bei Funktionssignaturen mit den eckigen Klammern), weil bei Datendefinitionen
durch die Position der Typvariablen in der ersten Zeile einer Datendefinition klar ist, dass es sich um eine
Typvariable handelt.

Wir können Datendefinitionen auch nutzen, um viele weitere Eigenschaften der Werte, die durch die Datendefinition
beschrieben werden, festzulegen. Zum Beispiel können wir so die Menge der nicht-leeren Listen definieren:

@#reader scribble/comment-reader
(racketblock
; a (nonempty-list-of X) is: (cons X (list-of X))
)



@subsection{Grammatik der Typen und Signaturen}

Wir können die Typen und Signaturen, mit denen wir nun programmieren können, durch eine Grammatik beschreiben.
Diese Grammatik spiegelt gut die rekursive Struktur von Typen wieder. Nicht alle Typen, die mit dieser
Grammatik gebildet werden können, sind sinnvoll. Beispielsweise ist @nonterm{X} ein Typ, der nicht sinnvoll
ist. Eine Bedeutung haben nur die Typen, bei denen alle vorkommenden Typvariablen durch ein 
im Ableitungsbaum darüber liegende Typabstraktion der Form @BNF-seq{@litchar{[} @nonterm{X} @litchar{]} @nonterm{Typ}})
gebunden wurden.

@BNF[
  (list @nonterm{Typ} 
          @nonterm{Basistyp}
          @nonterm{Datentyp}
          @BNF-seq[@litchar{(}@nonterm{TypKonstruktor} @kleeneplus[@nonterm{Typ}] @litchar{)}]
          @BNF-seq[@litchar{(}@kleeneplus[@nonterm{Typ}] @litchar{->} @nonterm{Typ}@litchar{)}]
          @nonterm{X}
          @BNF-seq{@litchar{[} @kleeneplus[@nonterm{X}] @litchar{]} @nonterm{Typ}})
  (list @nonterm{Basistyp}
          @litchar{Number}
          @litchar{String}
          @litchar{Boolean}
          @litchar{Image})
  (list @nonterm{Datentyp} 
        @litchar{Posn}
        @litchar{WorldState}
        "...")
  (list @nonterm{TypKonstruktor} 
        @litchar{list-of}
        @litchar{tree-of}
        "...")
  (list @nonterm{X} 
        @litchar{X}
        @litchar{Y}
        "...")]

Wir haben bisher nicht gesagt, wie wir Funktionstypen interpretieren sollen. Im Moment können wir es als
die Menge aller Funktionen, die diesem Typ genügen, interpretieren. Später werden wir dies noch präzisieren.

@section{Lokale Abstraktion}
Häufig benötigt man Variablen, Funktionen oder Strukturen nur @italic{lokal}, innerhalb einer Funktion. Für diesen Fall
gibt es die Möglichkeit, Definitionen lokal zu definieren, und zwar mit @racket[local] Ausdrücken.

@subsection{Lokale Funktionen}
Betrachten wir als erstes lokale Funktionen. Hier ein Beispiel: 

@margin-note{Beachten Sie, dass @racket[foldr] der Name der eingebauten Funktion ist, die unserem @racket[op-elements] entspricht.}
@#reader scribble/comment-reader
(racketblock
; (list-of String) -> String
; appends all strings in l with blank space between elements
(check-expect (append-all-strings-with-space (list "a" "b" "c")) " a b c ")
(define (append-all-strings-with-space l)
  (local (; String String -> String
          ; juxtapoint two strings and prefix with space
          (define (string-append-with-space s t)
            (string-append " " s t)))
    (foldr string-append-with-space
           " "
           l))))
)
Die Funktion @racket[string-append-with-space] ist eine @italic{lokale} Funktion, die nur innerhalb der Funktion @racket[append-all-strings-with-space]
sichtbar ist. Sie kann außerhalb von @racket[append-all-strings-with-space] nicht aufgerufen werden.

Da @racket[local] Ausdrücke Ausdrücke sind, können sie überall stehen, wo ein Ausdruck erwartet wird. Häufig werden als
äußerster Ausdruck eines Funktionsbodies verwendet, aber sie können auch an jeder anderen Stelle stehen.

Diese Ausdrücke sind daher äquivalent:

@ex[(local [(define (f x) (+ x 1))] (+ (* (f 5) 6) 7))]

@ex[(+ (local [(define (f x) (+ x 1))] (* (f 5) 6)) 7)]

Ein Extremfall ist der, die lokale Definition so weit nach innen zu ziehen, dass sie direkt an der Stelle steht, wo sie verwendet wird.

@ex[(+ (* ( (local [(define (f x) (+ x 1))] f) 5) 6) 7)]
           
Wir werden später sehen, dass es für diesen Fall noch eine bessere Notation gibt.

@subsection{Lokale Variablen}
Nicht nur Funktionen können lokal sein, sondern auch Variablen und Strukturdefinitionen.

Betrachten wir beispielsweise eine Funktion zum Exponentieren von Zahlen mit der Potenz 8:

@margin-note{Alternativ hätten wir im Body die Kurzschreibweise @racket[(* x x x x x x x x)] verwenden können.}
@block[
(define (power8 x)
  (* x (* x (* x (* x (* x (* x (* x x))))))))]

Zum Beispiel:

@ex[(power8 2)]

Diese Funktion benötigt acht Multiplikationen zum Berechnen des Ergebnis. Eine effizientere Methode
zum Berechnen der Potenz nutzt die Eigenschaft der Exponentialfunktion, dass a@superscript{2*b} = a@superscript{b} * a@superscript{b}.

@racketblock[
(define (power8-fast x)
  (local 
    [(define r1 (* x x))
     (define r2 (* r1 r1))
     (define r3 (* r2 r2))]
         r3))]

Mit Hilfe einer Sequenz lokaler Variablendeklarationen können wir also die Zwischenergebnisse repräsentieren und mit nur 3 Multiplikationen
das Ergebnis berechnen. Diese Variablendeklarationen könnte man nicht durch (globale) Variablendeklarationen ersetzen, denn ihr Wert hängt
von dem Funktionsparameter ab.

Im Allgemeinen verwendet man lokale Variablen aus zwei Gründen: 1) Um Redundanz zu vermeiden, oder 2) um Zwischenergebnissen einen Namen zu geben.
Auf den ersten Punkt kommen wir gleich nochmal zu sprechen; hier erstmal ein Beispiel für den zweiten Fall:

Im Abschnitt @secref{ballinbewegung} haben wir eine Funktion programmiert, die einen Bewegungsvektor zu einer Position addiert:

@racketblock[
(define (posn+vel p q)
  (make-posn (+ (posn-x p) (vel-delta-x q))
             (+ (posn-y p) (vel-delta-y q))))]


Mit Hilfe lokaler Variablen können wir den Zwischenergebnissen für die neue x- und y-Koordinate einen Namen geben
und deren Berechnung von der Konstruktion der neuen Position trennen:

@racketblock[
(define (posn+vel p q)
  (local [(define new-x (+ (posn-x p) (vel-delta-x q)))
          (define new-y (+ (posn-y p) (vel-delta-y q)))]
    (make-posn new-x new-y)))]

@margin-note{Es gibt sogar Programmiersprachen und Programmierstile, in denen man @italic{jedem} Zwischenergebnis einen Namen geben muss. Wenn Sie dies interessiert,
             recherchieren Sie was @italic{three address code}, @italic{administrative normal form} oder @italic{continuation-passing style} ist.}
Das Einführen von Namen für Zwischenergebnisse kann helfen, Code leichter lesbar zu machen, weil der Name hilft, zu verstehen, was das Zwischenergebnis repräsentiert.
Es kann auch dazu dienen, eine sehr tiefe Verschachtelung von Ausdrücken flacher zu machen.

Die andere wichtige Motivation für lokale Variablen ist die Vermeidung von Redundanz. Tatsächlich können wir mit lokalen Variablen
sogar zwei unterschiedliche Arten von Redundanz vermeiden: Statische Redundanz und dynamische Redundanz.

Mit statischer Redundanz ist unser DRY Prinzip gemeint: Wir verwenden lokale Variablen, um uns nicht im Programtext wiederholen zu müssen.
Dies illustriert unser erstes Beispiel. Illustrieren wir dies, indem wir unsere @racket[power8-fast] Funktion wieder de-optimieren
indem wir alle Vorkommen der Variablen durch ihre Definition ersetzen:

@racketblock[
(define (power8-fast-slow x)
  (local 
    [(define r1 (* x x))
     (define r2 (* (* x x) (* x x)))
     (define r3 (* (* (* x x) (* x x)) (* (* x x) (* x x))))]
         r3))]

Diese Funktion ist, bis auf die Assoziativität der Multiplikationen, äquivalent zu @racket[power-8] von oben. Offensichtlich ist hier aber das DRY-Prinzip verletzt,
weil die Unterausdrücke @racket[(* x x)] und @racket[(* (* x x) (* x x))] mehrfach vorkommen. Diese Redundanz wird durch die Verwendung der lokalen
Variablen in @racket[power8-fast] vermieden.

Die zweite Facette der Redundanz, die wir durch lokale Variablen vermeiden können, ist die dynamische Redundanz. Damit ist gemeint, dass wir
das mehrfache Auswerten eines Ausdrucks vermeiden können. Dies liegt daran, dass der Wert einer lokale Variable nur einmal bei ihrer Definition
berechnet wird und im folgenden nur das bereits berechnete Ergebnis verwendet wird. 
Im Beispiel @racket[power8-fast] haben wir gesehen, dass wir dadurch die Anzahl
der Multiplikationen von 8 auf 3 senken konnten. Im Allgemeinen "lohnt" sich die Definition einer lokalen Variable aus Sicht der dynamischen Redundanz dann,
wenn sie mehr als einmal ausgewertet wird.

Zusammengefasst haben lokale Variablen also den gleichen Zweck wie nicht-lokale (globale) Variablen, nämlich der Benennung von Konstanten bzw. Zwischenergebnissen und der Vermeidung
von statischer und dynamischer Redundanz; lokale Variablen sind allerdings dadurch mächtiger, dass sie
die aktuellen Funktionsparameter und andere lokale Definitionen verwenden können.

@subsection{Intermezzo: Successive Squaring}
Als fortgeschrittenes Beispiel für die Verwendung lokaler Variablen können wir die Verallgemeinerung des Potenzierungsbeispiels auf beliebige Exponenten betrachten.
Dieser Abschnitt kann übersprungen werden.

Um den Effekt der Vermeidung dynamischer Redundanz noch deutlicher zu illustrieren, betrachten wir die Verallgemeinerung von @racket[power8] auf beliebige (natürlichzahlige) Exponenten.
Wenn wir den Exponenten wie in Abschnitt @secref{natrec} beschrieben als Instanz eines rekursiven Datentyps auffassen, so ergibt sich folgende Definition:

@#reader scribble/comment-reader
(block
; NaturalNumber Number -> Number       
(define (exponential n x)
  (if (zero? n) 
      1
      (* x (exponential (sub1 n) x))))
)      

@margin-note{Dieses "Ausfalten" ist ein Spezialfall einer allgemeineren Technik, die @italic{partial evaluation} heißt.}
Die Redundanz in diesem Programm ist nicht offensichtlich; sie wird erst dann offenbar, wenn man die "Ausfaltung" der Definition für einen festen Exponenten
betrachtet; also die Version, die sich ergibt, wenn man die rekursiven Aufrufe für ein festgelegtes @racket[n] expandiert. 
Beispielsweise ergibt das Ausfalten von @racket[exponential] für den Fall @racket[n] = 8 genau die @racket[power8] Funktion von oben.
Die Redundanz wird also erst dann offensichtlich, wenn man, unter Ausnutzung der Assoziativität der Multiplikation, 
@racket[(* x (* x (* x (* x (* x (* x (* x x)))))))] umformt zu 
@racket[(* (* (* x x) (* x x)) (* (* x x) (* x x)))].

Eine kleine Komplikation der Anwendung der Technik von oben auf den allgemeinen Fall ist, dass der Exponent im Allgemeinen keine Potenz von 2 ist.
Wir gehen mit diesem Problem so um, dass wir die Fälle unterscheiden, ob der Exponent eine gerade oder eine ungerade Zahl ist.
Insgesamt ergibt sich dieser Algorithmus, der in der Literatur auch als Exponentiation durch @italic{successive squaring} bekannt ist.

@block[
(define (exponential-fast x n)
  (if (zero? n) 
      1
      (local
        [(define y (exponential-fast x (quotient n 2)))
         (define z (* y y))]
         (if (odd? n) (* x z) z))))]

@margin-note{Überlegen Sie, wieso @racket[exponential-fast] nur etwa log@subscript{2}(@racket[n]) Multiplikationen benötigt.}
Im @racket[power8] Beispiel oben haben wir die Anzahl der benötigten Multiplikationen von 8 auf 3 reduziert. In dieser allgemeinen Version ist der Unterschied
noch viel drastischer: Die @racket[exponential] Funktion benötigt @racket[n] Multiplikationen während @racket[exponential-fast] nur etwa
log@subscript{2}(@racket[n]) Multiplikationen benötigt. Das ist ein riesiger Unterschied. Beispielsweise benötigt 
auf meinem Rechner die Auswertung von @racket[(exponential 2 100000)] fast eine Sekunde, während @racket[(exponential-fast 2 100000)] weniger
als eine Millisekunde benötigt. Probieren Sie es selbst aus. Sie können beispielsweise die @racket[time] Funktion zur Messung
der Performance verwenden. 

@subsection{Lokale Strukturen}
Es ist möglich, auch Strukturen mit @racket[define-struct] lokal zu definieren. Wir werden diese Möglichkeit bis auf weiteres nicht verwenden.

@subsection{Scope lokaler Definitionen}
Der @italic{Scope} einer Definition eines Namens ist der Bereich im Programm, in dem sich eine Verwendung des Namens auf diese Definition bezieht.
Unserer Programmiersprache verwendet @italic{lexikalisches Scoping}, auch bekannt als @italic{statisches Scoping}. Dies bedeutet, dass der Scope einer lokalen Definition die Unterausdrücke 
des @racket[local] Ausdrucks sind.

In diesem Beispiel sind alle Verwendungen von @racket[f] und @racket[x] im Scope ihrer Definitionen.

@racketblock[
(local [(define (f n) (if (zero? n) 
                          0 
                          (+ x (f (sub1 n)))))
        (define x 5)]
  (+ x (f 2)))]

In diesem Ausdruck hingegen ist die zweite Verwendung von @racket[x] nicht im Scope der Definition:

@racketblock[
(+ (local [(define x 5)] (+ x 3))
   x)]

Es kann vorkommen, dass es mehrere Definitionen eines Namens (Variablen, Funktion, Strukturkonstruktoren/-selektoren) gibt, die einen
überlappenden Scope haben. In diesem Fall bezieht sich eine Verwendung des Namens immer auf die syntaktisch näheste Definition.
Wenn man also im abstrakten Syntaxbaums eines Programms nach außen geht, so "gewinnt" die erste Definition, die man auf dem Weg von dem
Auftreten eines Namens zur Wurzel des Baums trifft. 

Beispiel: Kopieren Sie das folgende Programm in den Definitionsbereich von DrRacket und drücken auf "Syntaxprüfung".
Gehen Sie nun mit dem Mauszeiger über eine Definition oder Benutzung eines Funktions- oder Variablennamens.
Die Pfeile, die Sie nun angezeigt bekommen, illustrieren, auf welche Definition sich ein Name bezieht.

@racketblock[
(add1 (local 
        [(define (f y)
                (local [(define x 2)
                        (define (g y) (+ y x))]
                  (g y)))
              (define (add1 x) (sub1 x))]
        (f (add1 2))))]

@section{Funktionen als Werte: Closures}

Betrachten Sie folgende Funktion zur numerischen Berechnung der Ableitung einer Funktion:

@#reader scribble/comment-reader
(racketblock
; (Number -> Number) Number -> Number
(define (derivative f x)
  (/ (- (f (+ x 0.001)) (f x))
     0.001))
)

Diese Funktion gibt uns die (numerische) Ableitung einer Funktion an einem speziellen Punkt.

Aber es wäre eigentlich besser, wenn uns diese Funktion die Ableitung selber als Funktion als
Ergebnis liefert. Wenn wir zum Beispiel eine Funktion 

@#reader scribble/comment-reader
(racketblock
; (Number -> Number) -> Image
(define (plot-function f) ...)
)
hätten, so könnten wir die Ableitung einer Funktion damit nicht zeichnen. Eigentlich möchten wir,
dass @racket[derivative] die Signatur

@#reader scribble/comment-reader
(racketblock
; (Number -> Number) -> (Number -> Number)
(define (derivative f) ...)
)
hat. Aber wie können wir diese Version von @racket[derivative] implementieren? Offensichtlich kann die
Funktion, die wir zurückgeben, keine global definierte Funktion sein, denn diese Funktion hängt ja
von dem Parameter @racket[f] ab. Wir können jedoch diese Funktion @italic{lokal} definieren (und
auch gleich etwas besser strukturieren), und dann diese Funktion zurückgeben:

@#reader scribble/comment-reader
(racketblock
; (Number -> Number) -> (Number -> Number)
(define (derivative f)
  (local 
    [(define delta-x 0.001)
     (define (delta-f-x x) (- (f (+ x delta-x)) (f x)))
     (define (g x) (/ (delta-f-x x) delta-x))]
    g)))

Was für eine Art von Wert ist es, der von @racket[derivative] zurückgegeben wird?

Nehmen wir es an, es wäre sowas wie die Definition von @racket[g], also 
@racket[(define (g x) (/ (delta-f-x x) delta-x))]. Wenn dem so wäre, was passiert mit
der Variablen @racket[delta-x] und der Funktion @racket[delta-f-x]? Diese sind ja
in @racket[derivative] nur lokal gebunden. Was passiert beispielsweise bei
der Auswertung von diesem Ausdruck: 

@racketblock[
(local [(define f (derivative exp))
        (define delta-x 10000)]
  (f 5))]

Wird die @racket[delta-x] Variable während der Auswertung von @racket[(f 5)] etwa zu @racket[10000]
ausgewertet? Das wäre ein Verstoß gegen lexikalisches Scoping und würde zu unvorhersehbaren
Interaktionen zwischen unterschiedlichen Programmteilen führen. Offensichtlich sollte
@racket[delta-x] in der zurückgegebenen Funktion sich immer auf die lokale Definition beziehen.

Wenn wir eine Funktion als Wert behandeln, so ist dieser Wert mehr als nur die Definition
der Funktion. Es ist nämlich die Definition der Funktion @italic{und} die Menge der lokal
definierten Namen (Variablen, Funktionen, Strukturen, Funktionsparameter). Diese Kombination
aus Funktionsdefinition und lokaler @italic{Umgebung} nennt man @italic{Funktionsabschluss} oder @italic{Closure}. 
Die Auswertung einer Funktion (nicht eines Funktionsaufrufs) ergibt also einen Closure.
Wird dieser Closure irgendwann später wieder im Rahmen eines Funktionsaufrufs angewendet, so 
wird die gespeicherte lokale Umgebung wieder aktiviert.
Später werden wir das Konzept des Closure noch präzisieren; im Moment merken wir uns, dass
die Auswertung einer Funktion die Funktionsdefinition plus ihre lokale Umgebung ergibt, und diese
lokale Umgebung bei Anwendung des Closure verwendet wird, um Namen im Funktionsbody auszuwerten.

@section{Lambda, die ultimative Abstraktion}

Nehmen sie an, sie möchten mit Hilfe der @racket[map] Funktion alle Elemente in einer Liste von
Zahlen @racket[lon] verdoppeln. Dies könnten Sie wie folgt anstellen:

@racketblock[
(local [(define (double x) (* 2 x))]
  (map double lon))]

Dieser Ausdruck ist komplizierter als es ein müsste. Für eine so einfache Funktion wie @racket[double],
die nur lokal verwendet wird, ist es Verschwendung, einen Namen zu vergeben und eine komplette
Extra Zeile Code zu verwenden.

In ISL+ (dem Sprachlevel von HTDP welches wir zurzeit verwenden) gibt es aus diesem Grund die Möglichkeit,
@italic{anonyme} Funktionen, also Funktionen ohne Namen, zu definieren. Anonyme Funktionen werden
mit dem Schlüsselwort @racket[lambda] gekennzeichnet. Daher nennt man solche Funktionen auch @racket[lambda]-Ausdrücke.

Hier ist das Beispiel von oben, aber so umgeschrieben, dass statt @racket[double] eine anonyme Funktion definiert wird.

@racketblock[(map (lambda (x) (* 2 x)) lon)]

Ein @racket[lambda] Ausdruck hat im Allgemeinen die Form @racket[(lambda (x-1 ... x-n) exp)] für Namen @racket[x-1],...,@racket[x-n]
und einen Ausdruck @racket[exp]. Ihre Bedeutung entspricht in etwa einer lokalen Funktionsdefinition der Form
@racket[(local [(define (f x-1 ... x-n) exp)] f)]. Beispielsweise hätten wir den Ausdruck von oben auch so schreiben können:

@racketblock[(map (local [(define (double x) (* 2 x))] double) lon)]

@margin-note{Man kann allerdings mit Hilfe sogenannter @italic{Fixpunktkombinatoren} Rekursion mit @racket[lambda]-Ausdrücken
                                                       simulieren.}
Dies ist kein exaktes "Desugaring". Lokale Funktionen können rekursiv sein; @racket[lambda]-Ausdrücke nicht.
Allerdings trägt @racket[lambda] sehr wohl zur Vereinfachung der Sprache bei, denn wir können nun
Funktionsdefinitionen "desugaren" zu Variablendefinitionen:

@racketblock[(define (f x-1 ... x-n) exp)]

entspricht

@racketblock[(define f (lambda (x-1 ... x-n) exp))]

Die @racket[lambda]-Ausdrücke machen also sehr deutlich, dass Funktionen "ganz normale" Werte sind, an die wir Variablen binden
können.  Wenn wir also bei obiger Definition von @racket[f] einen Funktionsaufruf @racket[(f e-1 ... e-n)] haben, so ist
das @racket[f] an der ersten Position kein Funktionsname, sondern ein Variablenname, der zu einem λ-Ausdruck ausgewertet wird.
Im Allgemeinen hat ein Funktionsaufruf also die Syntax @racket[(exp-0 exp-1 ... exp-n)], wobei alle @racket[exp-i] beliebige
Ausdrücke sind aber @racket[exp-0] bei der Auswertung eine Funktion (genauer: ein Closure) ergeben muss.

Das Wort @racket[lambda]-Ausdruck stammt aus dem λ-Kalkül, welches in den 1930er Jahren von Alonzo Church entwickelt wurde.
Das λ-Kalkül ist eine Untersprache von ISL+: Im λ-Kalkül gibt es nur λ-Ausdrücke und Funktionsapplikationen --- keine Zahlen, 
keine Wahrheitswerte, keine Variablendefinitionen, keine Listen oder Strukturen, und so weiter. Dennoch sind λ-Ausdrücke so mächtig, dass man sich
all diese Programmiersprachenfeatures innerhalb des λ-Kalkül nachbauen kann, beispielsweise mit Hilfe sogenannter 
@italic{Church-Kodierungen}.

@margin-note{Schauen Sie in DrRacket unter "Einfügen", mit welcher Tastenkombination sie den Buchstaben λ in ihr Programm
             einfügen können.}
Übrigens können Sie statt des ausgeschriebenen Wortes @racket[lambda] auch direkt den griechischen Buchstaben λ im Programmtext 
verwenden. Sie können also auch schreiben:

@racketblock[(map (λ (x) (* 2 x)) lon)]




@section{Wieso abstrahieren?}

Programme sind wie Bücher: Sie werden für Menschen (Programmierer) geschrieben und können halt nebenbei
auch noch auf einem Computer ausgeführt werden. Auf jeden Fall sollten Programme, genau wie Bücher,
keine unnötigen Wiederholungen enthalten, denn niemand möchte solche Programme lesen.

Die Einhaltung des DRY Prinzips durch die Erschaffung guter Abstraktionen hat viele Vorteile.
Bei Programmen mit wiederkehrenden Mustern besteht stehts die Gefahr der Inkonsistenz, da man
an jeder Stelle, an der das Muster wieder auftritt, dieses Muster wieder korrekt nachbilden muss.
Wir haben auch gesehen, dass es zu nicht unerheblicher Vergrößerung des Codes führen kann, wenn man
keine guten Abstraktionen hat und sich oft wiederholt.

Der wichtigste Vorteil guter Abstraktionen ist jedoch folgende: Es gibt für jede kohärente Funktionalität
des Programms genau eine Stelle, an der diese implementiert ist. Diese Eigenschaft macht es viel einfacher,
ein Programm zu schreiben und zu warten. Wenn man einen Fehler gemacht hat, ist der Fehler an einer
Stelle lokalisiert und nicht vielfach dupliziert. Wenn man die Funktionalität ändern möchte, gibt es eine
Stelle, an der man etwas ändern muss und man muss nicht alle Vorkommen eines Musters finden (was schwierig
oder praktisch unmöglich sein kann). Wichtige Eigenschaften, wie Terminierung oder Korrektheit, können Sie 
einmal für die Abstraktion nachweisen
und sie gilt dann automatisch für alle Verwendungen davon.

Diese Vorteile treffen auf @italic{alle} Arten der Abstraktion zu, die wir bisher kennengelernt haben:
Globale und lokale Funktions- und Variablendefinitionen, Abstrakte Typsignaturen, Abstrakte Datendefinitionen.

Aus diesem Grund formulieren wir folgende Richtlinie als Präzisierung des DRY-Prinzips:

@italic{Definieren Sie eine Abstraktion statt einen Teil eines Programms zu kopieren und dann zu modifizieren.}

Diese Richtlinie gilt nicht nur während der ersten Programmierung eines Programms. Auch in der Weiterentwicklung
und Wartung von Programmen sollten Sie stets darauf achten, ob es in ihrem Programm Verstöße gegen
dieses Prinzip gilt und diese Verstöße durch die Definition geeigneter Abstraktionen eliminieren.
