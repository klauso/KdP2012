#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-intermediate))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
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

@section{Abstraktion in Signaturen und Datendefinitionen}

@section{Lokale Abstraktion}

@section{Funktionen als Werte}

@section{Lambda, die ultimative Abstraktion}



