#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Polynome}

In den letzten beiden Kapiteln haben wir zwei neue Arten von Datendefinitionen
kennengelernt: Summentypen (in Form von Aufzählungen und Intervallen), 
mit denen man zwischen verschiedenen
Möglichkeiten auswählen kann. Produkttypen (in Form von Datendefinitionen für Strukturen), 
mit denen man Daten zusammenfassen und gruppieren kann. 

In vielen Fällen ist es sinnvoll, Summen und Produkte miteinander zu kombinieren, also zum 
Beispiel Summentypen zu definieren, bei denen einige Alternativen einen Produkttyp haben; oder Produkttypen
bei denen einige Felder Summentypen haben.

@margin-note{Die Analogie zu Polynomen in der Algebra geht allerdings noch viel weiter --- wir werden später noch etwas mehr dazu sagen.}
In der Algebra nennt man Ausdrücke, bei denen Variablen und Konstanten durch Summen und Produkte
kombiniert werden, @italic{Polynome}. In Analogie dazu werden wir von @italic{Polynomtypen} reden.
Polynomtypen werden auch als @italic{algebraische} oder @italic{induktive} Datentypen bezeichnet.

@section{Beispiel: Kollisionen zwischen Shapes}

Nehmen Sie an, sie möchten ein Computerspiel programmieren, in dem Spielfiguren in unterschiedlichen
geometrischen Formen vorkommen, zum Beispiel Kreise und Rechtecke. Diese können wir beispielsweise
so modellieren: @margin-note{Wir wählen den Namen @racket[gcircle] (für @italic{geometric} circle) um
keinen Namenskonflikt mit der @racket[circle] Funktion aus dem image.ss Teachpack zu haben.}

@#reader scribble/comment-reader
(racketblock
(define-struct gcircle (center radius))
; A GCircle is (make-gcircle Posn Number)
; interp. the geometrical representation of a circle

(define-struct grectangle (corner-ul corner-dr))
; A GRrectangle is (make-grectangle Posn Posn)
; interp. the geometrical representation of a rectangle
; where corner-ul is the upper left corner 
; and corner-dr the down right corner
)

Das interessante an diesen beiden Definitionen ist, dass Kreise und Rechtecke viele
Gemeinsamkeiten haben. Beispielsweise können beide verschoben, vergrößert oder gezeichnet werden.
Viele Algorithmen sind für beliebige geometrische Formen sinnvoll und unterscheiden
sich nur in den Details. Nennen wir die Abstraktion, die beliebige geometrische Figuren
bezeichnet, beispielsweise @racket[Shape]. Nehmen wir an, wir haben bereits 
eine Funktion, die für beliebige geometrische Formen bestimmen kann, ob sie überlappen:

@#reader scribble/comment-reader
(racketblock
; Shape Shape -> Boolean
; determines whether shape1 overlaps with shape2
(define (overlaps shape1 shape2) ...)
)

Auf Basis dieser und ähnlicher Funktionen können wir weitere Funktionen programmieren, die
völlig unabhängig vom konkreten Shape (ob Kreis oder Rechteck) sind, beispielsweise eine
Funktion, die für drei Shapes testet ob sie paarweise überlappen:

@#reader scribble/comment-reader
(racketblock
; Shape Shape Shape -> Boolean
; determines whether the shapes overlap pairwise
(define (overlaps/3 shape1 shape2 shape3)
  (and
   (overlaps shape1 shape2)
   (overlaps shape1 shape3)
   (overlaps shape2 shape3)))
)

Die Funktion @racket[overlaps] muss zwar unterscheiden, was für eine genaue Form ihre Parameter haben, aber 
die Funktion @racket[overlaps/3] nicht mehr. Dies ist eine sehr mächtige Form der Abstraktion und 
Code-Wiederverwendung: Wir formulieren abstrakte Algorithmen auf Basis einer Menge einfacher Funktionen
wie @racket[overlaps] und können dann diese Algorithmen auf @italic{alle} Arten von Shapes anwenden.

Die Funktion @racket[overlaps/3] steht also wirklich für eine ganze Familie von Algorithmen: Je nachdem, was für
Shapes ich gerade als Argument verwende, machen die Hilfsfunktionen wie @racket[overlaps] etwas anderes, aber 
der abstrakte Algorithmus in @racket[overlaps/3] ist immer gleich.

Schauen wir uns nun an, wie wir Datentypen wie @racket[Shape] und Funktionen wie @racket[overlaps] definieren können.
Um die Menge der möglichen geometrischen Figuren zu beschreiben, verwenden wir einen Summentyp, dessen Alternativen 
die Produkttypen von oben sind:

@#reader scribble/comment-reader
(racketblock
; A Shape is either:
; - a GCircle 
; - a GRectangle
; interp. a geometrical shape representing a circle or a rectangle
)

Nicht alle Algorithmen sind wie @racket[overlaps/3] --- sie sind unterschiedlich je nachdem welches Shape gerade vorliegt.
Wie bereits aus dem Entwurfsrezept für Summentypen bekannt, strukturieren wir solche Funktionen, indem wir
eine Fallunterscheidung machen.

Beispiel:

@#reader scribble/comment-reader
(racketblock
; Shape Posn -> Boolean
; Determines whether a point is inside a shape
(define (point-inside shape point)
  (cond [(gcircle? shape) (point-inside-circle shape point)]
        [(grectangle? shape) (point-inside-rectangle shape point)]))
)

In dieser Funktion fällt auf, dass in den Zweigen des @racket[cond] Ausdrucks Hilfsfunktionen aufgerufen werden, die wir noch implementieren
müssen. Man könnte stattdessen die Implementation dieser Funktionen auch direkt in den @racket[cond] Ausdruck schreiben, aber wenn die
Alternativen eines Summentyps Produkte sind, so werden diese Ausdrücke häufig so komplex, dass man sie besser in eigene Funktionen
auslagern sollte. Außerdem ergeben sich durch die Auslagerung in separate Funktionen häufig Gelegenheiten zur Wiederverwendung dieser Funktionenen.

In unserem Beispiel sehen die Implementationen dieser Hilfsfunktionen wie folgt aus:

@margin-note{Ergänzen Sie die Definitionen für @racket[vector-length] und @racket[posn-]!}
@#reader scribble/comment-reader
(racketblock
(define (point-inside-circle circle point)
  (<= (vector-length (posn- (gcircle-center circle) point)) 
      (gcircle-radius circle)))

(define (point-inside-rectangle rectangle point)
  (and
   (<= (posn-x (grectangle-corner-ul rectangle))
       (posn-x point)
       (posn-x (grectangle-corner-dr rectangle)))
   (<= (posn-y (grectangle-corner-ul rectangle))
       (posn-y point)
       (posn-y (grectangle-corner-dr rectangle)))))
)

Bei manchen Funktionen haben gleich mehrere Parameter einen Summentypen. In diesem Fall kann man erst 
für den ersten Parameter die Fallunterscheidung machen, dann geschachtelt für den zweiten Parameter usw.
Häufig kann man jedoch auch die Fallunterscheidungen zusammenfassen. Dies ist beispielsweise
bei dem oben angesprochenen @racket[overlaps] der Fall:

@#reader scribble/comment-reader
(racketblock
; Shape Shape -> Boolean
; determines whether shape1 overlaps with shape2
(define (overlaps shape1 shape2)
  (cond [(and (gcircle? shape1) (gcircle? shape2)) 
         (overlaps-circle-circle shape1 shape2)]
        [(and (grectangle? shape1) (grectangle? shape2))
         (overlaps-rectangle-rectangle shape1 shape2)]
        [(and (grectangle? shape1) (gcircle? shape2))
         (overlaps-rectangle-circle shape1 shape2)]
        [(and (gcircle? shape1) (grectangle? shape2))
         (overlaps-rectangle-circle shape2 shape1)]))
)

Auch hier haben wir wieder die Implementierung der einzelnen Fälle in Funktionen ausgelagert. Die Tatsache, dass wir
in den beiden letzten Fällen die gleiche Funktion aufrufen, illustriert, dass diese Hilfsfunktionen die Wiederverwendung
von Code fördern. Hier die Implementation der Hilfsfunktionen. Beachten Sie, dass auch in @racket[overlaps-rectangle-rectangle]
die Hilfsfunktion @racket[point-inside-rectangle] wiederverwendet wird.

@margin-note{Wenn Sie Spass an Geometrie haben, ergänzen Sie die 
             Implementation von @racket[collides-rectangle-circle]. 
             Beachten Sie, wie die Kommentare innerhalb der Funktionen
             sinnvoll die Implementation erklären.}
@#reader scribble/comment-reader
(racketblock
; GCircle GCircle -> Boolean
; determines whether c1 overlaps with c2
(define (overlaps-circle-circle c1 c2)
  ; Two circles overlap if and only if the distance of their 
  ;  centers is smaller than the sum of their radii
  (<= (vector-length (posn- (gcircle-center c1) (gcircle-center c2))) 
      (+ (gcircle-radius c1) (gcircle-radius c2))))

; GRectangle GRectangle -> Boolean
; determines whether r1 overlaps with r2
(define (overlaps-rectangle-rectangle r1 r2)
  ; Two rectangles overlap if and only if any corner of
  ; one rectangle is within the other rectangle
  (or
   (point-inside-rectangle r1 (grectangle-corner-ul r2))
   (point-inside-rectangle r1 (grectangle-corner-dr r2))
   (point-inside-rectangle r1 (make-posn 
                                   (posn-x (grectangle-corner-ul r2))
                                   (posn-y (grectangle-corner-dr r2))))
   (point-inside-rectangle r1 (make-posn 
                                   (posn-x (grectangle-corner-dr r2))
                                   (posn-y (grectangle-corner-ul r2))))))

; GRectangle GCircle -> Boolean
; determines whether r overlaps with c
(define (overlaps-rectangle-circle r c) ...)
)

@section{Programmentwurf mit Polynomtypen}
Das Entwurfsrezept aus Abschnitt @secref{entwurfsrezept} ergänzen wir wie folgt:

@itemize[#:style 'ordered
       @item{Wenn Sie eine Funktion programmieren möchten, die Informationen
             als Eingabe erhält oder als Ausgabe produziert, die man am besten
             durch Polynomtypen repräsentiert, so sollten Sie vor dem Programmieren
             dieser Funktion diesen Polynomtyp definieren (falls sie ihn nicht schon
             im Kontext einer anderen Funktion definiert haben).
             
             Ein Polynomtyp ist dann sinnvoll, wenn in ihrer Problemstellung
             unterschiedliche Arten von Informationen unterschieden werden, die jeweils
             als Produkttyp formuliert werden können, die aber ein gemeinsames Konzept 
             repräsentieren.
             
             Ein wichtiger Punkt ist, dass man Daten mit Polynomtypen hierarchisch organisieren
             kann. Die Feldtypen der Produkte, die man definiert, können also selber wieder
             einen Polynomtyp haben. Wenn also ein Produkttyp sehr viele Felder hat, oder ein
             Summentyp sehr viele Alternativen, so deutet dies darauf hin, dass Sie 
             eine tiefere Hierarchie (durch Verschachtelung von Polynomtypen) verwendensollten.}
       @item{Im zweiten Schritt, der Definition der Funktionssignatur und der Aufgabenbeschreibung,
             ändert sich nichts --- allerdings können und sollten Sie nun natürlich die Namen
             der definierten Polynomtypen verwenden.}
       @item{Nichts ändert sich im dritten Schritt. Wie immer bei Summentypen sollten Sie mindestens
             einen Test pro Alternative definieren.}
       @item{Bei der Definition des Funktionstemplates gibt es nun zwei Dimensionen: Den Summentyp
             und die Produkttypen in (einigen seiner) Alternativen.
             
             Falls wir als äußerstes einen Summentyp haben, so sollten wir zunächst in einem
             @racket[cond] Ausdruck alle Fälle unterscheiden.
             
             Für alle Alternativen, die Produkttypen sind, sollte in der Regel eine Hilfsfunktion
             definiert werden, die diesen Fall abdeckt. Diese Hilfsfunktion sollten sie in diesem Schritt auch nur
             (durch erneute Anwendung dieses Entwurfsrezepts) bis zum Template-Schritt implementieren.
             
             Nur wenn die Implementation
             dieses Falls wahrscheinlich sehr einfach ist, sollten die Selektoren für die Felder des Produkts
             in das Template mit aufgenommen werden.
             
             Allerdings gibt es einen wichtigen Fall, in dem sie @italic{keinen} @racket[cond] Ausdruck
             zur Unterscheidung der Alternativen ins Template aufnehme sollte, nämlich dann, wenn es möglich
             ist, die Funktion abstrakt zu formulieren --- sie also die Fälle nicht unterscheiden, sondern
             lediglich bereits existierende Funktionen aufrufen, die auch auf Basis des Polynomtyps implementiert
             wurden. Als Beispiel haben wir die @racket[overlap/3] Funktion gesehen.}
       @item{In diesem Schritt sollten sie aus dem Template ein lauffähiges Programm machen. Da sie im vorherigen
             Schritt eventuell Templates für Hilfsfunktionen definiert haben, müssen sie auch diese Hilfsfunktionen
             nun implementieren.}
       @item{Testen sie. Falls Tests fehlschlagen, gehen sie zurück zum vorherigen Schritt.}]
             
