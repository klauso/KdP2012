#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Produkte}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 2}

Nehmen Sie an, Sie möchten mit dem "universe" Teachpack ein Programm schreiben, welches
einen Ball simuliert, der zwischen vier Wänden hin und her prallt. Nehmen wir der Einfachheit
halber an, der Ball bewegt sich konstant mit zwei Pixeln pro Zeiteinheit.

Wenn Sie sich an das Entwurfsrezept halten, ist ihre erste Aufgabe, eine Datenrepräsentation
für all die Dinge, die sich ändern, zu definieren. Für unseren Ball mit konstanter Geschwindigkeit
sind dies zwei Eigenschaften: Die aktuelle Position sowie die Richtung, in die sich der Ball bewegt.

@margin-note{Wäre es sinnvoll, Varianten des "universe" Teachpacks zu haben, in denen der WorldState
             zum Beispiel durch zwei Werte repräsentiert wird?}}
Der WorldState im "universe" Teachpack ist allerdings nur ein einzelner Wert. Die Werte, die
wir bisher kennen, sind allerdings stets einzelne Zahlen, Bilder, Strings oder Wahrheitswerte
--- daran ändern auch Summentypen nichts. Wir müssen also irgendwie mehrere Werte so zusammenstellen
können, dass sie wie ein einzelner Wert behandelt werden können. 

Es wäre zwar denkbar, zum Beispiel zwei Zahlen in einen String reinzukodieren und bei Bedarf wieder
zu dekodieren (in der theoretischen Informatik sind solche Techniken als @italic{Gödelisierung} bekannt),
aber für die praktische Programmierung sind diese Techniken ungeeignet.

Jede höhere Programmiersprache hat Mechanismen, um mehrere Daten zu einem Datum zusammenzupacken und
bei Bedarf wieder in seine Bestandteile zu zerlegen. In BSL gibt es zu diesem Zweck
@italic{Strukturen} (@italic{structs}}. Man nennt Strukturdefinitionen auch oft @italic{Produkte}, weil 
Sie dem Kreuzprodukt von Mengen in der Mathematik entsprechen. Aus dieser Analogie ergibt sich übrigens auch
der Name 'Summentyp' aus dem letzten Kapitel, denn die Vereinigung von Mengen wird auch als die Summe der
Mengen bezeichnet. In manchen Sprachen werden Strukturen auch @italic{Records} genannt.


@section{Die @racket[posn] Struktur}

Eine Position in einem Bild wird durch zwei Zahlen eindeutig identifiziert: Die
Distanz vom linken Rand und die Distanz vom oberen Rand. Die erse Zahl nennt man die x-Koordinae
und die zweite die y-Koordinae.

In BSL werden solche Positionen mit Hilfe der @racket[posn] Struktur repräseniert. 
Eine @racket[posn] (Aussprache: Position) ist also @italic{ein} Wert, der @italic{zwei} Werte enthält.

@margin-note{Im Deutschen korrekt wäre eigentlich, von einem @italic{Exemplar} einer Struktur zu sprechen.
Es hat sich jedoch eingebürgert, analog zum Wort @italic{instance} im Englischen von einer Instanz zu reden, deshalb
werden auch wir diese grammatikalisch fragwürdige Terminologie verwenden.}
Wir können eine @italic{Instanz} der @racket[posn] Struktur mit der Funktion @racket[make-posn] erzeugen.
Die Signatur dieser Funktion ist @italic{Number Number -> Posn}.

Beispiel: Diese drei Ausdrücke erzeugen jeweils eine Instanz der @racket[posn] Struktur.

@racketblock[
(make-posn 3 4)
(make-posn 8 6)
(make-posn 5 12)]

Eine @racket[posn] hat den gleichen Status wie Zahlen oder Strings in dem Sinne, dass
Funktionen Instanzen von Strukturen konsumieren oder produzieren können.

Betrachten Sie nun eine Funktion, die die Distanz einer Position von der oberen linken Bildecke
berechnet. Hier ist die Signatur, Aufgabenbeschreibung und der Header einer solchen Funktion:

@#reader scribble/comment-reader
(racketblock
; Posn -> Number
; to compute the distance of a-posn to the origin
(define (distance-to-0 a-posn) 0)
)

Was neu ist an dieser Funktion ist, dass sie nur einen Parameter, @racket[a-posn], hat, in dem aber 
beide Koordinaten übergeben werden. Hier sind einige Tests die verdeutlichen, was @racket[distance-to-0]
berechnen soll. Die Distanz kann natürlich über die bekannte Pythagaros Formel berechnet werden.

@racketblock[
(check-expect (distance-to-0 (make-posn 0 5)) 5)
(check-expect (distance-to-0 (make-posn 7 0)) 7)
(check-expect (distance-to-0 (make-posn 3 4)) 5)
(check-expect (distance-to-0 (make-posn 8 6)) 10)]

Wie sieht nun der Funktionsbody von @racket[distance-to-0] aus? Offensichtlich müssen wir zu diesem
Zweck die x- und y-Koordinate aus dem Parameter @racket[a-posn] extrahieren. Hierzu gibt es zwei
Funktionen @racket[posn-x] und @racket[posn-y]. Die erste Funktion extrahiert die x-Koordinate, die zweite
die y-Koordinate. Hier zwei Beispiele die diese Funktionen illustrieren:

@ex[(posn-x (make-posn 3 4))
(posn-y (make-posn 3 4))]

Damit wissen wir nun genug, um @racket[distance-to-0] zu implementieren. Als Zwischenschritt
definieren wir ein Template für @racket[distance-to-0], in welchem die Ausdrücke zur Extraktion
der x- und y-Koordinate vorgezeichnet sind.

@racketblock[
(define (distance-to-0 a-posn)
  (... (posn-x a-posn) ...
   ... (posn-y a-posn) ...))]


Auf Basis dieses Templates ist es nun leicht, die Funktionsdefinition zu vervollständigen:

@racketblock[
(define (distance-to-0 a-posn)
  (sqrt
    (+ (sqr (posn-x a-posn))
       (sqr (posn-y a-posn)))))]

@section{Strukturdefinitionen}

Strukturen wie @racket[posn] werden normalerweise nicht fest in eine Programmiersprache eingebaut.
Stattdessen wird von der Sprache nur ein Mechanismus zur Verfügung gestellt, um eigene Strukturen
zu definieren. Die Menge der verfügbaren Strukturen kann also von jedem Programmierer beliebig erweitert
werden.

Eine Struktur wird durch eine spezielle Strukturdefinition erzeugt. Hier ist die Definition der @racket[posn]
Struktur in BSL:

@racketblock[(define-struct posn (x y))]

Im Allgemeinen hat eine Strukturdefinition diese Form:

@racketblock[(define-struct StructureName (FieldName ... FieldName))]

Das Schlüsselwort @racket[define-struct] bedeutet, dass hier eine Struktur definiert wird. Danach kommt
der Name der Struktur. Danach folgen, eingeschlossen in Klammern, die Namen der @italic{Felder} der Struktur.

Im Gegensatz zu einer normale Funktionsdefinition definiert man durch eine Strukturdefinition gleich einen
ganzen Satz an Funktionen, und zwar wie folgt:

@itemize[
       @item{Einen @italic{Konstruktor} --- eine Funktion, die soviele Parameter hat wie die Struktur Felder hat
             und eine Instanz der Struktur zurückliefert. Im Falle von @racket[posn] heißt diese Funktion @racket[make-posn];
             im allgemeinen Fall heißt sie @racket[make-StructureName], wobei @racket[StructureName] der Name der Struktur ist.}

       @item{Pro Feld der Struktur einen @italic{Selektor} --- eine Funktion, die den Wert eines Feldes aus
             einer Instanz der Struktur ausliest. Im Falle von @racket[posn] heißen diese Funktionen @racket[posn-x] 
             und @racket[posn-y]; im Allgemeinen heißen Sie @racket[StructureName-FieldName], wobei @racket[StructureName]
             der Name der Struktur und @racket[FielName] der Name des Feldes ist.}
       @item{Ein @italic{Strukturprädikat} --- eine boolsche Funktion, die berechnet, ob ein Wert eine Instanz
                 dieser Struktur ist. Im Falle von @racket[posn] heißt dieses Prädikat @racket[posn?]; 
                 im Allgemeinen heißt es @racket[StructureName?], wobei @racket[StructureName] der Name der Struktur ist.}]

Der Rest des Programms kann diese Funktionen benutzen als wären sie primitive Funktionen.    

@section{Verschachtelte Strukturen}
Ein sich mit konstanter Geschwindigkeit bewegender Ball im zweidimensionalen Raum 
kann durch zwei Eigenschaften beschrieben werden: Seinen Ort und die Geschwindigkeit und Richtung in die er sich bewegt. 
Wir wissen bereits wie man den Ort eines Objekts im zweidimensionalen Raum beschreibt: Mit Hilfe der @racket[posn] Struktur.
Es gibt verschiedene Möglichkeiten, die Geschwindigkeit und Richtung eines Objekts zu repräsentieren. Eine dieser
Möglichkeiten ist die, einen Bewegungsvektor anzugeben, zum Beispiel in Form einer Strukturdefinition wie dieser:

@block[(define-struct vel (deltax deltay))]

@margin-note{Der Grad der Veränderung von Einheiten wird in der Informatik (und anderswo) häufig als @italic{Delta} bezeichnet, daher der Name
             der Felder.}
Der Name @racket[vel] steht für @italic{velocity}; @racket[deltax] und @racket[deltay] beschreiben, wieviele Punkte auf der x- 
und y-Achse sich das Objekt pro Zeiteinheit bewegt.

Auf Basis dieser Definitionen können wir beispielsweise berechnen, wie sich der Ort eines Objekts ändert:
@#reader scribble/comment-reader
(racketblock
; posn vel -> posn
; computes position of loc after applying v
(check-expect (move (make-posn 5 6) (make-vel 1 2)) (make-posn 6 8))
(define (move loc v)
  (make-posn
   (+ (posn-x loc) (vel-deltax v))
   (+ (posn-y loc) (vel-deltay v))))
)   

Wie können wir nun ein sich bewegendes Balls selber repräsentieren? Wir haben gesehen, dass dieser durch seinen Ort und seinen
Bewegungsvektor beschrieben wird. Eine Möglichkeit der Repräsentation wäre diese:
@racketblock[(define-struct ball (x y deltax deltay))]

Hier ist ein Beispiel für einen Ball in dieser Repräsentation:

@racketblock[(define some-ball (make-bal 5 6 1 2))]

Allerdings geht in dieser Repräsentation die Zusammengehörigkeit der Felder verloren: Die ersten zwei Felder repräsentieren
den Ort, die anderen zwei Felder die Bewegung. Eine praktische Konsequenz ist, dass es auch umständlich ist, Funktionen
aufzurufen, die etwas mit Geschwindigkeiten und/oder Bewegungen machen aber nichts über Bälle wissen, wie zum Beispiel @racket[move]
oben, denn wir müssen die Daten erst immer manuell in die richtigen Strukturen verpacken. Um @racket[some-ball] 
mit Hilfe von @racket[move] zu bewegen, müssten wir Ausdrucke wie diesen schreiben:

@racketblock[
(move (make-posn (ball-x some-ball) (ball-y some-ball)) 
      (make-vel (ball-deltax some-ball) (ball-deltay some-ball)))]

Eine bessere Repräsentation @italic{verschachtelt} Strukturen ineinander:

@racketblock[(define-struct ball (loc vel))]

Diese Definition ist so noch nicht verschachtelt --- dies werden wir in Kürze durch Datendefinitionen
für Strukturen deutlich machen. Die Schachtelung können wir sehen, wenn wir Instanzen dieser Struktur
erzeugen. Der Beispielball @racket[some-ball] wird konstruiert, indem die Konstruktoren ineinander verschachtelt werden:

@racketblock[(define some-ball (make-ball (make-posn 5 6) (make-vel 1 2)))]

In dieser Repräsentation bleibt die logische Gruppierung der Daten intakt. Auch der Aufruf von @racket[move] gestaltet sich nun einfacher:

@racketblock[
(move (ball-loc some-ball) (ball-vel some-ball))]

Im Allgemeinen kann man durch Verschachtelung von Strukturen also Daten hierarchisch in Form eines Baums repräsentieren.

@section{Datendefinitionen für Strukturen}

Der Zweck einer Datendefinition für Strukturen ist, zu beschreiben, welche Art von Daten jedes Feld
enthalten darf. Für einige Strukturen ist die dazugehörige Datendefinition recht offensichtlich:

@#reader scribble/comment-reader
(racketblock
(define-struct posn (x y))
; A Posn is a structure: (make-posn Number Number)
; interp. the number of pixels from left and from top
)

Hier sind zwei plausible Datendefinitionen für @racket[vel] und @racket[ball]:

@#reader scribble/comment-reader
(racketblock
(define-struct vel (deltax deltay))
; a Vel is a structure: (make-vel Number Number)
interp. the velocity vector of a moving object

(define-struct ball (loc vel))
; a Ball is a structure: (make-ball Posn Vel)
; interp. the position and velocity of a ball
)

Eine Struktur hat jedoch nicht notwendigerweise genau eine zugehörige Datendefinition. Beispielsweise können
wir Bälle nicht nur im zweidimensionalen, sondern auch im ein- oder drei-dimensionalen Raum betrachten.
Im eindimensionalen Raum können Position und Velocity jeweils durch eine Zahl repräsentiert werden.
Daher können wir definieren:

@#reader scribble/comment-reader
(racketblock
; a Ball1d is a structure: (make-ball Number Number)
; interp. the position and velocity of a 1D ball
)
 
Strukturen können also "wiederverwendet" werden. Sollte man dies stets tun wenn möglich?

Prinzipiell bräuchten wir nur eine einzige Struktur mit zwei Feldern und könnten damit alle Produkttypen mit zwei Komponenten kodieren.
Beispielsweise könnten wir uns auch @racket[vel] und @racket[ball] sparen und stattdessen nur @racket[posn] verwenden:

@#reader scribble/comment-reader
(racketblock
; a Vel is a structure: (make-posn Number Number)
interp. the velocity vector of a moving object

(define-struct ball (loc vel))
; a Ball is a structure: (make-posn Posn Vel)
; interp. the position and velocity of a ball
)

Wir können sogar prinzipiell @italic{alle} Produkttypen, auch solche mit mehr als zwei Feldern, mit Hilfe von @racket[posn] ausdrücken,
indem wir @racket[posn] verschachteln.

Beipiel:

@#reader scribble/comment-reader
(racketblock
; a 3DPosn is a structure: (make-posn Number (make-posn Number Number))
; interp. the x/y/z coordinates of a point in 3D space
)

Die Sprache LISP basierte auf diesem Prinzip: Es gab in ihr nur eine universelle Datenstruktur, die sogenannte @italic{cons-Zelle}, 
die verwendet wurde, um alle Arten von Produkten zu repräsentieren. Die cons-Zelle entspricht folgender Strukturdefinition in BSL:

@margin-note{Die Namen @racket[cons], @racket[car] und @racket[cdr] haben eine Historie, die für uns aber nicht relevant ist.
             @racket[car] ist einfach der Name für die erste Komponente und @racket[cdr] der für die zweite Komponente des Paars.}
@racketblock[(define-struct cons-cell (car cdr))]

Ist diese Wiederverwendung von Strukturdefinitionen eine gute Idee? Der Preis, den man dafür bezahlt, ist, dass man die unterschiedlichen
Daten nicht mehr unterscheiden kann, weil es pro Strukturdefinition nur ein Prädikat gibt. Unsere Empfehlung ist daher, Strukturdefinitionen
nur dann in mehreren Datendefinitionen zu verwenden, wenn die unterschiedlichen Daten ein gemeinsames semantisches Konzept haben. Im
Beispiel oben gibt es für @racket[Ball] und @racket[Ball1d] das gemeinsame Konzept des Balls im n-dimensionalen Raum. Es gibt jedoch kein
sinnvolles gemeinsames Konzept für @racket[Ball] und @racket[Posn]; daher ist es nicht sinnvoll, eine gemeinsame Strukturdefinition zu verwenden.

Ein anderes sinnvolles Kriterium, um über Wiederverwendung zu entscheiden, ist die Frage, ob es wichtig ist, dass man mit einem Prädikat die
unterschiedlichen Daten unterscheiden kann --- falls ja, so sollte man jeweils eigene Strukturen verwenden.

@section{Fallstudie: Ein Ball in Bewegung}

Probieren Sie aus, was das folgende Programm macht. Verstehen Sie, wie Strukturen und Datendefinitionen verwendet wurden, um das Programm zu
strukturieren!

@#reader scribble/comment-reader
(racketblock
(define WIDTH 200)
(define HEIGHT 200)
(define BALL-IMG (circle 10 "solid" "red"))
(define BALL-CENTER-TO-EDGE (/ (image-width BALL-IMG) 2))

(define-struct vel (deltax deltay))
; a Vel is (make-vel Number Number)
; interp. the velocity vector of a moving object

(define-struct ball (loc velocity))
; a Ball is (make-ball Posn Vel)
; interp. the position and velocity of a object 

; Posn Vel -> Posn
; applies q to p and simulates the movement in one clock tick
(check-expect (posn+vel (make-posn 5 6) (make-vel 1 2)) 
              (make-posn 6 8))
(define (posn+vel p q)
  (make-posn (+ (posn-x p) (vel-deltax q)) 
             (+ (posn-y p) (vel-deltay q))))


; Ball -> Ball
; computes movement of ball in one clock tick
(check-expect (move-ball (make-ball (make-posn 20 30) 
                                    (make-vel 5 10))) 
              (make-ball (make-posn 25 40) 
                         (make-vel 5 10)))
(define (move-ball ball)
  (make-ball (posn+vel (ball-loc ball) 
                       (ball-velocity ball))
             (ball-velocity ball)))

; A Collision is either
; - "top"
; - "down"
; - "left"
; - "right"
; - "none"

; Posn -> Collision
; detects with which of the walls (if any) an object at posn collides
(check-expect (collision (make-posn 0 12))  "left")
(check-expect (collision (make-posn 15 HEIGHT)) "down")
(check-expect (collision (make-posn WIDTH 12))  "right")
(check-expect (collision (make-posn 15 0)) "top")
(check-expect (collision (make-posn 55 55)) "none")
(define (collision posn)
  (cond 
    [(<= (posn-x posn) BALL-CENTER-TO-EDGE) 
     "left"]
    [(<= (posn-y posn) BALL-CENTER-TO-EDGE) 
     "top"]
    [(>= (posn-x posn) (- WIDTH BALL-CENTER-TO-EDGE)) 
     "right"]
    [(>= (posn-y posn) (- HEIGHT BALL-CENTER-TO-EDGE)) 
     "down"]
    [else "none"]))
  
; Vel Collision -> Vel  
; computes the velocity of an object after a collision
(check-expect (bounce (make-vel 3 4) "left") 
              (make-vel -3 4))
(check-expect (bounce (make-vel 3 4) "top") 
              (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "none") 
              (make-vel 3 4))
(define (bounce vel collision)
  (cond [(or (string=? collision "left") 
             (string=? collision "right")) 
         (make-vel (- (vel-deltax vel)) 
                   (vel-deltay vel))]
        [(or (string=? collision "down") 
             (string=? collision "top")) 
         (make-vel (vel-deltax vel) 
                   (- (vel-deltay vel)))]
        [else vel]))
        
; WorldState is a Ball

; WorldState -> Image
; renders ball at its position
(check-expect (image? (render initial-ball)) true)
(define (render ball)
  (place-image BALL-IMG (posn-x (ball-loc ball)) 
               (posn-y (ball-loc ball)) 
               (empty-scene WIDTH HEIGHT)))


; WorldState -> WorldState
; moves ball to its next location
(check-expect (tick (make-ball (make-posn 20 12) (make-vel 1 2))) 
              (make-ball (make-posn 21 14) (make-vel 1 2)))
(define (tick ball)
  (move-ball (make-ball (ball-loc ball) 
                        (bounce (ball-velocity ball) 
                                (collision (ball-loc ball))))))

(define initial-ball (make-ball (make-posn 20 12)
                                (make-vel 1 2)))

(define (main ws) (big-bang ws 
                            (on-tick tick 0.01) 
                            (to-draw render)))

; start with: (main initial-ball)
)


@section{Erweiterung des Entwurfsrezepts}

Die vorhergehenden Beispiele haben gezeigt, dass viele Probleme es erfordern, parallel
mit Funktionen passende Datenstrukturen zu entwickeln. Dies bedeutet, dass sich
die Schritte des Entwurfsrezepts aus Abschnitt @secref{entwurfsrezept} wie folgt ändern:

@itemize[#:style 'ordered
       @item{Wenn in einer Problembeschreibung Information auftauchen, die zusammengehören
             oder ein Ganzes beschreiben, benötigt man Strukturen. Die Struktur korrespondiert
             zu dem "Ganzen" und hat für jede "relevante" Eigenschaft ein Feld.
             
             Eine Datendefinition für ein Feld muss einen Namen für die Menge der Instanzen
             der Struktur angeben, die durch diese Datendefinition beschrieben werden.
             Sie muss beschreiben, welche Daten für welches Feld erlaubt sind. Hierzu sollten
             nur Namen von eingebauten Datentypen oder von Ihnen bereits definierten Daten
             verwendet werden.
             
             Geben Sie in der Datendefinition Beispiele für Instanzen der Struktur, die der 
             Datendefinition entsprechen, an.}
       @item{Nichts ändert sich im zweiten Schritt.}
       @item{Verwenden Sie im dritten Schritt die Beispiele aus dem ersten Schritt, um Tests
             zu entwerfen. Wenn eines der Felder einer Struktur, die Eingabeparameter ist, einen
             Summentypen hat, so sollten Testfälle für alle Alternativen vorliegen. Bei Intervallen
             sollten die Endpunkte der Intervalle getestet werden.}
       @item{Eine Funktion die Instanzen von Strukturen als Eingabe erhält wird in vielen 
             Fällen die Felder der Strukturinstanz lesen. Um Sie an diese Möglichkeit zu erinnern,
             sollte das Template für solche Funktionen die Selektorausdrücke (zum Beispiel 
             @racket[(posn-x param)] falls @racket[param] ein Parameter vom Typ @racket[Posn] ist)
             zum Auslesen der Felder enthalten.
             
             Falls der Wert eines Feldes selber Instanz einer Struktur ist, sollten Sie jedoch
             @italic{nicht} Selektorausdrücke für die Felder dieser verschachtelten Strukturinstanz
             ins Template aufnehmen. Meistens ist es besser, die Funktionalität, die diese 
             Unterstruktur betrifft, in eine neue Hilfsfunktion auszulagern.}
       @item{Benutzen Sie die Selektorausdrücke aus dem Template um die Funktion zu implementieren.
             Beachten Sie, dass Sie möglicherweise nicht die Werte aller Felder benötigen.}
       @item{Testen Sie, sobald Sie den Funktionsheader geschrieben haben. Überprüfen Sie, dass
             zu diesem Zeitpunkt alle Tests fehlschlagen (bis auf die bei denen zufällig der eingesetzte
             Dummy-Wert richtig ist). Dieser Schritt ist wichtig, denn er bewahrt Sie vor Fehlern in
             den Tests und stellt sicher, dass ihre Tests auch wirklich eine nicht-triviale Eigenschaft testen.
             
             Testen Sie so lange, bis alle Ausdrücke im Programm während des Testens mindestens einmal
             ausgeführt wurden. Die Codefärbung in DrRacket nach dem Testen unterstützt Sie dabei.}]

 