#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Daten beliebiger Größe}


Die Datentypen, die wir bisher gesehen haben, repräsentieren grundsätzlich
Daten, die aus einer festen Anzahl von atomaren Daten bestehen. Dies 
liegt daran, dass jede Datendefinition nur andere Datendefinitionen verwenden
darf, die vorher bereits definiert wurden. Betrachten wir zum Beispiel

@#reader scribble/comment-reader
(racketblock
(define-struct gcircle (center radius))
; A GCircle is (make-gcircle Posn Number)
; interp. the geometrical representation of a circle
)

so wissen wir, dass eine @racket[Posn] aus zwei und eine @racket[Number]
aus einem atomaren Datum (jeweils Zahlen) besteht und damit
ein @racket[GCircle] aus genau drei atomaren Daten.

In vielen Situationen wissen wir jedoch nicht zum Zeitpunkt des Programmierens, aus
wievielen atomaren Daten ein zusammengesetztes Datum besteht. In diesem Kapitel befassen
wir uns damit, wie wir Daten beliebiger (und zum Zeitpunkt des Programmierens unbekannter)
Größe repräsentieren und Funktionen, die solche Daten verarbeiten, programmieren können.


@section{Rekursive Datentypen}

Betrachten wir als Beispiel ein Programm, mit dem Stammbäume von Personen verwaltet werden können.
Jede Person im Stammbaum hat einen Vater und eine Mutter; manchmal sind Vater oder Mutter einer
Person auch unbekannt.

Mit den bisherigen Mitteln könnten wir zum Beispiel so vorgehen, um die Vorfahren einer Person zu repräsentieren:

@#reader scribble/comment-reader
(racketblock
(define-struct parent (name grandfather grandmother))
; A Parent is: (make-parent String String String)
; interp. the name of a person with the names of his/her grandparents

(define-struct person (name father mother))
; A Person is: (make-person String Parent Parent)
; interp. the name of a person with his/her parents
)

Allerdings können wir so nur die Vorfahren bis zu genau den Großeltern repräsentieren. Natürlich könnten wir noch
weitere Definitionen für die Urgroßeltern usw. hinzufügen, aber stets hätten wir eine beim Programmieren festgelegte
Größe. Außerdem verstoßen wir gegen das DRY Prinzip, denn die Datendefinitionen sehen für jede Vorgängergeneration
sehr ähnlich aus.

Was können wir machen um mit diesem Problem umzugehen? Betrachten wir etwas genauer, was für Daten wir hier eigentlich
modellieren wollen. Die Erkenntnis, die wir hier benötigen, ist, dass ein Stammbaum eine rekursive Struktur hat:
Der Stammbaum einer Person besteht aus dem Namen der Person und dem Stammbaum seiner Mutter und dem Stammbaum seiner Eltern.
Ein Stammbaum hat also eine Eigenschaft, die man auch @italic{Selbstähnlichkeit} nennt: Ein Teil eines Ganzen hat die 
gleiche Struktur wie das Ganze.

Dies bedeutet, dass wir die Restriktion fallen lassen müssen, dass in einer Datendefinition nur die Namen bereits vorher 
definierter Datentypen auftauchen dürfen. In unserem Beispiel schreiben wir:

@#reader scribble/comment-reader
(racketblock
(define-struct person (name father mother))

; A FamilyTree is: (make-person name FamilyTree FamilyTree)
; interp. the name of a person and the tree of his/her parents.
)

Die Definition von @racket[FamilyTree] benutzt also selber wieder @racket[FamilyTree].
Erstmal ist nicht ganz klar, was das bedeuten soll. Vor allen Dingen ist aber auch nicht
klar, wie wir einen Stammbaum erzeugen sollen. Wenn wir versuchen, einen zu erzeugen,
bekommen wir ein Problem:

@racket[(make-familytree "Heinz" (make-familytree "Horst" (make-familytree "Joe" ...)))]

Wir können überhaupt gar keinen Stammbaum erzeugen, weil wir zur Erzeugung bereits einen
Stammbaum haben müssen. Ein Ausdruck, der einen Stammbaum erzeugt, wäre also unendlich groß.

Bei genauer Überlegung sind Stammbäume aber niemals unendlich groß, sondern sie enden bei
irgendeiner Generation - zum Beispiel weil die Vorfahren unbekannt oder nicht von Interesse sind.

Diesem Tatbestand können wir dadurch Rechnung tragen, dass wir aus @racket[FamilyTree] einen
Summentyp machen, und zwar wie folgt:

@#reader scribble/comment-reader
(racketblock
(define-struct person (name father mother))

; A FamilyTree is either:
;- (make-person name FamilyTree FamilyTree)
;- false
; interp. either the name of a person and the tree of its parents,
; or false if the person is not known/relevant.
)

Dieser neue, nicht-rekursive Basisfall erlaubt es uns nun, Werte dieses Typen zu erzeugen.
Hier ist ein Beispiel:

@racketblock[
(define HEINZ 
  (make-person "Heinz" 
               (make-person "Elke" false false)
               (make-person "Horst" 
                            (make-person "Joe" 
                                         false
                                         (make-person "Rita" 
                                                      false 
                                                      false))
                            false)))]             


Was bedeutet also so eine rekursive Datendefinition? Wir haben bisher Datentypen immer
als Mengen von Werten aus dem Datenuniversum interpretiert, und dies ist auch 
weiterhin möglich, nur die Konstruktion der Menge, die der Typ repräsentiert, ist nun
etwas aufwendiger:

Sei ft@subscript{1} die Menge {false}, ft@subscript{2} die Vereinigung aus ft@subscript{1} und der Menge der @racket[(make-person name false false)] für alle 
Strings @racket[name]. Im Allgemeinen sei ft@subscript{n} die Vereinigung aus ft@subscript{n-1} und der
Menge der @racket[(make-person name p1 p2)] für alle Strings @racket[name] sowie für alle @racket[p1] und
alle @racket[p2] aus ft@subscript{n-1}. Beispielsweise ist @racket[HEINZ] ein Element von ft@subscript{5} (und 
damit auch ft@subscript{6}, ft@subscript{7} usw.) aber nicht von ft@subscript{4}.

Dann ist die Bedeutung von @racket[FamilyTree] die Vereinigung aller dieser Mengen, also ft@subscript{1}
vereinigt mit ft@subscript{2} vereinigt mit ft@subscript{3} vereinigt mit ... .

Aus dieser Mengenkonstruktion wird auch klar, wieso rekursive Datentypen es ermöglichen, Daten beliebiger
Größe zu repräsentieren: Jede Menge ft@subscript{i} enthält die Werte, deren maximale Tiefe in Baumdarstellung i ist.
Da wir alle ft@subscript{i} miteinander vereinigen, ist die Tiefe (und damit auch die Größe) unbegrenzt.

@section{Programmieren mit rekursiven Datentypen}

Im letzten Abschnitt haben wir gesehen, wie man rekursive Datentypen definiert, was sie bedeuten, und wie man Instanzen
dieser Datentypen erzeugt. Nun wollen wir überlegen, wie man Funktionen programmieren kann, die Instanzen
solcher Typen als Argumente haben oder als Resultat produzieren.

Betrachten Sie dazu folgende Aufgabe: Programmieren sie eine Funktion, die herausfindet, ob es im Stammbaum
einer Person einen Vorfahren mit einem bestimmten Namen gibt.

Eine Signatur, Aufgabenbeschreibung und Tests sind dazu schnell definiert:

@#reader scribble/comment-reader
(racketblock
; FamilyTree -> Boolean
; determines whether person p has an ancestor a
(check-expect (person-has-ancestor HEINZ "Joe") true)
(check-expect (person-has-ancestor HEINZ "Emil") false)
(define (person-has-ancestor p a) ...)
)

Da @racket[FamilyTree] ein Summentyp ist, sagt unser Entwurfsrezept, dass wir eine Fallunterscheidung machen. 
In jedem Zweig der Fallunterscheidung können wir die Selektoren für die Felder eintragen. In
unserem Beispiel ergibt sich:

@#reader scribble/comment-reader
(racketblock
(define (person-has-ancestor p a)
  (cond [(person? a) 
         ...(person-name a) ... 
         ...(person-father a)...
         ...(person-mother a) ...]
        [else ...]))
)

@margin-note{Wir interpretieren das Wort "Vorfahr" so, dass eine Person ihr eigener Vorfahr ist. Wie 
             müsste das Programm aussehen, um die nicht-reflexive Interpretation des Worts "Vorfahr"
             zu realisieren?}
Die Ausdrücke @racket[(person-father a)] und @racket[(person-mother a)] stehen für Werte, die einen komplexen
Summentypen, nämlich wieder @racket[FamilyTree] haben. Offensichtlich hat eine Person einen Vorfahren
@racket[a], wenn die Person entweder selber @racket[a] ist oder die Mutter oder der Vater einen
Vorfahr mit dem Namen @racket[a] hat.

Unser Entwurfsrezept schlägt für Fälle, in denen Felder einer Strukur selber einen komplexen Summen-/Produkttyp
haben, vor, eigene Hilfsfunktionen zu definieren. Dies können wir wie folgt andeuten:

@#reader scribble/comment-reader
(racketblock
(define (person-has-ancestor a name)
  (cond [(person? a) 
         ... (person-name a) ...
         ... (father-has-ancestor (person-father a) ...)...
         ... (mother-has-ancestor (person-mother a) ...)...]
        [else ...]))
)


Wenn wir uns jedoch etwas genauer überlegen, was @racket[father-has-ancestor] und @racket[mother-has-ancestor] tun sollen,
stellen wir fest, dass sie die gleiche Signatur und Aufgabenbeschreibung haben wie @racket[person-has-ancestor]!
Das bedeutet, dass die Templates für diese Funktionen wiederum jeweils zwei neue Hilfsfunktionen erfordern. Da wir nicht
wissen, wie tief der Stammbaum ist, können wir auf diese Weise das Programm also gar nicht realisieren.

Zum Glück haben wir aber bereits eine Funktion, deren Aufgabe identisch mit der von 
@racket[father-has-ancestor] und @racket[mother-has-ancestor] ist, nämlich
@racket[person-has-ancestor] selbst. Dies motiviert das folgende Template:

@#reader scribble/comment-reader
(racketblock
(define (person-has-ancestor a name)
  (cond [(person? a) 
         ... (person-name a) ...
         ... (person-has-ancestor (person-father a) ...)...
         ... (person-has-ancestor (person-mother a) ...)...]
        [else ...]))
)

Die Struktur der Daten diktiert also die Struktur der Funktionen. Dort wo die Daten rekursiv sind, sind auch die Funktionen, die solche
Daten verarbeiten, rekursiv.

Die Vervollständigung des Templates ist nun eifach:

@#reader scribble/comment-reader
(racketblock
(define (person-has-ancestor p a)
  (cond [(person? a) 
         (or
          (string=? (person-name a) name)
          (person-has-ancestor (person-father a) name)
          (person-has-ancestor (person-mother a) name))]
        [else false]))
)


Die erfolgreich ablaufenden Tests illustrieren, dass diese Funktion anscheinend das tut, was sie soll, aber wieso?

Vergleichen wir die Funktion mit einem anderen Ansatz, der nicht so erfolgreich ist. Betrachten wir nochmal 
den Anfang der Programmierung der Funktion, diesmal mit anderem Namen:

@#reader scribble/comment-reader
(racketblock
; FamilyTree -> Boolean
; determines whether person p has an ancestor a
(check-expect (person-has-ancestor-stupid HEINZ "Joe") true)
(check-expect (person-has-ancestor-stupid HEINZ "Emil") false)
(define (person-has-ancestor-stupid p a) ...)
)

Wenn wir schon eine Funktion hätten, die bestimmen kann, ob eine Person einen bestimmten Vorfahren hat,
könnten wir einfach diese Funktion aufrufen. Aber zum Glück sind wir ja schon gerade dabei, diese Funktion
zu programmieren. Also rufen wir doch einfach diese Funktion auf:

@#reader scribble/comment-reader
(racketblock
; FamilyTree -> Boolean
; determines whether person p has an ancestor a
(check-expect (person-has-ancestor-stupid HEINZ "Joe") true)
(check-expect (person-has-ancestor-stupid HEINZ "Emil") false)
(define (person-has-ancestor-stupid p a) 
  (person-has-ancestor-stupid p a))
)

Irgendwie scheint diese Lösung zu einfach zu sein. Ein Ausführen der Tests bestätigt den Verdacht.
Allerdings schlagen die Tests nicht fehl, sondern die Ausführung der Tests terminiert nicht und
wir müssen sie durch Drücken auf die "Stop" Taste abbrechen.

Wieso funktioniert @racket[person-has-ancestor] aber nicht @racket[person-has-ancestor-stupid]?

Zunächst einmal können wir die Programme operationell miteinander vergleichen. Wenn wir den
Ausdruck @racket[(person-has-ancestor-stupid HEINZ "Joe")] betrachten, so sagt unsere
Reduktionssemantik:

@racket[(person-has-ancestor-stupid HEINZ "Joe")] @step @racket[(person-has-ancestor-stupid HEINZ "Joe")]

Es gibt also keinerlei Fortschritt bei der Auswertung. Dies erklärt, wieso die Ausführung nicht stoppt.

Dies ist anders bei @racket[(person-has-ancestor HEINZ "Joe")]. Die rekursiven Aufrufe rufen 
@racket[person-has-ancestor] stets auf Vorfahren der Person auf. Da der Stammbaum nur eine endliche
Tiefe hat, muss irgendwann @racket[(person-has-ancestor false "Joe")] aufgerufen werden, und wir
landen im zweiten Fall des konditionalen Ausdrucks, der keine rekursiven Ausdrücke mehr enthält.

Wir können das Programm auch aus Sicht der Bedeutung der rekursiven Datentypdefinition betrachten. 
Für jede Eingabe @racket[p] gibt es ein minimales i so dass @racket[p] in ft@subscript{i} ist.
Für @racket[HEINZ] ist dieses i=5. Da die Werte der @racket[mother] und @racket[father] Felder von @racket[p] damit 
in ft@subscript{i-1} sind, ist klar, dass der @racket[p] Parameter bei allen rekursiven Aufrufe in ft@subscript{i-1} ist.
Da das Programm offensichtlich für Werte aus ft@subscript{0} (im Beispiel die Menge {false}) terminiert, ist
klar, dass dann auch alle Aufrufe mit Werten aus ft@subscript{1} terminieren müssen, damit dann aber auch
die Aufrufe mit Werten aus ft@subscript{2} und so weiter. Damit haben wir gezeigt, dass die Funktion
für alle Werte aus ft@subscript{i} für ein beliebiges i wohldefiniert ist --- mit anderen Worten:
für alle Werte aus @racket[FamilyTree].

Dieses informell vorgetragene Argument aus dem vorherigen Absatz ist mathematisch gesehen ein
Induktionsbeweis.

Bei @racket[person-has-ancestor-stupid] ist die Lage anders, da im rekursiven Aufruf das Argument 
eben nicht aus ft@subscript{i-1} ist.

Die Schlussfolgerung aus diesen Überlegungen ist, dass Rekursion in Funktionen unproblematisch
und wohldefiniert ist, solange sie der Struktur der Daten folgt. Da Werte rekursiver
Datentypen eine unbestimmte aber endliche Größe haben, ist diese sogenannte @italic{strukturelle Rekursion}
stets wohldefiniert. Unser Entwurfsrezept schlägt vor, dass immer dort wo Datentypen rekursiv sind,
auch die Funktionen, die darauf operieren, (strukturell) rekursiv sein sollen.

Bevor wir uns das angepasste Entwurfsrezept im Detail anschauen, wollen wir erst noch überlegen, wie
Funktionen aussehen, die Instanzen rekursive Datentypen @italic{produzieren}.

Als Beispiel betrachten wir eine Funktion, die sehr nützlich ist, um den eigenen Stammbaum etwas 
eindrucksvoller aussehen zu lassen, nämlich eine, mit der der Name aller Vorfahren um einen
Titel ergänzt werden kann.

Hier ist die Signatur, Aufgabenbeschreibung und ein Test für diese Funktion:

@#reader scribble/comment-reader
(racketblock
; FamilyTree -> FamilyTree
; prefixes all members of a family tree p with title t
(check-expect 
 (promote HEINZ "Dr. ")
 (make-person 
  "Dr. Heinz" 
  (make-person "Dr. Elke" false false) 
  (make-person 
   "Dr. Horst" 
   (make-person "Dr. Joe" false 
                (make-person "Dr. Rita" false false)) false)))
 
(define (promote p t) ...)
)

Die Funktion konsumiert und produziert also gleichzeitig einen Wert vom Typ @racket[FamilyTree].
Das Template für solche Funktionen unterscheidet sich nicht von dem für 
@racket[person-has-ancestor]. Auch hier wenden wir wieder dort Rekursion an, wo auch der
Datentyp rekursiv ist:

@#reader scribble/comment-reader
(racketblock
(define (promote p t)
  (cond [(person? p) 
            ...(person-name p)...
            ...(promote (person-father p) ...)...
            ...(promote (person-mother p) ...)...]
        [else ...]))
)

Nun ist es recht einfach, die Funktion zu vervollständigen. Um Werte vom Typ @racket[FamilyTree]
zu produzieren, bauen wir die Resultate der rekursiven Aufrufe in einen Aufrufe
des @racket[make-person] Konstruktors ein:


@#reader scribble/comment-reader
(racketblock
(define (promote p t)
  (cond [(person? p) 
         (make-person
            (string-append t (person-name p))
            (promote (person-father p) t)
            (promote (person-mother p) t))]
        [else p]))
)

@section{Listen}