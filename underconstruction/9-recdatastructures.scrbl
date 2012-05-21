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
  (cond [(person? p) 
         ...(person-name p) ... 
         ...(person-father p)...
         ...(person-mother p) ...]
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
(define (person-has-ancestor p a)
  (cond [(person? p) 
         ... (person-name p) ...
         ... (father-has-ancestor (person-father p) ...)...
         ... (mother-has-ancestor (person-mother p) ...)...]
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
(define (person-has-ancestor p a)
  (cond [(person? p) 
         ... (person-name p) ...
         ... (person-has-ancestor (person-father p) ...)...
         ... (person-has-ancestor (person-mother p) ...)...]
        [else ...]))
)

Die Struktur der Daten diktiert also die Struktur der Funktionen. Dort wo die Daten rekursiv sind, sind auch die Funktionen, die solche
Daten verarbeiten, rekursiv.

Die Vervollständigung des Templates ist nun eifach:

@#reader scribble/comment-reader
(racketblock
(define (person-has-ancestor p a)
  (cond [(person? p) 
         (or
          (string=? (person-name p) a)
          (person-has-ancestor (person-father p) a)
          (person-has-ancestor (person-mother p) a))]
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

Die @racket[FamilyTree] Datendefinition von oben steht für eine Menge von Bäumen, in denen 
jeder Knoten genau zwei ausgehende Kanten hat. Selbstverständlich können wir auch auf die
gleiche Weise Bäume repräsentieren, die drei oder fünf ausgehende Kanten haben --- indem wir
eine Alternative des Summentyps haben, in der der Datentyp drei bzw. fünfmal vorkommt.

Ein besonders wichtiger Spezialfall ist der, wo jeder Knoten genau eine ausgehende Kante hat.
Diese degenerierten Bäume nennt man auch @italic{Listen}.

@subsection{Listen, hausgemacht}
Hier ist eine mögliche Definition für Listen von Zahlen:

@#reader scribble/comment-reader
(racketblock
(define-struct lst (first rest))

; A List-of-Numbers is either:
; - (make-lst Number List-Of-Numbers)
; - false
; interp. the head and rest of a list, or the empty list
)

Hier ist ein Beispiel, wie wir eine Liste mit den Zahlen 1 bis 3 erzeugen können:

@racketblock[(make-lst 1 (make-lst 2 (make-lst 3 false)))]

Der Entwurf von Funktionen auf Listen funktioniert genau wie der Entwurf von Funktionen auf 
allen anderen Bäumen. Betrachten wir als Beispiel eine Funktion, die alle Zahlen in einer
Liste aufaddiert.

Hier ist die Spezifikation dieser Funktion:

@#reader scribble/comment-reader
(racketblock
; List-Of-Numbers -> Number
; adds up all numbers in a list
(check-expect (sum (make-lst 1 (make-lst 2 (make-lst 3 false)))) 6)
(define (sum l) ...)
)

Für das Template schlägt das Entwurfsrezept vor, die verschiedenen Alternativen zu unterscheiden und in den rekursiven
Alternativen rekursive Funktionsaufrufe einzubauen:

@racketblock[
(define (sum l)
  (cond [(lst? l) ... (lst-first l) ... (sum (lst-rest l)) ...]
        [else ...]))
]

Auf Basis dieses Templates ist die Vervollständigung nun einfach:

@racketblock[
(define (sum l)
  (cond [(lst? l) (+ (lst-first l) (sum (lst-rest l)))]
        [else 0]))
]


@subsection{Listen aus der Konserve}

Weil Listen so ein häufig vorkommender Datentyp sind, gibt es in BSL vordefinierte Funktionen für Listen.
Wie @racket[List-of-Numbers] zeigt, benötigen wir diese vordefinierten Funktionen eigentlich nicht, weil wir
Listen einfach als degenerierte Bäume repräsentieren können. Dennoch machen die eingebauten Listenfunktionen
das Programmieren mit Listen etwas komfortabler und sicherer.

Die eingebaute Konstruktorfunktion für Listen in BSL heißt @racket[cons]; die leere Liste wird nicht durch @racket[false]
sondern durch die spezielle Konstante @racket[empty] repräsentiert. 

Es ist sinnvoll, die leere Liste durch einen neuen Wert zu repräsentieren, der für nichts anderes steht, denn dann
kann es niemals zu Verwechslungen kommen. Wenn wir etwas analoges in unserer selbstgebauten Datenstruktur für
Listen machen wollten, könnten wir dies so erreichen, indem wir eine neue Struktur ohne Felder definieren und
ihren einzigen Wert als Variable definieren:

@racketblock[
(define-struct empty-lst ())
(define EMPTYLIST (make-empty-lst))]

Dementsprechend würde die Beispielliste von oben nun so konstruiert:

@racketblock[(make-lst 1 (make-lst 2 (make-lst 3 EMPTYLIST)))]
             
Die @racket[cons] Operation entspricht unserem @racket[make-lst]
von oben, allerdings mit einem wichtigen Unterschied: Sie überprüft zusätzlich, dass das zweite
Argument auch eine Liste ist:

@ex[(cons 1 (cons 2 empty))]

@ex[(cons 1 2)]

Man kann sich @racket[cons] also so vorstellen wie diese selbstgebaute Variante von @racket[cons] auf Basis von @racket[List-Of-Numbers]:

@racketblock[
(define (our-cons x l)
  (if (or (empty-lst? l) (lst? l)) 
      (make-lst x l)
      (error "second argument of cns must be a list")))]


Die wichtigsten eingebauten Listenfunktionen sind: 

@racket[empty], @racket[empty?], @racket[cons], @racket[first], @racket[rest] und @racket[cons?].

Sie entsprechen in unserem selbstgebauten Listentyp: 

@racket[EMPTYLIST], @racket[empty-lst?], @racket[our-cons], @racket[lst-first], @racket[lst-rest] und @racket[lst?].

Die Funktion, die @racket[make-lst] entspricht, wird von BSL versteckt, um sicherzustellen, dass alle Listen
stets mit @racket[cons] konstruiert werden und dementsprechend die Invariante forciert wird, dass das zweite
Feld der Struktur auch wirklich eine Liste ist. Mit unseren bisherigen Mitteln können wir dieses "Verstecken"
von Funktionen nicht nachbauen; dazu kommen wir später, wenn wir über Module reden.

Unser Beispiel von oben sieht bei Nutzung der eingebauten Listenfunktionen also so aus:

@#reader scribble/comment-reader
(racketblock
; A List-of-Numbers is one of:
; - (cons Number List-Of-Numbers)
; - empty

; List-Of-Numbers -> Number
; adds up all numbers in a list
(check-expect (sum (cons 1 (cons 2 (cons 3 empty)))) 6)
(define (sum l)
  (cond [(cons? l) (+ (first l) (sum (rest l)))]
        [else 0]))
)

@subsection{Die @racket[list] Funktion}

Es stellt sich schnell als etwas mühselig heraus, Listen mit Hilfe von @racket[cons] und @racket[empty] zu konstruieren.
Aus diesem Grund gibt es etwas syntaktischen Zucker, um Listen komfortabler zu erzeugen: Die @racket[list] Funktion.

Mit der @racket[list] Funktion können wir die Liste @racket[(cons 1 (cons 2 (cons 3 empty)))] so erzeugen: 

@racketblock[(list 1 2 3)]

Die @racket[list] Funktion ist jedoch nur etwas syntaktischer Zucker, der wie folgt definiert ist:

@racketblock[(list exp-1 ... exp-n)]

steht für n verschachtelte @racket[cons] Ausdrücke:
 
@racketblock[(cons exp-1 (cons ... (cons exp-n empty)))]

Es ist wichtig, zu verstehen, dass dies wirklich nur eine abkürzende Schreibweise ist. Allerdings ist diese Schreibweise 
nicht nur leichter zu schreiben, sondern auch leichter zu lesen. Aus diesem Grund sollten Sie ab diesem
Zeitpunkt ein neues Sprachlevel aktivieren, nämlich "Anfänger mit Listenabkürzungen". Der Unterschied zum "Anfänger" Level
liegt darin, dass im neuen Sprachlevel Listen in der Form @racket[(list 1 2 3)] ausgegeben werden
und nicht @racket[(cons 1 (cons 2 (cons 3 empty)))]. Dennoch ist es weiterhin so, dass @racket[list] nur
eine Abkürzung für die Benutzung von @racket[cons] und @racket[empty] ist.

@subsection{Datentypdefinitionen für Listen}

Wir haben oben eine Datendefinition @racket[List-of-Numbers] gesehen. Sollen wir auch für @racket[List-of-Strings]
oder @racket[List-of-Booleans] eigene Datendefinitionen schreiben? Sollen diese die gleiche Struktur benutzen, oder
sollen wir separate Strukturen für jede dieser Datentypen haben?

Es ist sinnvoll, dass alle diese Datentypen die gleiche Struktur benutzen, nämlich in Form der eingebauten 
Listenfunktionen. Hierfür gibt es zwei Gründe: Erstens wären all diese Strukturen sehr ähnlich und wir würden damit
gegen das DRY Prinzip verstoßen. Zweitens gibt es eine ganze Reihe von Funktionen, die auf @italic{beliebigen}
Listen arbeiten, zum Beispiel eine Funktion @racket[second], die das zweite Listenelement einer Liste zurückgibt:

@racketblock[
(define (second l)
  (if (or (empty? l) (empty? (rest l)))
      (error "need at least two list elements")
      (first (rest l))))]

Diese Funktion (die übrigens schon vordefiniert ist, genau wie @racket[third], @racket[fourth] und so weiter)
funktioniert für beliebige Listen, unabhängig von der Art der gespeicherten Daten. Solche Funktionen könnten wir
nicht schreiben, wenn wir für je nach Typ der Listenelemente andere Strukturen verwenden würden.

Der häufigste Anwendungsfall von Listen ist der, dass die Listen @italic{homogen} sind. Das bedeutet, dass 
alle Listenelemente einen gemeinsamen Typ haben. Dies ist keine sehr große Einschränkung, denn dieser
gemeinsame Typ kann beispielsweise auch ein Summentyp mit vielen Alternativen sein. Für diesen Fall
verwenden wir die Kurzschreibweise @racket[List-of-]@italic{name}, um die entsprechende Instanz
des folgenden Schemas für Datendefinition zu bezeichnen:

@#reader scribble/comment-reader
(racketblock
; A List-of-names is one of:
; - (cons name List-Of-name)
; - empty
)

In Zukunft werden wir also einfach Datentypen wie @racket[List-of-String], @racket[List-of-Boolean], 
@racket[List-of-List-of-String] oder @racket[List-of-FamilyTree] verwenden und meinen damit implizit
die oben angeführte Datendefinition.

Was aber ist ein geeigneter Datentyp für die Signatur von @racket[second] oben, also im Allgemeinen
für Funktionen, die auf beliebigen Listen funktionieren?

Hierfür verwenden wir die Schreibweise @racket[List-of-X], @racket[List-of-Y] und so weiter, also zum Beispiel so:

@#reader scribble/comment-reader
(racketblock
; List-of-X -> X
(define (second l) ...)
)

Wir machen mit dieser Signatur deutlich, dass wir für @racket[X] (und jede andere vorkommende @italic{Typvariable})
jeden beliebigen Typ einsetzen können und @racket[second] dann diesen Typ hat. Also hat @racket[second]
zum Beispiel den Typ List-of-Number -> Number oder List-of-List-of-String -> List-of-String.

Allerdings werden wir im Moment nur im Ausnahmefall Funktionen wie @racket[second] selber programmieren. 
Die meisten Funktionen, die wir im Moment programmieren wollen, verarbeiten Listen mit einem konkreten Elementtyp.
Auf sogenannte @italic{polymorphe} Funktionen wie @racket[second] werden wir später noch zurückkommen.

@subsection{Aber sind Listen wirklich rekursive Datenstrukturen?}
Wenn man sich Listen aus der realen Welt anschaut (auf einem Blatt Papier, in einer Tabellenkalkulation etc.), 
so suggerieren diese häufig keine rekursive, verschachtelte Struktur sondern sehen "flach" aus. Ist es also
"natürlich", Listen so wie wir es getan haben, über einen rekursiven Datentyp - als degenerierten Baum - zu definieren?

In vielen (vor allem älteren) Programmiersprachen gibt es eine direktere Unterstützung für Listen. Listen sind in
diesen Programmiersprachen fest eingebaut. Listen sind in diesen Sprachen nicht rekursiv; stattdessen wird häufig über
einen Index auf die Elemente der Liste zugegriffen. Um die Programmierung mit Listen zu unterstützen, gibt es 
einen ganzen Satz von Programmiersprachenkonstrukten (wie z.B. verschiedenen Schleifen- und Iterationskonstrukten), 
die der Unterstützung dieser fest eingebauten Listen dienen.

@margin-note{Es gibt allerdings auch durchaus Situationen, in denen rekursiv aufgebaute Listen effizienter
             sind. Mehr dazu später.}
Aus Hardware-Sicht sind solche Listen, über die man mit einem Index zugreift, sehr natürlich, denn sie entsprechen
gut dem von der Hardware unterstützen Zugriff auf den Hauptspeicher. 
Insofern sind diese speziellen Listenkonstrukte zum Teil durch ihre Effizienz begründet. Es gibt auch in Racket 
(allerdings nicht in BSL) solche Listen; dort heißen sie @italic{Vektoren}.

Der Reiz der rekursiven Formulierung liegt darin, dass man keinerlei zusätzliche Unterstützung für Listen in der
Programmiersprache benötigt. Wir haben ja oben gesehen, dass wir uns die Art von Listen, die von BSL direkt
unterstützt werden, auch selber programmieren können. Es ist einfach "yet another recursive datatype", und alle
Konstrukte, Entwurfsrezepte und so weiter funktionieren nahtlos auch für Listen. Wir brauchen keine speziellen
Schleifen oder andere Spezialkonstrukte um Listen zu verarbeiten, sondern machen einfach das, was wir auch
bei jedem anderen rekursiven Datentyp machen. Das ist bei fest eingebauten
Index-basierten Listen anders; es gibt viele Beispiele für Sprachkonstrukte, die für "normale Werte" funktionieren, 
aber nicht für Listen, und umgekehrt. 

BSL und Racket stehen in der Tradition der Programmiersprache @italic{Scheme}. Die Philosophie, die im ersten
Satz der Sprachspezifikation von Scheme (@url{http://www.r6rs.org}) zu finden ist, ist damit auch für BSL und Racket gültig:

@italic{Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses 
and restrictions that make additional features appear necessary. Scheme demonstrates that a very small number of 
rules for forming expressions, with no restrictions on how they are composed, suffice to form a practical and efficient 
programming language that is flexible enough to support most of the major programming paradigms in use today.}

Die Behandlung von Listen als rekursive Datentypen ist ein Beispiel für diese Philosophie.

Manche Programmieranfänger finden es nicht intuitiv, Listen und Funktionen darauf rekursiv zu formulieren.
Aber ist es nicht besser, ein Universalwerkzeug zu verwenden, welches in sehr vielen Situationen verwendbar ist,
als ein Spezialwerkzeug, das in nichts besser ist als das Universalwerkzeug und nur in einer Situation anwendbar ist?

@subsection{Natürliche Zahlen als rekursive Datenstruktur}
Es gibt in BSL nicht nur Funktionen, die Listen konsumieren, sondern auch solche, die Listen
produzieren. Eine davon ist @racket[make-list]. Hier ein Beispiel:

@ex[(make-list 4 "abc")]

Die @racket[make-list] Funktion konsumiert also eine natürliche Zahl n und einen Wert und produziert eine Liste mit n Wiederholungen
des Werts. Obwohl diese Funktion also nur atomare Daten konsumiert, produziert sie ein beliebig großes
Resultat. Wie ist das möglich?

@margin-note{Auch in der Mathematik werden natürliche Zahlen ähnlich rekursiv definiert. Recherchieren
             sie, was die @italic{Peano-Axiome} sind.}
Eine erleuchtende Antwort darauf ist, dass man auch natürliche Zahlen als Instanzen eines rekursiven Datentyps
ansehen kann. Hier ist eine mögliche Definition:

@#reader scribble/comment-reader
(racketblock
; A Nat (Natural Number) is one of:
; – 0
; – (add1 Nat)
)

Beispielsweise können wir die Zahl @racket[3] repräsentieren als @racket[(add1 (add1 (add1 0)))]. Die @racket[add1]
Funktion hat also eine Rolle ähnlich zu den Konstruktorfunktionen bei Strukturen. Die Rolle der Selektorfunktion
wird durch die Funktion @racket[sub1] übernommen. Das Prädikat für die erste Alternative von Nat ist @racket[zero?];
das Prädikat für die zweite Alternative heißt @racket[positive?].

Mit dieser Sichtweise sind wir nun in der Lage, mit unserem Standard-Entwurfsrezept Funktionen wie @racket[make-list]
selber zu definieren. Nennen wir unsere Variante von @racket[make-list] @racket[iterate-value]. Hier ist
die Spezifikation:

@#reader scribble/comment-reader
(racketblock
; Nat X -> List-of-X
; creates a list with n occurences of x
(check-expect (iterate-value 3 "abc") (list "abc" "abc" "abc"))
(define (iterate-value n x) ...)
)

Gemäß unseres Entwurfsrezepts für rekursive Datentypen erhalten wir folgendes Template:

@racketblock[
(define (iterate-value n x)
  (cond [(zero? n) ...]
        [(positive? n) ... (iterate-value (sub1 n) ...)...]))]


Dieses Template zu vervollständigen ist nun nur noch ein kleiner Schritt:

@racketblock[
(define (iterate-value n x)
  (cond [(zero? n) empty]
        [(positive? n) (cons x (iterate-value (sub1 n) x))]))]

@section{Das neue Entwurfsrezept}

Lesen Sie hierzu Abschnitt @hyperlink["http://www.ccs.neu.edu/home/matthias/HtDP2e/htdp2e-part2.html#%28part._design~3alists%29"]{4.2 in HTDP/2e}.