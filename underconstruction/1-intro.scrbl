#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))

   
@title[#:version ""]{Programmieren mit Ausdrücken}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 1}

@section{Programmieren mit arithmetischen Ausdrücken}
 
Jeder von Ihnen weiß, wie man Zahlen addiert, Kaffee kocht, oder einen Schrank eines schwedischen Möbelhauses zusammenbaut.
Die Abfolge von Schritten, die sie hierzu durchführen, nennt man @italic{Algorithmus}, und Sie wissen
wie man einen solchen Algorithmus ausführt. 
In diesem Kurs werden wir die Rollen umdrehen: Sie werden den Algorithmus programmieren, und der Computer 
wird ihn ausführen.  Eine formale Sprache, in der solche Algorithmen formuliert werden können, heißt
@italic{Programmiersprache}. Die Programmiersprache, die wir zunächst verwenden werden, heißt
@italic{BSL}. BSL steht für "Beginning Student Language". Zum Editieren und Ausführen der BSL Programme
verwenden wir @italic{DrRacket}. DrRacket kann man unter der URL @url{http://racket-lang.org/} herunterladen.
Bitte stellen Sie als Sprache "How To Design Programs - Anfänger" ein. Folgen Sie diesem Text am besten,
indem Sie DrRacket parallel starten und immer mitprogrammieren.

Viele einfache Algorithmen sind in einer Programmiersprache bereits vorgegeben, z.B. solche
zur Arithmetik mit Zahlen. Wir können "Aufgaben" stellen, indem wir DrRacket eine Frage stellen,
auf die uns DrRacket dann im Ausgabefenster die Antwort gibt. So können wir zum Beispiel die Frage

@racketblock[(+ 1 1)]

im @italic{Definitionsbereich} (dem oberen Teil der DrRacket Oberfläche) stellen --- als Antwort erhalten wir im @italic{Interaktionsbereich} (dem Bereich unterhalb
des Definitionsbereichs) bei Auswertung dieses Programms ("Start" Knopf) @ev[(+ 1 1)].
Im Definitionsbereich 
 schreiben und editieren Sie ihre Programme. Sobald Sie hier etwas ändern, taucht der "Speichern" Knopf
 auf, mit dem Sie die Definitionen in einer Datei abspeichern können. Im Interaktionsbereich wird das Ergebnis einer Programmausführung angezeigt;
 außerdem können hier Ausdrücke eingegeben werden, die sofort ausgewertet werden aber nicht in der Datei mit abgespeichert werden.

@todo{Screenshot zur Beschreibung der DrRacket Oberfläche?}

 Die Art von Programmen bzw. Fragen wie @racket[(+ 1 1)] nennen wir @italic{Ausdrücke}. In Zukunft werden wir solche Frage/Antwort Interaktionen
wie folgt darstellen:
 
@ex[(+ 1 1)] 

Operationen wie @racket[+] nennen wir im Folgenden @italic{Funktionen}. Die Operanden wie @racket[1] nennen wir @italic{Argumente}.
Hier einige weitere Beispiele für Ausdrücke mit anderen Funktionen:

@ex[(+ 2 2)
(* 3 3)
(- 4 2)
(/ 6 2)
(sqr 3)
(expt 2 3)
(sin 0)]

Die Argumente dieser Funktionen sind jeweils Zahlen, und das Ergebnis ist auch wieder eine Zahl. Wir können auch direkt eine Zahl als Ausdruck verwenden. Zum Beispiel:

@ex[5]

DrRacket zeigt als Ergebnis wieder genau dieselbe Zahl an. Eine Zahl, die direkt in einem Ausdruck verwendet wird, heißt auch Zahlen@italic{literal}.

Für manche Ausdrücke kann der Computer das mathematisch korrekte Ergebnis nicht berechnen.
Stattdessen erhalten wir eine Annäherung an das mathematisch korrekte Ergebnis. Zum Beispiel:

@ex[(sqrt 2)]
@ex[(cos pi)]

Das @racket[i] im letzten Ergebnis steht für "inexact", also ungenau. In BSL kann man an diesem @racket[i] sehen, ob eine
Zahl ein exaktes Ergebnis oder nur ein angenähertes Ergebnis ist.


 Programme beinhalten Ausdrücke. Alle Programme, die wir bisher gesehen haben, @italic{sind} Ausdrücke.
 Jeder von Ihnen kennt Ausdrücke aus der Mathematik. Zu diesem Zeitpunkt ist ein Ausdruck in unserer 
 Programmiersprache entweder eine Zahl, oder etwas, das mit einer linken Klammer "(" startet und mit
 einer rechten Klammer ")" endet.
 Wir bezeichnen Zahlen als @italic{atomare Ausdrücke} und Ausdrücke, die mit einer Klammer starten, als @italic{zusammengesetzte Ausdrücke}.
 Später werden andere Arten von Ausdrücken hinzukommen.

 
 
 Wie kann man mehr als zwei Zahlen addieren? Hierzu gibt es zwei Möglichkeiten:

 Durch Schachtelung:

@ex[(+ 2 (+ 3 4))]

oder durch Addition mit mehr als zwei Argumenten:

@ex[(+ 2 3 4)]

Immer wenn Sie in BSL eine Funktion wie @racket[+] oder @racket[sqrt] benutzen möchten,
schreiben Sie eine öffnende Klammer, gefolgt vom Namen der Funktion, dann einem Lehrzeichen 
(oder Zeilenumbruch) und dann die Argumente der Funktion, also in unserem Fall die Zahlen, auf die die
Funktion angewandt werden soll. 

Am Beispiel der Schachtelung haben Sie gesehen, dass auch zusammengesetzte Ausdrücke als Argumente zugelassen sind.
Diese Schachtelung kann beliebig tief sein:

@ex[(+ (* 5 5) (+ (* 3 (/ 12 4)) 4))]

Das Ergebnis für einen solchen geschachtelten Ausdruck wird so berechnet, wie sie es auch auf einem Blatt Papier
machen würden: Wenn ein Argument ein zusammengesetzter Ausdruck ist, so wird zunächst das Ergebnis für diesen Ausdruck berechnet. 
Dieser Unterausdruck ist möglicherweise selber wieder geschachtelt; in diesem Fall wird diese
Berechnungsvorschrift auch auf diese Unterausdrücke wieder angewendet (@italic{rekursive} Anwendung).
Falls mehrere Argumente zusammengesetzte Ausdrücke sind, so wird von links nach rechts berechnet.

@todo{Hier würde passen: "Wie in der Mathematik können sie gleiches mit gleichem ersetzen."}

Zusammengefasst ist Programmieren zu diesem Zeitpunkt das Schreiben von arithmetischen Ausdrücken.
Ein Programm auszuführen bedeutet, den Wert der darin enthaltenen Ausdrücke zu berechnen.
Ein Drücken auf "Start" bewirkt die Ausführung des Programms im Definitionsbereich; die Ergebnisse
der Ausführung werden im Interaktionsbereich angezeigt.

Noch ein praktischer Hinweis: Wenn Sie dieses Dokument mit einem Webbrowser lesen, sollten alle Funktionen, die 
in den Beispielausdrücken vorkommen, einen Hyperlink zu ihrer Dokumentation enthalten. Beispielsweise
sollte der Additionsoperator im Ausdruck @racket[(+ 5 7)] einen solchen Hyperlink enthalten. Unter diesen
Links finden Sie auch eine Übersicht über die weiteren Operationen, die sie verwenden können.

@section[#:tag "arithmeticnm"]{Arithmetik mit nicht-numerischen Werten}

Wenn wir nur Programme schreiben könnten, die Zahlen verarbeiten, wäre Programmieren genau so 
langweilig wie Mathematik ;-) Zum Glück gibt es viele andere Arten von Werten, mit denen 
wir ganz analog zu Zahlen rechnen können, zum Beispiel Text, Wahrheitswerte, Bilder usw.

Zu jedem dieser sogenannten @italic{Datentypen} gibt es @italic{Konstruktoren}, mit denen man Werte dieser
 Datentypen konstruieren kann, sowie @italic{Funktionen}, die auf Werte dieses Datentyps angewendet
 werden können und die weitere Werte des Datentyps konstruieren. Konstruktoren für numerische
 Werte sind zum Beispiel @racket[42] oder @racket[5.3] (also die Zahlen@italic{literale}; Funktionen sind zum Beispiel 
 @racket[+] oder @racket[*].

Die Konstruktoren für Text (im folgenden auch @italic{String} genannt) erkennt man an Anführungszeihen. So ist zum Beispiel

@racket["Konzepte der Programmiersprachen"]

ein Stringliteral. Eine Funktion auf diesem Datentyp ist @racket[string-append], zum Beispiel

@ex[(string-append "Konzepte der " "Programmiersprachen")]

Es gibt weitere Funktionen auf Strings: Um Teile aus einem String zu extrahieren, um die Reihenfolge
der Buchstaben umzukehren, um in Groß- oder Kleinbuchstaben zu konvertieren usw. Zusammen bilden diese
Funktionen die @italic{Arithmetik der Strings}.


Die Namen dieser ganzen Funktionen muss man sich nicht merken; bei Bedarf können die zur Verfügung stehenden
Funktionen für Zahlen, Strings und andere Datentypen in der DrRacket Hilfe nachgeschlagen werden
unter: Hilfe -> How to Design Programs Languages -> Beginning Student -> Pre-defined Functions

Einige Funktionen haben die Eigenschaft, dass sie Werte eines Datentyps als Funktion erwarten, aber
Werte eines anderen Datentyps als Ergebnis liefern, zum Beispiel die Funktion @racket[string-length]:

@ex[(+ (string-length "Programmiersprachen") 5)]

Bei Funktionen, die mehrere Argumente erwarten, gibt es solche, die Argumente unterschiedlicher Datentypen
erwarten, zum Beispiel

@ex[(replicate 3 "hi")]

Es gibt auch Funktionen, die Datentypen ineinander umwandeln, zum Beispiel

@ex[(number->string 42)
    (string->number "42")]

Ein weiterer wichtiger Datentyp sind Wahrheitswerte (Boolsche Werte). Die einzigen
Konstruktoren hierfür sind die Literale @racket[true] und @racket[false]. Funktionen auf boolschen
Werten sind zum Beispiel die aussagenlogischen Funktionen:

@ex[
(and true true) 
(and true false)
(or true false) 
(or false false)
(not false)] 

Boolsche Werte werden auch häufig von Vergleichsfumktionen zurückgegeben:

@ex[(> 10 9) 
(< -1 0) 
(= 42 9) 
(string=? "hello" "world")
]

Natürlich können Ausdrücke weiterhin beliebig verschachtelt werden, z.B. so:

@margin-note{Beachten Sie in diesem Beispiel wie die Einrückung des Textes
hilft, zu verstehen, welcher Teilausdruck Argument welcher Funktion ist. Probieren
Sie in DrRacket aus, wie die Funktionen "Einrücken" bzw. "Alles einrücken"
im Menü "Racket" die Einrückung ihres Programms verändern.}
@ex[(and (or (= (string-length "hello world") (string->number "11"))
             (string=? "hello world" "good morning"))
         (>= (+ (string-length "hello world") 60) 80))]


Der letzte Datentyp den wir heute einführen werden, sind Bilder. In BSL sind
Bilder "ganz normale" Werte, mit dazugehöriger Arithmetik, also Funktionen darauf.
Existierende Bilder können per copy&paste oder über das Menü "Einfügen -> Bild" direkt in
das Programm eingefügt werden. Wenn Sie dieses Dokument im Browser betrachten, können Sie
das Bild dieser Rakete @ev[rocket] mit Copy&Paste in ihr Programm einfügen. Genau wie die Auswertung
einer Zahl die Zahl selber ergibt, ergibt die Auswertung des Bilds das Bild selber.

@racket[>] @ev[rocket]

@ev[rocket]

Wie auf anderen Datentypen sind auch auf Bildern eine Reihe von Funktionen verfügbar.
Beispielsweise kann die Fläche des Bildes durch diesen Ausdruck berechnet werden:

@racket[>] @racket[(* (image-width (unsyntax @ev[rocket]))  (image-height (unsyntax @ev[rocket])))]

@ev[
  (* (image-width rocket)
   (image-height rocket))]

Statt existierende Bilder in das Programm einzufügen kann man auch neue Bilder konstruieren:
   
@ex[(circle 10 "solid" "red")
    (rectangle 30 20 "solid" "blue")]



Die Arithmetik der Bilder umfasst nicht nur Funktionen um Bilder zu konstruieren, sondern auch
um Bilder in verschiedener Weise zu kombinieren:

@ex[(overlay (circle 5 "solid" "red")
             (rectangle 20 20 "solid" "blue"))]


Zwei wichtige Funktionen die Sie noch kennen sollten sind @racket[empty-scence] und @racket[place-image]. Die erste
erzeugt eine Szene, ein spezielles Rechteck in dem Bilder plaziert werden können.
Die zweite Funktionen setzt ein Bild in eine Szene:

@ex[
(place-image (circle 5 "solid" "green") ; ergibt 
             50 80
             (empty-scene 100 100))]     


@section{Auftreten und Umgang mit Fehlern}

 Bei der Erstellung von Programmen können unterschiedliche Arten von Fehlern auftreten.
 Es ist wichtig, die Klassen und Ursachen dieser Fehler zu kennen.

 Eine wichtige Art von Fehlern sind @italic{Syntaxfehler}. Ein Beispiel für ein Programm
 mit einem Syntaxfehler sind die Ausdrücke (+ 2 3(  oder (+ 2 3 oder (+ 2 (+ 3 4).

 Syntaxfehler werden vor der Programmausführung von DrRacket geprüft und gefunden;
 diese Prüfung kann auch mit der Schaltfläche "Syntaxprüfung" veranlasst werden.

 Ein Syntaxfehler tritt auf, wenn ein BSL Programm nicht zur BSL Grammatik passt.
 Später werden wir diese Grammatik genau definieren; informell ist eine Grammatik 
 eine Menge von Vorschriften über die Struktur korrekter Programme.
 
 Die Grammatik von dem Teil von BSL, den sie bereits kennen, ist sehr einfach:
@itemize[
 @item{Ein BSL Programm ist eine Sequenz von Ausdrücken.}
 @item{Ein Ausdruck ist eine Zahl, ein Bild, ein Boolscher Wert, ein String, oder ein Funktionsaufruf.}
 @item{Ein Funktionsaufruf hat die Form @racket[(f a1 a2 ...)] wobei @racket[f] der name einer Funktion ist und 
                                        @racket[a1],@racket[a2],... Ausdrücke sind.}
 ]

Die folgenden Programme sind alle syntaktisch korrekt, allerdings lassen sich nicht alle diese Programme auswerten: 

@ex[ (+ 2 (* 3 4))
 (number->string "asdf")
 (string-length "asdf" "fdsa")
 (/ 1 0)
 (string->number "asdf")]

 Nicht jedes syntaktische korrekte Programm hat in BSL eine Bedeutung. @italic{Bedeutung} heißt in diesem
 Fall dass das Programm korrekt ausgeführt werden kann und einen Wert zurückliefert.
 Die Menge der BSL Programme, die eine Bedeutung haben, ist nur eine @italic{Teilmenge} der syntaktisch
 korrekten BSL Programme.

  Die Ausführung des Programms @racket[(number->string "asdf")] ergibt einen @italic{Laufzeitfehler}, ein Fehler der auftritt während
das Programm läuft (im Unterschied zu Syntaxfehlern, die @italic{vor} der Programmausführung erkannt werden).
Wenn in BSL ein Laufzeitfehler auftritt, wird die Programmausführung abgebrochen und eine 
 Fehlermeldung ausgegeben.
 
 Dieser Fehler ist ein Beispiel für einen @italic{Typfehler}: Die Funktion erwartet, dass ein Argument
 einen bestimmten Typ hat, diesem Fall 'Zahl', aber tatsächlich hat das Argument einen anderen
 Typ, in diesem Fall 'String'.

 Ein anderer Fehler, der auftreten kann, ist der, dass die Anzahl der angegebenen Argumente nicht 
 zu der Funktion passt. Im Programm @racket[ (string-length "asdf" "fdsa")] tritt dieser Fehler.

 Manchmal stimmt zwar der Datentyp des Arguments, aber trotzdem 'passt' der Argument
 in irgendeiner Weise nicht. Im Beispiel @racket[(/ 1 0)] ist es so, dass die Divionsfunktion als Argumente
 Zahlen erwartet. Das zweite Argument ist eine Zahl, trotzdem resultiert die Ausführung in einer
 Fehlermeldung, denn die Division durch Null ist nicht definiert.

 Programmiersprachen unterscheiden sich darin, zu welchem Zeitpunkt solche Fehler 
 gefunden werden, also zum Beispiel vor der Ausführung oder erst während der Ausführung. Im Allgemeinen
 gilt: Je früher desto besser! - allerdings muss diese zusätzliche Sicherheit häufig mit anderen
 Restriktionen erkauft werden, zum Beispiel der, dass einige korrekte Programme nicht
 mehr ausgeführt werden können.

 Eine andere Situation liegt im Fall @racket[(string->number "asdf")] vor. Die Ausführung des Programms 
 ergibt den Wert @racket[false].
 Dieser Wert signalisiert, dass der übergebene String keine Zahl repräsentiert.
 In diesem Fall tritt also @italic{kein} Laufzeitfehler auf, sondern die Ausführung wird fortgesetzt.
 Der Aufrufer von @racket[(string->number "asdf")] hat dadurch die Möglichkeit, abzufragen, ob die
 Umwandlung erfolgreich war oder nicht, und je nachdem das Programm anders fortzusetzen.
 Das Programm ist also aus BSL-Sicht wohldefiniert. Die Funktion @racket[string->number] hätte alternativ
 aber auch so definiert werden können, dass sie in dieser Situation einen Laufzeitfehler auslöst.


@section[#:tag "semanticsofexpressions"]{Bedeutung von BSL Ausdrücken}


 Fassen wir nochmal den jetzigen Stand zusammen: Programmieren ist das Aufschreiben arithmetischer Ausdrücke,
 wobei die Arithmetik Zahlen, Strings, Boolsche Werte und Bilder umfasst. Programme sind syntaktisch
 korrekt wenn Sie gemäß der Regeln aus dem vorherigen Abschnitt konstruiert wurden. Nur syntaktisch
 korrekte Programme (aber nicht alle) haben in BSL eine Bedeutung. Die Bedeutung eines Ausdrucks in BSL
 ist ein Wert, und dieser Wert wird durch folgende Auswertungsvorschriften ermittelt:

@itemize[
 @item{Zahlen, Strings, Bilder und Wahrheitswerte sind Werte. Wir benutzen im Rest dieser Vorschrift Varianten des Buchstaben @v als Platzhalter für  Ausdrücke die Werte sind und Varianten des Buchstaben @e für beliebige Ausdrücke (Merkhilfe: Wert = @italic{v}alue, Ausdruck = @italic{e}xpression).}        
 @item{Ist der Ausdruck bereits ein Wert so ist seine Bedeutung dieser Wert.}
 @item{ Hat der Ausdruck die Form @racket[(f (unsyntax @e1) ... (unsyntax @eN))], wobei @racket[f] ein Funktionsname und @e1,..., @eN  Ausdrücke sind, so wird dieser Ausdruck
                                  wie folgt ausgewertet:
   @itemize[
       @item{Falls @e1,..., @eN  bereits Werte @v1,...,@vN sind und 
                   @racket[f] auf  den  Werten @v1,...,@vN definiert ist, so so ist der Wert des Ausdrucks die Anwendung von @racket[f] auf @v1,...,@vN }     
       @item{Falls @e1,...,@eN bereits Werte @v1,...,@vN sind aber 
                   @racket[f] ist @italic{nicht} auf den  Werten @v1,...,@vN definiert, dann wird die Auswertung mit einer passenden Fehlermeldung abgebrochen.}
       @item{Falls mindestens eines der Argumente noch kein Wert ist, so werden durch Anwendung der gleichen Auswertungsvorschriften 
             die Werte aller Argumente bestimmt, so dass dann die vorherige Auswertungsregel anwendbar ist.}                                                                                                                                                                                     
                                                                                                                                                                                      
          ]}                                 
 
]

Diese Vorschriften können wir als Anleitung zur schrittweisen @italic{Reduktionen} von Programmen auffassen. Wir schreiben @e @step @(prime e) um zu
sagen, dass @e in einem Schritt zu @(prime e) reduziert werden kann. Werte können nicht reduziert werden. Die Reduktion ist wie folgt definiert:

@margin-note{Experimentieren Sie in DrRacket mit dem "Stepper" Knopf: Geben Sie einen komplexen
Ausdruck in den Definitionsbereich ein, drücken Sie auf "Stepper" und dann auf die "Schritt nach rechts"
Taste und beobachten was passiert.}
   @itemize[
       @item{Falls der Ausdruck die Form @racket[(f (unsyntax @v1) ... (unsyntax @vN))] hat und
                   die Anwendung von @racket[f] auf   @racket[v1], @racket[v2],... den Wert @racket[v] ergibt, dann 
                   @racket[(f (unsyntax @v1) ... (unsyntax @vN))]  @step @racket[v].}
       @item{Falls ein Audruck @e1 einen Unterausdruck @e2 enthält, der reduziert werden kann, also @e2 @step @(prime @e2), dann
       gilt @e1 step @(prime @e1), wobei @(prime @e1) aus @e1 entsteht indem @e2 durch @(prime @e2) ersetzt wird.}]
       
Die letzte Regel nennen wir die @italic{Kongruenzregel}. In dem Teil der Sprache, den Sie bereits kennengelernt haben, 
können nur Funktionsaufrufe Unterausdrücke haben, daher gilt
in diesem Fall folgender Spezialfall der Kongruenzregel: Fall @eI @step @(prime @eI), 
dann @racket[(f (unsyntax @e1) ... (unsyntax @eN))] @step @racket[(f (unsyntax @e1) ... (unsyntax @eI-1) (unsyntax @(prime @eI)) (unsyntax @eI+1) ...)]. 
              
Wir benutzen folgende Konventionen:

@itemize[
         @item{@e1 @step @e2 @step @e3 ist eine Kurzschreibweise für @e1 @step @e2 und @e2 @step @e3}
         @item{Wenn wir schreiben @e @multistep @(prime @e) so bedeutet dies dass es ein n ≥ 0 und @e1, ...,@eN gibt so dass 
                                  @e @step @e1 @step ... @step  @eN @step @(prime @e) gilt. Insbesondere gilt @e @multistep @e .
           Man sagt, @multistep ist der reflexive und transitive Abschluss von @step .}]



Beispiele:

@itemize[
@item{@racket[(+ 1 1)] @step @racket[2].}

@item{@racket[(+ (* 2 3) (* 4 5))] @step @racket[(+ 6 (* 4 5))] @step @racket[(+ 6 20)] @step @racket[26].}

@item{@racket[(+ (* 2 3) (* 4 5))] @step @racket[(+ (* 2 3) 20)] @step @racket[(+ 6 20)] @step @racket[26].}

@item{@racket[(+ (* 2 (+ 1 2) (* 4 5)))] @step @racket[(+ (* 2 3) (* 4 5))].}

@item{@racket[(+ (* 2 3) (* 4 5))] @multistep @racket[26] aber nicht @racket[(+ (* 2 3) (* 4 5))] @step @racket[26].}

@item{@racket[(+ 1 1)] @multistep @racket[(+ 1 1)] aber nicht @racket[(+ 1 1)] @step @racket[(+ 1 1)].}

@item{@racket[(overlay (circle 5 "solid" "red")  (rectangle 20 20 "solid" "blue"))] @step @racket[(overlay (unsyntax @ev[(circle 5 "solid" "red")])  (rectangle 20 20 "solid" "blue"))]
       @step @racket[(overlay (unsyntax @ev[(circle 5 "solid" "red")])  (unsyntax @ev[(rectangle 20 20 "solid" "blue")]))]  
       @step @ev[(overlay (circle 5 "solid" "red")  (rectangle 20 20 "solid" "blue"))].}

]

Im Allgemeinen kann ein Ausdruck mehrere reduzierbare Unterausdrücke haben, also die Kongruenzregeln an mehreren Stellen gleichzeitig einsetzbar sein.
In den Beispielen oben haben wir zum Beispiel @racket[(+ (* 2 3) (* 4 5))] @step @racket[(+ (* 2 3) 20)] aber auch 
@racket[(+ (* 2 3) (* 4 5))] @step @racket[(+ 6 (* 4 5))]. Es ist jedoch nicht schwer zu sehen, dass immer wenn
wir die Situation @e1 @step e2 und @e1 step @e3 haben, dann gibt es ein @e4 so dass gilt @e2 @multistep @e4 und @e3 @multistep @e4 . 
Diese Eigenschaft nennt man @italic{Konfluenz}. Reduktionen die auseinanderlaufen können also immer wieder zusammengeführt werden;
der Wert den man am Ende erhält ist auf jeden Fall eindeutig.


Auf Basis dieser Reduktionsregeln können wir nun eine definieren, wann zwei Programme die gleiche Bedeutung haben.
Zwei Ausdrücke @e1 und @e2 haben die gleiche Bedeutung, geschrieben @e1 @equiv @e2 , falls es einen Wert @v gibt
so dass @e1 @multistep @v und @e2 @multistep @v . 

Beispiele:

@itemize[
@item{@racket[(+ 1 1)] @equiv @racket[2].}

@item{@racket[(+ (* 2 3) 20)]  @equiv @racket[(+ 6 (* 4 5))] @equiv @racket[26].}

@item{ @racket[(overlay (unsyntax @ev[(circle 5 "solid" "red")])  (rectangle 20 20 "solid" "blue"))]
       @equiv @racket[(overlay (circle 5 "solid" "red") (unsyntax @ev[(rectangle 20 20 "solid" "blue")]))].}

]

Die Rechtfertigung für diese Definition liegt in folgender wichtiger Eigenschaft begründet: Wir können innerhalb eines großen Programms Teilausdrücke beliebig durch Teilausdrücke mit der gleichen Bedeutung ersetzen, ohne die Bedeutung des Gesamtprogramms zu verändern. Etwas formaler können wir das so ausdrücken: 

Sei @e1 ein Unterausdruck eines größeren Ausdrucks @e2 und @e1 @equiv @e3 . Ferner sei @(prime @e2) eine Kopie von @e2 in dem Vorkommen von @e1 durch @e3 ersetzt wurden. Dann gilt: @e2 @equiv @(prime @e2). 

Diese Eigenschaft folgt direkt aus der Kongruenzregel und der Definition von @equiv . Dieser Gleichheitsbegriff ist identisch mit dem, den Sie aus der Schulmathematik kennen, wenn Sie Gleichungen umformen, zum Beispiel wenn wir a + a - b umformen zu 2a - b weil wir wissen dass a + a = 2a. 

Die Benutzung von @equiv um zu zeigen dass Programme die gleiche Bedeutung haben nennt man auch @italic{equational reasoning}.

