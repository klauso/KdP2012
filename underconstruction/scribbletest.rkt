#lang scribble/manual
@(require scribble/eval)
@(require "marburgscript.rkt")


   
@title{Konzepte der Programmiersprachen, SS 2012}
@author["Prof. Dr. Klaus Ostermann" "Tillmann Rendel"]

@section{Programmieren mit Ausdrücken}
Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 1

@subsection{Programmieren mit arithmetischen Ausdrücken}
 
Jeder von Ihnen weiß, wie man Zahlen addiert, dividiert oder multipliziert, denn Sie wurden
in der Schule von den Lehrern mit einem Verfahren (einem sog. @italic{Algorithmus}) dazu "programmiert". 
In diesem Kurs werden wir die Rollen umdrehen: Sie werden programmieren, und der Computer 
wird ihre Anweisungen ausführen.  Die Sprache in der wir diese Anweisungen formulieren heißt
@italic{Programmiersprache}. Die Programmiersprache, die wir zunächst verwenden werden, heißt
@italic{BSL}. BSL steht für "Beginning Student Language". Zum Editieren und Ausführen der BSL Programme
verwenden wir @italic{DrRacket}. DrRacket kann man unter der URL @url{http://racket-lang.org/} herunterladen.
Bitte stellen Sie als Sprache "How To Design Programs - Anfänger" ein.

Viele einfache Algorithmen sind in einer Programmiersprache bereits vorgegeben, z.B. solche
zur Arithmetik mit Zahlen. Wir können "Aufgaben" stellen, indem wir DrRacket eine Frage stellen,
auf die uns DrRacket dann im Ausgabefenster die Antwort gibt. So können wir zum Beispiel die Frage

@racketblock[(+ 1 1)]

stellen --- als Antwort erhalten wir bei Ausführung dieser Anweisung ("Start" Knopf) @ev[(+ 1 1)].
Diese Art von Fragen nennen wir @italic{Ausdrücke}. In Zukunft werden wir solche Frage/Antwort Interaktionen
wie folgt darstellen:
 
@ex[(+ 1 1)] 

Hier einige weitere Beispiele für Ausdrücke mit weiteren arithmetischen Operationen.
@ex[(+ 2 2)
(* 3 3)
(- 4 2)
(/ 6 2)
(sqr 3)
(expt 2 3)
(sin 0)
pi
(cos pi)]  
         
Das @racket[i] im Ergebnis @racket[#i-1.0] steht für "inexact", also ungenau - im Unterschied zur
Mathematik sind manche Berechnungen auf einem Computer notwendigerweise nur Annäherungen
an das mathematisch korrekte Ergebnis


 Der Bereich, in dem Sie diesen Text lesen, ist der @italic{Definitionsbereich}. In diesem Bereich 
 schreiben und editieren Sie ihre Programme. Sobald Sie hier etwas ändern, taucht der "Speichern" Knopf
 auf, mit dem Sie die Definitionen abspeichern können.

 Programme beinhalten Ausdrücke. Alle Programme, die wir bisher gesehen haben, @italic{sind} Ausdrücke.
 Jeder von Ihnen kennt Ausdrücke aus der Mathematik. Zu diesem Zeitpunkt ist ein Ausdruck in unserer 
 Programmiersprache ist entweder eine Zahl, oder etwas das mit einer linken Klammer "(" startet und mit
 einer rechten Klammer ")" endet. Wir bezeichnen Zahlen als @italic{primitive Ausdrücke}. 
 Später werden andere Arten von Ausdrücken hinzukommen.

 Wenn Sie auf "Start" drücken, wertet DrRacket die Ausdrücke von oben nach unten aus und zeigt die 
 Ergebnisse im @italic{Interaktionsbereich} (der Bereich unter dem Definitionsbereich). Sie können auch 
 direkt im Interaktionsbereich Ausdrücke eingeben, die dann sofort ausgewertet werden. Allerdings 
 werden die Ausdrücke im Interaktionsbereich nicht durch den "Speichern" Knopf mit abgespeichert.

 Wie kann man mehr als zwei Zahlen addieren? Hierzu gibt es zwei Möglichkeiten:

 Durch Schachtelung:

@ex[(+ 2 (+ 3 4))]

oder durch Addition mit mehr als zwei Operanden

@ex[(+ 2 3 4)]

Immer wenn Sie in BSL eine arithmetische Operation wie @racket[+] oder @racket[sqrt] benutzen möchten,
schreiben Sie eine öffnende Klammer, gefolgt von der Operation, dann einem Lehrzeichen 
(oder Zeilenumbruch) und dann die @italic{Operanden}, also in unserem Fall die Zahlen auf die die
Operation angewandt werden soll.

Am Beispiel der Schachtelung haben Sie gesehen, dass auch Ausdrücke als Operanden zugelassen sind.
Diese Schachtelung kann beliebig tief sein:

@ex[(+ (* 5 5) (+ (* 3 (/ 12 4)) 4))]


Solche geschachtelten Ausdrücke werden so ausgewertet, wie Sie es auch auf einem Blatt Papier 
machen würden: Wenn ein Operand ein nicht-primitiver Ausdruck ist, so wird zunächst dieser Ausdruck berechnet. 
Dieser Unterausdruck ist möglicherweise selber wieder geschachtelt; in diesem Fall wird diese
Berechnungsvorschrift auch auf diese Unterausdrücke wieder angewendet (@italic{rekursive} Anwendung).
Falls mehrer Operanden nicht-primitive Ausdrücke sind, so wird von links nach rechts ausgewertet.

Zusammengefasst ist Programmieren zu diesem Zeitpunkt das Schreiben von arithmetischen Ausdrücken.
Ein Programm auszuführen bedeutet den Wert der darin enthaltenen Ausdrücke zu berechnen.
Ein Drücken auf "Start" bewirkt die Ausführung des Programms im Definitionsbereich; die Resultate
der Ausführung werden im Interaktionsbereich angezeigt.


@subsection{Arithmetik mit nicht-numerischen Werten}

Wenn wir nur Programme schreiben könnten, die Zahlen verarbeiten, wäre Programmieren genau so 
langweilig wie Mathematik ;-) Zum Glück gibt es viele andere Arten von Werten, mit denen 
wir ganz analog zu Zahlen rechnen können, zum Beispiel Text, Wahrheitswerte, Bilder usw.

Zu jedem dieser sogenannten @italic{Datentypen} gibt es @italic{Konstruktoren}, mit denen man Werte dieser
 Datentypen konstruieren kann, sowie @italic{Operationen}, die auf Werte dieses Datentyps angewendet
 werden können und die weitere Werte des Datentyps konstruieren. Konstruktoren für numerische
 Werte sind zum Beispiel @racket[42] oder @racket[5.3] (also die Zahlen@italic{literale}; Operationen sind zum Beispiel 
 @racket[+] oder @racket[*].

Die Konstruktoren für Text (im folgenden auch @italic{String} genannt) erkennt man an Anführungszeihen. So ist zum Beispiel

@racket["Konzepte der Programmiersprachen"]

ein Stringliteral. Eine Operation auf diesem Datentyp ist @racket[string-append], zum Beispiel

@ex[(string-append "Konzepte der " "Programmiersprachen")]

Es gibt weitere Operationen auf Strings: Um Teile aus einem String zu extrahieren, um die Reihenfolge
der Buchstaben umzukehren, um in Groß- oder Kleinbuchstaben zu konvertieren usw. Zusammen bilden diese
Operationen die @italic{Arithmetik der Strings}.


Die Namen dieser ganzen Operationen muss man sich nicht merken; bei Bedarf können die zur Verfügung stehenden
Operationen für Zahlen, Strings und andere Datentypen in der DrRacket Hilfe nachgeschlagen werden
unter: Hilfe -> How to Design Programs Languages -> Beginning Student -> Pre-defined Functions

Einige Operationen haben die Eigenschaft, dass sie Werte eines Datentyps als Operand erwarten, aber
Werte eines anderen Datentyps als Ergebnis liefern, zum Beispiel die Operation @racket[string-length]:

@ex[(+ (string-length "Programmiersprachen") 5)]

Bei Operationen, die mehrere Operanden erwarten, gibt es solche, die Operanden unterschiedlicher Datentypen
erwarten, zum Beispiel

@ex[(replicate 3 "hi")]

Es gibt auch Operationen, die Datentypen ineinander umwandeln, zum Beispiel

@ex[(number->string 42)
    (string->number "42")]

Ein weiterer wichtiger Datentyp sind Wahrheitswerte (Boolsche Werte). Die einzigen
Konstruktoren hierfür sind die Literale @racket[true] und @racket[false]. Operationen auf boolschen
Werten sind zum Beispiel die aussagenlogischen Operationen:

@ex[
(and true true) 
(and true false)
(or true false) 
(or false false)
(not false)] 

Boolsche Werte werden auch häufig von Vergleichsoperationen zurückgegeben:

@ex[(> 10 9) 
(< -1 0) 
(= 42 9) 
(string=? "hello" "world")
]

Natürlich können Ausdrücke weiterhin beliebig verschachtelt werden, z.B. so:

@ex[(and (or (= (string-length "hello world") (string->number "11"))
         (string=? "hello world" "good morning"))
     (>= (+ (string-length "hello world") 60) 80))]


Der letzte Datentyp den wir heute einführen werden, sind Bilder. In BSL sind
Bilder "ganz normale" Werte, mit dazugehöriger Arithmetik, also Operationen darauf.
Existierende Bilder können per copy&paste oder über das Menü "Einfügen -> Bild" direkt in
das Programm eingefügt werden. Hier ist ein Bild einer Rackete. Genau wie die Auswertung
einer Zahl die Zahl selber ergibt, ergibt die Auswertung des Bilds das Bild selber.



Hier ein Beispiel für die Verwendung einer Operation auf Bildern:

@racketblock[
  (* (image-width )
   (image-height ))]


Statt existierende Bilder in das Programm einzufügen kann man auch neue Bilder konstruieren:

@;{
   
@ex[(circle 10 "solid" "red")]



(rectangle 30 20 "outline" "blue") ; ergibt 

; Die Arithmetik der Bilder umfasst nicht nur Operationen um Bilder zu konstruieren, sondern auch
; um Bilder in verschiedener Weise zu kombinieren:

(overlay (circle 5 "solid" "red")
         (rectangle 20 20 "solid" "blue")) ; ergibt 


; Zwei wichtige Operationen die Sie noch kennen sollten sind empty-scence und place-image. Die erste
; erzeugt eine Szene, ein spezielles Rechteck in dem Bilder plaziert werden können.
; Die zweite Operation setzt ein Bild in eine Szene:

(place-image (circle 5 "solid" "green") ; ergibt 
             50 80
             (empty-scene 100 100))     


; Auftreten und Umgang mit Fehlern
; ================================

; Bei der Erstellung von Programmen können unterschiedliche Arten von Fehlern auftreten.
; Es ist wichtig, die Klassen und Ursachen dieser Fehler zu kennen.

; Eine wichtige Art von Fehlern sind _Syntaxfehler_. Ein Beispiel für ein Programm
; mit einem Syntaxfehler sind die Ausdrücke "(+ 2 3("  oder "(+ 2 3" oder "(+ 2 (+ 3 4)"
;
; Syntaxfehler werden vor der Programmausführung von DrRacket geprüft und gefunden;
; diese Prüfung kann auch mit der Schaltfläche "Syntaxprüfung" veranlasst werden.
;
; Ein Syntaxfehler tritt auf, wenn ein BSL Programm nicht zur BSL Grammatik passt.
; Später werden wir diese Grammatik genau definieren; informell ist eine Grammatik 
; eine Menge von Vorschriften über die Struktur korrekter Programme.
; 
; Die Grammatik von dem Teil von BSL, den sie bereits kennen, ist sehr einfach:
; 1. Ein BSL Programm ist eine Sequenz von Ausdrücken.
; 2. Ein Ausdruck ist eine Zahl, ein Bild, ein Boolscher Wert, ein String, oder ein Funktionsaufruf.
; 3. Ein Funktionsaufruf hat die Form (f a1 a2 ...) wobei f der name einer Operation ist und a1,a2,... Ausdrücke sind.

; Diese Programme sind alle syntaktisch korrekt:
; a)  (+ 2 (* 3 4))
; b)  (number->string "asdf")
; c)  (string-length "asdf" "fdsa")
; c)  (number->string "21" "42")
; d)  (/ 1 0)
; e)  (string->number "asdf")

; Allerdings hat nicht jedes dieser Programme in BSL eine Bedeutung. _Bedeutung_ heißt in diesem
; Fall dass das Programm korrekt ausgeführt werden kann und einen Wert zurückliefert.
; Die Menge der BSL Programme, die eine Bedeutung haben, ist nur eine Teilmenge der syntaktisch
; korrekten BSL Programme.

; Die Ausführung von Programm b) ergibt einen _Laufzeitfehler_, ein Fehler der auftritt während
; das Programm läuft (im Unterschied zu Syntaxfehlern, die _vor_ der Programmausführung erkannt werden).
; Wenn in BSL ein Laufzeitfehler auftritt, wird die Programmausführung abgebrochen und eine 
; Fehlermeldung ausgegeben, in diesem Fall die Fehlermeldung:
;          number->string: expects a number; given "asdf"
;
; Dieser Fehler ist ein Beispiel für einen _Typfehler_: Die Operation erwartet, dass ein Operand
; einen bestimmten Typ hat, diesem Fall 'Zahl', aber tatsächlich hat der Operand einen anderen
; Typ, in diesem Fall 'String'.

; Ein anderer Fehler, der auftreten kann, ist der, dass die Anzahl der angegebenen Operanden nicht 
; zu der Operation passt. Im Beispiel c) tritt folgender Fehler auf:
;           string-length: expects only 1 argument, but found 2

; Manchmal stimmt zwar der Datentyp des Operanden, aber trotzdem 'passt' der Operand
; in irgendeiner Weise nicht. Im Beispiel d) ist es so, dass der Divionsoperator als Operanden
; Zahlen erwartet. Der zweite Operand ist eine Zahl, trotzdem resultiert die Ausführung in einer
; Fehlermeldung, nämlich "/: division by zero".
;
; Programmiersprachen unterscheiden sich darin, zu welchem Zeitpunkt Fehler der Kategorien a) bis d)
; gefunden werden, also zum Beispiel vor der Ausführung oder erst während der Ausführung. Im Allgemeinen
; gilt: Je früher desto besser! - allerdings muss diese zusätzliche Sicherheit häufig mit anderen
; Restriktionen erkauft werden, zum Beispiel der Restriktion dass einige korrekte Programme nicht
; mehr ausgeführt werden können.

; Eine andere Situation liegt im Fall e) vor. Die Ausführung des Programms e) ergibt den Wert "false".
; Dieser Wert signalisiert, dass der übergebene String keine Zahl repräsentiert.
; In diesem Fall tritt also _kein_ Laufzeitfehler auf, sondern die Ausführung wird fortgesetzt.
; Das Programm ist also aus BSL-Sicht wohldefiniert. Die Operation string->number hätte alternativ
; aber auch so definiert werden können, dass sie in dieser Situation einen Laufzeitfehler auslöst.


; Bedeutung von BSL Programmen
; =============================

; Fassen wir nochmal den jetzigen Stand zusammen: Programmieren ist das Aufschreiben arithmetischer Ausdrücke,
; wobei die Arithmetik Zahlen, Strings, Boolsche Werte und Bilder umfasst. Programme sind syntaktisch
; korrekt wenn Sie gemäß der Regeln aus dem vorherigen Abschnitt konstruiert wurden. Nur syntaktisch
; korrekte Programme (aber nicht alle) haben in BSL eine Bedeutung. Die Bedeutung eines Ausdrucks in BSL
; ist ein Wert, und dieser Wert wird durch folgende Auswertungsvorschriften ermittelt:
;
; 1. Ist der Ausdruck bereits ein Wert (eine Zahl, ein String, ein Bild oder ein Wahrheitswert) so ist seine Bedeutung dieser Wert.
; 2. Hat der Ausdruck die Form (f a1 a2 ...), wobei f ein Operationsname und a1, a2,... Ausdrücke sind, so werden
; von links nach rechts die Ausdrücke a1, a2,... zu Werten v1, v2,... ausgewertet. Ist die Operation f auf den 
; Werten v1, v2,... definiert, so ist der Wert des Ausdrucks die Anwendung von f auf v1, v2,... 
; Ist die Operation nicht auf v1, v2,... definiert, so wird die Auswertung mit einer passenden Fehlermeldung abgebrochen.
}