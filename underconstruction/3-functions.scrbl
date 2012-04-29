#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Hierarchische Abstraktion mit Funktionen}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 2}

@section[#:tag "letterprogram"]{Funktionale Dekomposition}
Programme bestehen nur in den seltensten Fällen aus einer einzelnen Funktion.
Typischerweise bestehen Programme aus vielen 
Funktionsdefinitionen, die teilweise dadurch voneinander abhängig sind, dass
sie sich untereinander aufrufen. Betrachten Sie das folgende Programm
zur Erstellung etwas plumper "Nigerian-Scam" Briefe:

@block[
(define (letter fst lst signature-name)
  (string-append
    (opening lst)
    "\n"
    (body fst lst)
    "\n"
    (closing signature-name)))
 
(define (opening lst)
  (string-append "Sehr geehrte(r) Herr/Frau " lst ","))
 
(define (body fst lst)
  (string-append
   "König Zabutu aus dem Tacka-Tucka-Land ist gestorben und hat Sie, "
   fst
   ", in seinem Testament als Alleinerben eingesetzt.\n"
   "Lieber " fst lst ", gegen eine kleine Bearbeitungsgebühr überweise ich das Vermögen."))
 
(define (closing signature-name)
  (string-append
   "Mit freundlichen Grüßen,"
   "\n"
   signature-name))]

Diese Definitionen können wir nun benutzen, um mit wenig Aufwand viele solcher Briefe zu erstellen:

@ex[(letter "Tillmann" "Rendel" "Klaus Ostermann")]

Das Ergebnis ist ein langer String. Das \n in dem String steht für einen Zeilenumbruch. 
Sobald dieser String zum Beispiel in eine Datei geschrieben wird, 
wird aus dem \n ein echter Zeilenumbruch.

Im Allgemeinen sollte ein Programm so gestaltet sein, daß es eine Funktion pro Aufgabe
gibt, die das Programm erledigen soll. Aufgaben sollten hierbei hierarchisch angeordnet sein:
Auf der obersten Ebene gibt es die Gesamtaufgabe des Programms (wie das Anfertigen
eines Serienbriefs); diese große Aufgabe wird in kleinere Aufgaben wie das Anfertigen
der Eröffnung eines Serienbriefs zerlegt, diese können ggf. wieder in weitere kleine
Aufgaben zerlegt werden.

Die Struktur der Funktionen sollte der hierarchischen Struktur der Aufgaben folgen:
Zu jeder Aufgabe gibt es eine Funktion die in ihrer Implementierung die Funktionen
aufruft, die zu den Unteraufgaben korrespondieren. Die Funktionen können dementsprechend
als Baum (oder azyklischer Graph) angeordnet werden, an dessen Wurzel die Hauptfunktion steht 
und an dessen Blättern
die Funktionen der untersten Ebene (die nur primitive Funktionen aufrufen) stehen.

Ein Programm, in dem jede Funktion eine klare Aufgabe hat, ist leicht zu verstehen und leichter
zu warten, denn wenn es Änderungswünsche gibt, so betreffen diese typischerweise eine
bestimmte Aufgabe des Systems. Ist die Implementation dieser Aufgabe in Form einer Funktionsdefinition 
lokalisiert, so muss nur diese eine Funktion modifiziert werden.

Wenn man eine größere Menge von Funktionen zu programmieren hat, stellt sich die Frage, in welcher 
Reihenfolge man diese Funktionen programmiert. Zwei wichtige Varianten sind "Top-Down" 
und "Bottom-Up". "Top-Down" bedeutet, dass man mit der Hauptfunktion anfängt, dann alle Funktionen
programmiert die in der Hauptfunktion aufgerufen werden, dann alle Funktionen die widerum von
diesen Funktionen aufgerufen werden, und so weiter. "Bottom-Up" ist genau umgekehrt: Man
programmiert zunächst die einfachsten Funktionen die nur primitive Funktionen aufrufen, dann
die Funktionen, die die gerade programmierten Funktionen aufrufen, und so weiter bis man ganz
zum Schluss die Hauptfunktion programmiert. Abzuschätzen welche Vorgehensweise in welcher 
Situation die richtige ist ein wichtiges Thema in der Methodik der Softwarekonstruktion.

Wenn die Aufrufstruktur der Funktionen ein azyklischer Graph ist, so ergibt sich automatisch
eine Schichtenstruktur, in der alle Funktionen auf einer Schicht nur Funktionen aus darunter liegenden
Schichten aufrufen. Eine noch stärkere Einschränkung ist die, dass Funktionen ausschliesslich
Funktionen aus der Schicht direkt unter Ihnen aufrufen dürfen. Diese Einschränkung ist allerdings
sinnvoll, denn sie ermöglicht bei geschickter Wahl der Funktionen, eine Schicht zu verstehen ohne 
alle darunter liegenden Schichten verstehen zu müssen. Diese Situation nennt man 
@italic{hierarchische Abstraktion}, und sie ist der Schlüssel, um mit Komplexität großer Softwaresysteme
umzugehen. 

Die Grundidee bei hierarchischer Abstraktion durch Funktionen ist die, dass man Funktionen so gestalten sollte, dass ein Programmierer
anhand des Namens und der Dokumentation (der @italic{Spezifikation}) einer Funktion in der Lage ist, eine Funktion effektiv 
in einem Programm zu verwenden --- es also nicht notwendig ist, den Programmtext der Funktion und 
(transitiv) aller Funktionen, die diese Funktion aufruft, zu verstehen. Später werden wir sehen,
dass die Spezifikation einer Funktion zumindest partiell in Form von maschinell überprüfbaren Verträgen (@italic{contracts})
formalisiert werden kann. Für informelle Spezifikationen werden meist Kommentare verwendet, die
oberhalb der Funktionsdefinition stehen.

In unserem Beispiel oben ist es so, dass man nicht die Details der @racket[opening] Funktion verstehen
muss, um die @racket[letter] Funktion zu verstehen; es reicht, zu wissen, dass diese Funktion die 
Eröffnung des Briefes zurückgibt --- wie auch immer die Details davon aussehen werden. In diesem Fall
ist der Name der Funktion ausreichend, um @racket[opening] effektiv benutzen zu können;
in anderen Fällen ist weitere Dokumentation erforderlich. In jedem Fall sollten Sie jedoch versuchen,
Funktionsnamen so zu wählen, dass möglichst wenig weitere Dokumentation erforderlich ist, denn
wenn sich ein Programm weiterentwickelt, passiert es häufig, dass nur der Programmtext aber
nicht die Kommentare "gewartet" werden und dementsprechend Dokumentation häufig obsolet ist.

@section{Vom Problem zum Programm}

Betrachten Sie folgende Problemstellung für ein zu erstellendes Programm:

@italic{The owner of a movie theater has complete freedom in setting ticket prices. 
The more he charges, the fewer the people who can afford tickets. In a recent experiment the owner determined 
a precise relationship between the price of a ticket and average attendance. At a price of $5.00 per ticket, 
120 people attend a performance. Decreasing the price by a dime ($.10) increases attendance by 15. Unfortunately, 
the increased attendance also comes at an increased cost. Every performance costs the owner $180. Each attendee
costs another four cents ($0.04). The owner would like to know the exact relationship between profit and ticket 
price so that he can determine the price at which he can make the highest profit.}

Die Aufgabe ist relativ klar, aber wie man daraus ein Programm macht nicht.
Eine gute Art, diese Aufgabe anzugehen, ist es, die Quantitäten und 
ihre Abhängigkeiten voneinander zu betrachten und nacheinander 
in Form einer Funktion zu definieren:

Die Problemstellung sagt, wie die Anzahl der Zuschauer vom Eintrittspreis abhängt.
Dies ist eine klar definierte Unteraufgabe, daher verdient sie eine eigene Funktion:

@racketblock[
(define (attendees ticket-price)
  (+ 120 (* (/ 15 0.1) (- 5.0 ticket-price))))]

Der Umsatz hängt vom Verkauf der Eintrittskarten ab: Es ist das Produkt 
aus Eintrittspreis und Anzahl der Zuschauer.

@racketblock[
(define (revenue ticket-price)
  (*  (attendees ticket-price) ticket-price))]

Die Kosten setzen sich aus zwei Teilen zusammen: Einem festen Anteil ($180)
und einem variablen Teil, der von der Anzahl der Zuschauer abhängt.
Da die Zahl der Zuschauer wiederum vom Eintrittspreis abhängt, muss
diese Funktion auch den Ticketpreis als Eingabeparameter entgegennehmen
und verwendet die bereits definierte @racket[attendees] Funktion:

@racketblock[    
(define (cost ticket-price)
   (+ 180 (* 0.04 (attendees ticket-price))))]

Der Gewinn ist schliesslich die Differenz zwischen Umsatz und Kosten.
Da wir bereits Funktionen für die Berechnung von Umsatz und Kosten haben,
muss diese Funktion all die Werte als Eingabe bekommen, die diese Funktionen
benötigen --- in diesem Fall ist dies der Eintrittspreis:
@margin-note{Dieses Programm enthält diverse @italic{magic numbers}. Eliminieren Sie
             diese durch entsprechende Variablendefinitionen!}

@racketblock[    
(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))]

Diese Funktionen können wir nun verwenden, um den Gewinn bei einem bestimmten Eintrittspreis zu berechnen.
Probieren Sie aus, bei welchem Preis der Gewinn maximiert wird!

Hier ist eine alternative Version des gleichen Programms, welches nur aus einer einzigen Funktion besteht:

@racketblock[
(define (profit price)
 (- (* (+ 120
         (* (/ 15 0.1)
            (- 5.0 price)))
            price)
    (+ 180
       (* 0.04
          (+ 120
             (* (/ 15 0.1)
                (- 5.0 price)))))))]

Überprüfen Sie, dass diese Definition tatsächlich die gleichen Ergebnisse wie das Programm oben produziert.
Wir benötigen also prinzipiell nur eine Funktion für dieses Programm; dennoch ist es offensichtlich, dass
die erste Version oben deutlich lesbarer ist. Sie ist auch besser wartbar: Überlegen Sie sich 
beispielsweise, welche Änderungen im Programmtext erforderlich sind, wenn die festen Kosten entfallen und stattdessen
pro Zuschauer Kosten in Höhe von $1,50 anfallen. Probieren Sie in beiden Versionen, diese Änderung zu implementieren.
Vergleichen Sie.

@section{Systematischer Entwurf von Programmen}

Das meiste, was sie bisher über das Programmieren gelernt haben, dreht sich darum, wie die Programmiersprache, die sie verwenden, funktioniert
und was ihre Konstrukte bedeutet. Wir haben einige wichtige Sprachkonstrukte kennengelernt (Funktionsdefinitionen, Variablendefinitionen, Ausdrücke)
und etwas Erfahrung damit gesammelt, wie man diese Konstrukte einsetzen kann.

Diese Kenntnisse sind jedoch nicht ausreichend, um systematisch aus einer Problembeschreibung ein Programm zu konstruieren. Hierzu müssen
wir lernen, was in einer Problembeschreibung relevant ist und was nicht. Wir müssen verstehen, welche Daten das Programm konsumiert und
welche es abhängig von den Eingabedaten produzieren muss. Wir müssen herausfinden, ob eine benötigte Funktion in einer
Bibliothek vielleicht schon vorhanden ist oder ob wir selber diese Funktion programmieren müssen. Wenn wir ein Programm haben, müssen wir
sicherstellen, dass es sich tatsächlich wie gewünscht verhält. Hierbei können allerlei Fehler auftreten, die wir verstehen
und beheben müssen.

Gute Programme haben eine kurze Beschreibung dessen, was sie tun, welche Eingabe sie erwarten, und was für eine Ausgabe sie produzieren.
Am besten wird gleichzeitig dokumentiert, dass das Programm tatsächlich funktioniert. Nebenbei sollten Programme auch noch so strukturiert
sein, dass eine kleine Änderung in der Problembeschreibung auch nur eine kleine Änderung am Programm bedeutet.

Diese ganze Arbeit ist notwendig, weil Programmierer nur selten für sich selbst Programme schreiben. 
@margin-note{Das Wort @italic{Softwareentwickler} ist übrigens im Wesentlichen nur ein Modewort für @italic{Programmierer}. Ersetzen Sie daher in Ihrem Lebenslauf 
alle Vorkommen von "Programm" mit "Software" und "programmieren" durch "entwickeln" :-)}
Programmierer schreiben Programme, die andere Programmierer verstehen und weiterentwickeln müssen.
Große Programme werden über lange Zeiträume von großen Teams entwickelt. Neue Programmierer stoßen
während dieses Zeitraums hinzu, andere gehen. Kunden ändern ständig ihre Anforderungen an das
Programm. Große Programme enthalten fast immer Fehler, und oft sind diejenigen, die den Fehler beheben müssen
nicht identisch mit denen, die die Fehler eingebaut haben.

Ein reales, im Einsatz befindliches Programm ist zudem niemals "fertig". Es muss in den meisten Fällen ständig
weiterentwickelt oder zumindest gewartet oder an neue Technik angepasst werden. Es ist keine Seltenheit, dass
Programme eine Lebenszeit von vielen Jahrzehnten haben und daher die ursprünglichen Programmierer eines Programms, 
das weiterentwickelt oder gewartet werden soll, vielleicht schon im Ruhestand oder verstorben sind.
@margin-note{Recherchieren Sie, was das "Jahr 2000 Problem" ist.}
Die gleichen Probleme treten übrigens selbst dann auf, wenn es nur einen einzigen Programmierer gibt, denn
auch dieser vergißt nach einiger Zeit, was er vor sich einmal bei einem Programm gedacht hat, und profitiert
dann genau so von einem sinnvollen Entwurf als wenn ein anderer Programmierer seinen Code lesen würde.

Aus diesen Gründen werden wir Ihnen systematische Anleitungen an die Hand geben, mit denen unterschiedliche Entwurfsaufgaben
Schritt für Schritt gelöst werden können. Diese Anleitungen nennen wir @italic{Entwurfsrezepte}.

@subsection{Testen}
Ein wichtiger Bestandteil der vorgestellten Methodik wird es sein, Programme systematisch und automatisiert zu @italic{testen}.
Beim Testen führt man ein Programm oder einen Programmteil (wie eine Funktion) mit Testdaten aus und überprüft, ob das
Resultat das ist, welches man erwartet. Natürlich kann man "von Hand" testen, indem man zum Beispiel im Interaktionsbereich
Ausdrücke auswertet und die Ergebnisse überprüft. Allerdings wird es schnell langweilig, wenn man die gleichen Tests 
immer wieder aufschreibt, um zu überprüfen, ob man bei einer Programmänderung nichts "kaputt" gemacht hat.

Wann, wo, wieso und wieviel man testen sollte werden wir später diskutieren. Hier beschreiben wir nur, @italic{wie} man in DrRacket
automatisiert testet. Hierzu gibt es eine spezielle Funktion in BSL, @racket[check-expect]. Diese erwartet zwei Parameter,
von denen der erste ein Ausdruck ist, der getestet werden soll, und der zweite ein Ausdruck, der das gewünschte Ergebnis beschreibt.
Beispiel: @racket[(check-expect (+ 2 3) 5)] überprüft, ob das Ergebnis der Auswertung von @racket[(+ 2 3)] den Wert @racket[5] ergibt.

Hier ist ein Beispiel wie @racket[check-expect] verwendet werden kann, um eine Funktion zur Konvertierung zwischen Fahrenheit und Grad Celsius zu testen:

@racketblock[
(check-expect (f2c -40) -40)
(check-expect (f2c 32) 0)
(check-expect (f2c 212) 100)
 
(define (f2c f)
  (* 5/9 (- f 32)))]

Alle @racket[check-expect] Tests werden jedesmal ausgeführt, wenn auf den "Start" Knopf gedrückt wird. Falls alle Tests erfolgreich waren, wird
dies durch eine kurze Meldung quittiert. Falls mindestens ein Test fehlschlägt, wird dies durch eine Fehlermeldung mit genaueren Informationen 
dazu angezeigt.

Es gibt einige Varianten von @racket[check-expect], wie zum Beispiel @racket[check-within] und @racket[check-range]. Verschaffen Sie sich mit
Hilfe der Dokumentation einen Überblick.

Um den Programmierer zu unterstützen, zeigt DrRacket durch Färbung des Codes an, welche Teile ihres Programms bei der Ausführung der Tests
durchlaufen wurden. Probieren Sie dieses Verhalten selbst aus!

Selbstverständlich funktionieren Tests nicht nur mit Zahlen sondern mit allen Datentypen wie beispielsweise auch Bildern.
Beispielsweise können wir diese Funktion
@block[
(define (ring innerradius outerradius color)
  (overlay (circle innerradius "solid" "white")
           (circle outerradius "solid" color)))]

so testen:

@racketblock[
(check-expect (ring 5 10 "red") (unsyntax @ev[(ring 5 10 "red")]))]

Da @racket[check-expect] beliebige Ausdrücke als Parameter erwartet, können statt dem
Ergebnis selber auch auch @italic{Eigenschaften} von Ergebnissen überprüft werden, zum Beispiel so:

@racketblock[(check-expect (image-width (ring 5 10 "red")) 20)]

Wir werden später sehen, dass dies wichtig ist, um Tests nicht zu stark an die Implementation einer Funktion zu koppeln.

@subsection{Informationen und Daten}

Ein Programm beschreibt eine Berechnung, die @italic{Informationen} aus der Domäne des Programms verarbeitet und produziert.
Eine Information ist zum Beispiel so etwas wie "das Auto ist 5m lang" oder "der Name des Angestellten ist 'Müller' ".

Ein Programm kann jedoch solche Informationen nicht direkt verarbeiten. Wir müssen stattdessen Informationen als @italic{Daten} repräsentieren.
Diese Daten können wiederum als Information @italic{interpretiert} werden. Beispielsweise könnten wir die erste Information oben 
als Zahl mit dem Wert @racket[5] und die zweite Information als String mit dem Wert @racket["Müller"] repräsentieren.

Eine Datum wie die Zahl @racket[5] wiederum kann auf vielfältige Weise interpretiert werden. Beispielsweise können, wie im
Beispiel oben, die Länge eines Autos in Metern gemeint sein. Genau so kann sie aber als Temperatur in Grad Celsius, als
Geldbetrag in Euro, oder als Endnote Ihrer Klausur in dieser Veranstaltung interpretiert werden (hoffentlich nicht :-).

Da diese Beziehung zwischen Information und Daten so wichtig ist, werden wir Sie von nun an in Form spezieller Kommentare, die wir @italic{Datendefinitionen} 
nennen, aufschreiben. Eine Datendefinition beschreibt eine Klasse von Daten durch einen sinnvollen Namen, der auf die Interpretation der Daten hinweist.

Hier sind einige Beispiele für Datendefinitionen:

@#reader scribble/comment-reader
(racketblock
  ; Distance is a Number.
  ; interp. the number of pixels from the top margin of a canvas

  ; Speed is a Number.
  ; interp. the number of pixels moved per clock tick

  ; Temperature is a Number.
  ; interp. degrees Fahrenheit

  ; Length is a Number.
  ; interp. the length in centimeters

  ; Count is a Number.
  ; interp. the number of characters in a string. 
  ...
  )

Zum jetzigen Zeitpunkt kennen Sie nur einige wenige Formen von Daten (Zahlen, Strings, Bilder, Wahrheitswerte), daher
müssen Sie alle Informationen mit diesen Datentypen repräsentieren. Später werden wir andere Datentypen kennenlernen,
in denen es deutlich anspruchsvoller wird, eine geeignete Repräsentation für seine Informationen zu wählen.

@subsection[#:tag "entwurfsrezept"]{Entwurfsrezept zur Funktionsdefinition}

Auf Basis der gerade besprochenen Trennung zwischen Informationen und Daten können wir nun den Entwurf einzelner Funktionen
als eine Abfolge von Schritten beschreiben.

@itemlist[#:style 'ordered
          @item{Schreiben Sie eine Signatur, eine Aufgabenbeschreibung, und einen Funktionskopf.
                Eine @italic{Signatur} ist ein BSL Kommentar, der dem Leser sagt, wieviele und welchr Eingaben die Funktion konsumiert
                und was für eine Ausgabe sie produziert. Hier sind zwei Beispiele:
                @itemlist[
                          @item{Für eine Funktion, die einen String konsumiert und eine Zahl produziert:
                            @para{@italic{; String -> Number}  }}
                          @item{Für eine Funktion die eine Temperatur und einen Boolschen Wert konsumiert und einen String produziert:
                            @para{@italic{; Temperature Boolean -> String}}
                            Beachten Sie dass wir die vorherige Datendefinition für @italic{Temperature} verwendet haben.}]
                
                Eine @italic{Aufgabenbeschreibung} ist ein BSL Kommentar der den Zweck der Funktion @bold{in einer Zeile} zusammenfasst.
                Die Aufgabenbeschreibung ist die kürzestmögliche Antwort auf die Frage: @italic{Was berechnet die Funktion?}
                Jeder Leser ihres Programms sollte verstehen, was eine Funktion berechnet ohne die Funktionsdefinition selbst lesen zu müssen.
                
                Ein @italic{Funktionskopf}, manchmal auch @italic{Header} oder @italic{Stub} genannt, ist eine Funktionsdefinition,
                die zur Signatur passt aber in der der Body der Funktion nur ein Dummy-Wert ist, zum Beispiel @racket[0] falls eine Zahl
                zurückgegeben werden soll oder @racket[(empty-scene 100 100)] falls ein Bild zurückgegeben werden soll. Beim Entwurf des
                Funktionskopfs müssen trotzdem wichtige Entscheidungen getroffen werden, nämlich die Namen der Funktion und der Eingabeparameter
                müssen bestimmt werden. Typischerweise sollten die Parameternamen einen Hinweis darauf geben, was für Informationen
                oder welchen Zweck die Parameter repräsentieren. Die Namen der Parameter können oft sinnvoll in der Aufgabenbeschreibung verwendet werden.
                
                Hier ein vollständiges Beispiel für eine Funktionsdefinition nach diesem Schritt:
                
@#reader scribble/comment-reader
(racketblock
; Number String Image -> Image
; add s to img, y pixels from top, 10 pixels to the left
(define (add-image y s img)
  (empty-scene 100 100))
  )
          Zu diesem Zeitpunkt können Sie bereits auf den "Start" Knopf drücken und die Funktion benutzen --- allerdings wird natürlich stets nur der Dummy-Wert
          und nicht das gewünschte Ergebnis zurückgegeben.}
          
          @item{Schreiben Sie zwischen Aufgabenbeschreibung und Funktionskopf @italic{Tests}, die anhand von Beispielen dokumentieren, was die Funktion macht. Diese Tests sind einerseits Teil der Dokumentation der Funktion,
                              auf der anderen Seite werden diese Tests automatisiert ausgeführt und schützen sie damit vor Fehlern im Funktionsbody.
                              Hier ist ein Beispiel, wie eine Funktion nach diesem Schritt aussieht:
@#reader scribble/comment-reader
(racketblock
; Number -> Number
; compute the area of a square whose side is len
(check-expect (area-of-square 2) 4)
(check-expect (area-of-square 7) 49)

(define (area-of-square len) 0)
)
                              
                              }
          @item{Im dritten Schritt überlegen Sie sich, welche der Ihnen zur Verfügung stehenden Eingabedaten und ggf. Hilfsfunktionen und Variablen Sie zur Berechnung benötigen.
                Sie ersetzen den Dummy-Wert aus dem zweiten Schritt mit einem @italic{Template} (Schablone), indem die Eingabedaten/Funktionen/Variablen von oben vorkommen.
                Im Moment sieht dieses Template so aus, dass einfach die Eingabedaten/Funktionen/Variablen durch @racket[...] voneinander getrennt unsortiert im 
                Funktionsbody stehen. Später werden wir interessantere Templates kennenlernen.
                
                In unserem letzten Beispiel könnte die Funktion nach diesem Schritt beispielsweise so aussehen:
                
@#reader scribble/comment-reader
(racketblock
; Number -> Number
; compute the area of a square whose side is len
(check-expect (area-of-square 2) 4)
(check-expect (area-of-square 7) 49)

(define (area-of-square len) ... len ...)
)
                }
          @item{Jetzt ist es an der Zeit, den Funktionsbody zu implementieren, also das Template nach und nach durch einen Ausdruck zu ersetzen, der die Spezifikation (Signatur, Aufgabenbeschreibung, Tests)
                erfüllt. Unsere @racket[area-of-square] Funktion könnte nun so aussehen:
                
@#reader scribble/comment-reader
(racketblock
; Number -> Number
; compute the area of a square whose side is len
(check-expect (area-of-square 2) 4)
(check-expect (area-of-square 7) 49)

(define (area-of-square len) (* len len))
)
            Die @racket[add-image] Funktion könnte nach diesem Schritt so aussehen:

@#reader scribble/comment-reader
(racketblock
; Number String Image -> Image
; add s to img, y pixels from top, 10 pixels to the left
(check-expect (add-image 5 "hello" (empty-scene 100 100)) 
              (place-image (text "hello" 10 "red") 10 5 (empty-scene 100 100)))
(define (add-image y s img)
  (place-image (text s 10 "red") 10 y img))
)
Es ist wichtig, zu verstehen, dass der Test @italic{nicht} aussagt, dass @racket[add-image] mit Hilfe von @racket[place-image]
implementiert wurde. Die @racket[check-expect] Funktion vergleicht die Bilder, die aus den Audrücken entstehen, und nicht
die Ausdrücke selber. Beispielsweise könnten wir den zweiten Parameter auch durch ein Bildliteral (also ein Bild im Programmtext) ersetzen.
Daher "verrät" der Test nicht die Implementation der Funktion. Dies ist wichtig, weil wir damit die Möglichkeit haben, 
die Implementation der Funktion zu verändern, ohne dass Klienten der Funktion (also Aufrufer der Funktion) davon betroffen werden.
Dieses Konzept nennt man auch @italic{Information Hiding}.

                }
         @item{Der letzte Schritt ist es, die Funktion zu testen. Da unsere Tests automatisiert sind, genügt hierzu ein Klick auf "Start". Falls
               ein Test fehlschlägt, ist entweder der Test falsch, oder die Funktionsdefinition enthält einen Fehler (oder beides gleichzeitig).
               Als erstes sollten Sie in diesem Fall überprüfen, ob das beobachtete Verhalten wirklich fehlerhaft war oder nur ihr Test
               fehlerhaft ist. Reparieren Sie, je nachdem, den Test beziehungsweise die Funktionsdefinition, bis der Test fehlerfrei ausgeführt wird.}
                 
                 ]

@subsection{Programme mit vielen Funktionen}
Die meisten Programme bestehen nicht aus einer sondern aus vielen Funktionen. Diese Funktionen
sind häufig voneinander abhängig, da eine Funktion eine andere Funktion aufrufen kann.

Sie haben oben ein Entwurfsrezept für den Entwurf einzelner Funktionen gesehen. Dieses sollten beim
Entwurf jeder einzelnen Funktion verwenden. Wenn Sie viele Funktionen und globale Konstanten (Variablen)
definiert haben, so sollten Sie im Funktionstemplate die Funktionen und Konstanten aufführen, von
denen Sie glauben, dass sie im endgültigen Funktionsbody benötigt werden.

Da sie nicht alle Funktionen auf einmal programmieren können, stellt sich die Frage, in welcher Reihenfolge
sie vorgehen. Ein häufiger Ansatz ist der @italic{Top-Down Entwurf}, bei dem man zuerst die Hauptfunktion(en)
der Anwendung programmiert. Diese Funktionen werden zweckmäßigerweise in weitere Hilfsfunktionen zerlegt, die
während des Programmierens der Funktion noch gar nicht existieren. Deshalb bietet es sich an, stets eine
"Wunschliste" der Funktionen, die noch programmiert werden müssen, zu führen. Ein Eintrag auf der Wunschliste
besteht aus einem sinnvollen Funktionsnamen, einer Signatur und einer Aufgabenbeschreibung. Am Anfang steht auf dieser Liste
nur die Hauptfunktion. Stellen Sie beim Programmieren einer Funktion fest, dass Sie eine neue Funktion benötigen, fügen
Sie der Wunschliste einen entsprechenden Eintrag hinzu. Sind sie mit einer Funktion fertig, suchen Sie sich die nächste 
Funktion von der Liste, die
sie implementieren möchten.@margin-note{Recherchieren Sie was die Abkürzungen FIFO und LIFO bedeuten. Diskutieren
Sie, ob FIFO oder LIFO für die Wunschliste geeignet sind und was für Konsequenzen dies hat.}
Ist die Liste leer, sind sie fertig.

Ein Vorteil von Top-Down Entwurf ist, dass Sie Schritt für Schritt ihr großes Entwurfsproblem in immer kleinere
Probleme zerlegen können, bis die Probleme so klein werden, dass Sie sie direkt lösen können (im Fall von
Funktionsdefinitionen sind dies Funktionen, die nur noch eingebaute Funktionen oder Bibliotheksfunktionen verwenden).

Ein wichtiger Nachteil ist, dass Sie erst relativ spät die Details der Hilfsfunktionen programmieren. Falls Sie 
einen Denkfehler gemacht haben und die Hilfsfunktionen so gar nicht implementiert werden können, müssen Sie unter
Umständen einen großen Teil ihrer Arbeit wieder in den virtuellen Papierkorb werfen. Ein anderer wichtiger Nachteil
ist der, dass Sie erst sehr spät ihre Funktionen testen können, nämlich erst wenn alle Hilfsfunktionen vollständig implementiert
wurden. Eine Möglichkeit, dieses Problem zu umgehen, ist, eine Hilfsfunktion erstmal durch einen @italic{Test Stub}
zu ersetzen. Ein Test Stub ist eine Dummy-Funktionsdefinition, die eine vordefinierte Antwort zurückliefert, wie sie im Kontext
eines Tests erwartet wird. Nehmen Sie beispielsweise an, sie möchten eine @racket[area-of-cube] Funktion definieren, die
die eine noch nicht programmierte @racket[area-of-square] Funktione benutzt. Um @racket[area-of-cube] trotzdem testen
zu können, können Sie @racket[area-of-square] zunächst provisorisch durch einen Test Stub wie in diesem Beispiel zu implementieren.
Wenn Sie sich später dafür entscheiden, diese Funktion zu implementieren, ersetzen Sie den Test Stub durch die 
richtige Funktionsdefinition.

@#reader scribble/comment-reader
(racketblock
; Number -> Number
; computes the area of a cube with side length len
(check-expect (area-of-cube 3) 54)
(define (area-of-cube len) (* 6 (area-of-square len)))

; Number -> Number
; computes the area of a square with side length len
(check-expect (area-of-square 3) 9)
(define (area-of-square len) (if (= len 3) 9 (error "not yet implemented")))
)

Der Test Stub für @racket[area-of-square] benutzt die Funktion @racket[error]. Diese ist gut dafür geeignet, zu dokumentieren,
dass eine Funktion noch nicht fertig implementiert wurde. Dies ist insbesondere besser, als stillschweigend ein falsches Ergebnis
zurückzuliefern, denn dann fällt ihnen unter Umständen erst sehr spät auf, dass Sie diesen Teil noch implementieren müssen.

@section{Information Hiding}
Der Name, die Signatur, die Aufgabenbeschreibung und die Tests bilden zusammen die @italic{Spezifikation}
einer Funktion. Die Spezifikation sollte ausreichend viele Informationen enthalten, um die Funktion
benutzen zu können --- ein Aufrufer sollte also nicht erst die Implementation der Funktion (und vielleicht
sogar rekursiv die Implementationen aller Funktionen die darin aufgerufen werden) studieren müssen, um 
die Funktion nutzen zu können.

Eines der wichtigsten Prinzipien, um in der Programmierung mit der Komplexität großer Programme umzugehen, heißt
@italic{Information Hiding}, im Deutschen manchmal auch @italic{Geheimnisprinzip} genannt. In Bezug auf Funktionen
sagt dieses Prinzip aus, dass ein Programm besser lesbar, verstehbar und wartbar ist, wenn alle Aufrufer von
Funktionen sich nur auf die Spezifikationen der Funktion verlassen, aber nicht von Implementierungsdetails abhängen.
Ferner sagt dieses Prinzip aus, dass es einen Unterschied zwischen Spezifikation und Implementation geben sollte, und
zwar dergestalt, dass es viele mögliche Implementationen der gleichen Spezifikation gibt. Wenn sich alle an diese
Regel halten, so ist garantiert, dass man die Implementation jeder Funktion beliebig modifizieren kann --- solange
die Spezifikation weiterhin eingehalten wird, ist durch dieses Prinzip sichergestellt, dass das Programm weiterhin funktioniert.


Betrachten Sie als Beispiel die @racket[body] Funktion aus dem Spam-Mail-Generator von oben.
Hier ist die Definition mit einer möglichen Spezifikation:

@#reader scribble/comment-reader
(racketblock
; String String -> String

; generiert the pretense for money transfer for the victim fst last

(check-range (string-length (body "Tillman" "Rendel")) 50 300)

(define (body fst lst)
  (string-append
   "König Zabutu aus dem Tacka-Tucka-Land ist gestorben und hat Sie, "
   fst
   ", in seinem Testament als Alleinerben eingesetzt.\n"
   "Lieber " fst " " lst ", gegen eine kleine Bearbeitungsgebühr überweise ich das Vermögen."))
)

Ein Beispiel für einen Aufrufer, der sich nicht an die Spezifikation hält und unzulässig an 
die Implementation der koppelt wäre einer, der Folgetext für den Brief definiert, der sich
auf Details des Textes wie Namen und Orte bezieht, die nicht in der Spezifikation gennant werden.

Halten sich jedoch alle Aufrufer an das Geheimnisprinzip, so ist sichergestellt, dass sich
die Implementation von @racket[body] weitgehend ändern läßt, solange es ein plausibler Text
gemäß der Aufgabenbeschreibung ist, der zwischen 50 und 300 Zeichen lang ist.

Dieses Beispiel illustriert weiterhin, wieso es häufig sinnvoll ist, in Tests nur
bestimmte Eigenschaften des Ergebnisses zu testen, aber nicht das Ergebnis exakt vorzuschreiben.
Im Beispiel wird nur getestet, dass die Länge des generierten Strings zwischen 50 und 300 ist --- 
dies wird damit zum Teil der Spezifikation, auf die sich Aufrufer verlassen können. 
Würde hingegen auf einen festen Ausgabestring getestet, so würde der Test zu viele Details über
die Implementierung verraten und damit das Information Hiding Prinzip nutzlos gemacht.

