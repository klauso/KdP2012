;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-intro) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
;  Konzepte der Programmiersprachen, SS 2012, Prof. Dr. Klaus Ostermann, Tillmann Rendel
;  -------------------------------------------------------------------------------------
;  
;  Das Skript für diese Vorlesung wird als Menge von Racket Quelltexten organisiert. 
;  Öffnen Sie diese zum Lesen mit DrRacket.
; ------------------------------------------------------------------------------------------------
;  Die Skripte dienen zur Übersicht und Erinnerung an das durchgenommene Material.
;  Als alleiniges Lehrmaterial sind sie nicht geeignet. Zu diesem Zweck wird es bei jedem 
;  Skript Verweise auf Lehrmaterial geben, welches die Themen ausführlicher darstellt.
;  
;  Häufig werden wir auf das Buch "How to Design Programs" [HTDP]. Eine neuere Version des Buches,
;  HtDP/2e, ist partiell bereits öffentlich verfügbar [HTDP/2e].
;  
;  [HTDP] Matthias Felleisen et al, How To Design Programs, MIT Press, 2003
;         Online Version: http://htdp.org
;  [HTDP/2e] Matthias Felleisen et al, How To Design Programs, 2nd edition. 
;         Partielle Online Version: http://www.ccs.neu.edu/home/matthias/HtDP2e/
;         
; In folgenden Skripten werden wir auf diese Quellen nur noch als [HTDP] bzw. [HTDP/2e] bezeichnen.        
; --------------------------------------------------------------------------------------------------
  
  
; Dieses Skript basiert auf [HTDP/2e] Kapitel 1

; Programmieren mit arithmetischen Ausdrücken
; ===========================================
 
; Jeder von Ihnen weiß, wie man Zahlen addiert, dividiert oder multipliziert, denn Sie wurden
; von den Lehrern mit einem Verfahren (einem sog. _Algorithmus_) dazu "programmiert". 
; In diesem Kurs werden wir die Rollen umdrehen: Sie werden programmieren, und der Computer 
; wird ihre Anweisungen ausführen.  Die Sprache in der wir diese Anweisungen formulieren heißt
; _Programmiersprache_. Die Programmiersprache, die wir zunächst verwenden werden, heißt
; _BSL_. BSL steht für "Beginning Student Language".

; Viele einfache Algorithmen sind in einer Programmiersprache bereits vorgegeben, z.B. solche
; zur Arithmetik mit Zahlen. Wir können "Aufgaben" stellen, indem wir DrRacket eine Frage stellen,
; auf die uns DrRacket dann im Ausgabefenster die Antwort gibt. So können wir zum Beispiel die Frage

(+ 1 1)

; stellen - als Antwort erhalten wir bei Ausführung dieser Anweisung ("Start" Knopf) 2.
; Diese Art von Fragen nennen wir _Ausdrücke_.

; Hier einige weitere Beispiele für Ausdrücke mit weiteren arithmetischen Operationen.
                                        
(+ 2 2)
(* 3 3)
(- 4 2)
(/ 6 2)
(sqr 3)
(expt 2 3)
(sin 0)
(cos pi)  ; das "i" im Ergebnis #i-1.0 steht für "inexact", also ungenau - im Unterschied zur
          ; Mathematik sind manche Berechnungen auf einem Computer notwendigerweise nur Annäherungen
          ; an das mathematisch korrekte Ergebnis
                                         
; Der Bereich, in dem Sie diesen Text lesen, ist der _Definitionsbereich_. In diesem Bereich 
; schreiben und editieren Sie ihre Programme. Sobald Sie hier etwas ändern, taucht der "Speichern" Knopf
; auf, mit dem Sie die Definitionen abspeichern können.

; Programme beinhalten Ausdrücke. Alle Programme, die wir bisher gesehen haben, _sind_ Ausdrücke.
; Jeder von Ihnen kennt Ausdrücke aus der Mathematik. Zu diesem Zeitpunkt ist ein Ausdruck in unserer 
; Programmiersprache ist entweder eine Zahl, oder etwas das mit einer linken Klammer "(" startet und mit
; einer rechten Klammer ")" endet. Wir bezeichnen Zahlen als _primitive Ausdrücke_. 
; Später werden andere Arten von Ausdrücken hinzukommen.

; Wenn Sie auf "Start" drücken, wertet DrRacket die Ausdrücke von oben nach unten aus und zeigt die 
; Ergebnisse im _Interaktionsbereich_ (der Bereich unter dem Definitionsbereich). Sie können auch 
; direkt im Interaktionsbereich Ausdrücke eingeben, die dann sofort ausgewertet werden. Allerdings 
; werden die Ausdrücke im Interaktionsbereich nicht durch den "Speichern" Knopf mit abgespeichert.

; Wie kann man mehr als zwei Zahlen addieren? Hierzu gibt es zwei Möglichkeiten:

; Durch Schachtelung:

(+ 2 (+ 3 4))

; oder durch Addition mit mehr als zwei Operanden

(+ 2 3 4)

; Immer wenn Sie in BSL eine arithmetische Operation wie "+" oder "sqrt" benutzen möchten,
; schreiben Sie eine öffnende Klammer, gefolgt von der Operation, dann einem Lehrzeichen 
; (oder Zeilenumbruch) und dann die _Operanden_, also in unserem Fall die Zahlen auf die die
; Operation angewandt werden soll.
;
; Am Beispiel der Schachtelung haben Sie gesehen, dass auch Ausdrücke als Operanden zugelassen sind.
; Diese Schachtelung kann beliebig tief sein:

(+ (* 5 5) (+ (* 3 (/ 12 4)) 4)) ; ergibt 38

; Solche geschachtelten Ausdrücke werden so ausgewertet, wie Sie es auch auf einem Blatt Papier 
; machen würden: Wenn ein Operand ein nicht-primitiver Ausdruck ist, so wird zunächst dieser Ausdruck berechnet. 
; Dieser Unterausdruck ist möglicherweise selber wieder geschachtelt; in diesem Fall wird diese
; Berechnungsvorschrift auch auf diese Unterausdrücke wieder angewendet (sog. _rekursive_ Anwendung).
; Falls mehrer Operanden nicht-primitive Ausdrücke sind, so wird von links nach rechts ausgewertet.

; Zusammengefasst ist Programmieren zu diesem Zeitpunkt das Schreiben von arithmetischen Ausdrücken.
; Ein Programm auszuführen bedeutet den Wert der darin enthaltenen Ausdrücke zu berechnen.
; Ein Drücken auf "Start" bewirkt die Ausführung des Programms im Definitionsbereich; die Resultate
; der Ausführung werden im Interaktionsbereich angezeigt.


; Arithmetik mit nicht-numerischen Werten
; =======================================

; Wenn wir nur Programme schreiben könnten, die Zahlen verarbeiten, wäre Programmieren genau so 
; langweilig wie Mathematik ;-) Zum Glück gibt es viele andere Arten von Werten, mit denen 
; wir ganz analog zu Zahlen rechnen können, zum Beispiel Text, Wahrheitswerte, Bilder usw.

; Zu jedem dieser sogenannten _Datentypen_ gibt es _Konstruktoren_, mit denen man Werte dieser
; Datentypen konstruieren kann, sowie _Operationen_, die auf Werte dieses Datentyps angewendet
; werden können und die weitere Werte des Datentyps konstruieren. Konstruktoren für numerische
; Werte sind zum Beispiel 42 oder 5.3 (also die Zahlen_literale_; Operationen sind zum Beispiel 
; "+" oder "*".

; Die Konstruktoren für Text (im folgenden auch _String_ genannt) erkennt man an Anführungszeihen. So ist zum Beispiel

"Konzepte der Programmiersprachen"

; ein Stringliteral. Eine Operation auf diesem Datentyp ist string-append, zum Beispiel

(string-append "Konzepte der " "Programmiersprachen") ; ergibt "Konzepte der Programmiersprachen"

; Es gibt weitere Operationen auf Strings: Um Teile aus einem String zu extrahieren, um die Reihenfolge
; der Buchstaben umzukehren, um in Groß- oder Kleinbuchstaben zu konvertieren usw. Zusammen bilden diese
; Operationen die _Arithmetik der Strings_.


; Die Namen dieser ganzen Operationen muss man sich nicht merken; bei Bedarf können die zur Verfügung stehenden
; Operationen für Zahlen, Strings und andere Datentypen in der DrRacket Hilfe nachgeschlagen werden
; unter: Hilfe -> How to Design Programs Languages -> Beginning Student -> Pre-defined Functions

; Einige Operationen haben die Eigenschaft, dass sie Werte eines Datentyps als Operand erwarten, aber
; Werte eines anderen Datentyps als Ergebnis liefern, zum Beispiel die Operation string-length:

(+ (string-length "Programmiersprachen") 5) ; ergibt 24

; Bei Operationen, die mehrere Operanden erwarten, gibt es solche, die Operanden unterschiedlicher Datentypen
; erwarten, zum Beispiel

(replicate 3 "hi") ; ergibt "hihihi"

; Es gibt auch Operationen, die Datentypen ineinander umwandeln, zum Beispiel

(number->string 42) ; ergibt den String "42"

(string->number "42") ; ergibt die Zahl 42

; Ein weiterer wichtiger Datentyp sind Wahrheitswerte (Boolsche Werte). Die einzigen
; Konstruktoren hierfür sind die Literale true und false. Operationen auf boolschen
; Werten sind zum Beispiel die aussagenlogischen Operationen:

(and true true) ; ergibt true
(and true false) ; ergibt false
(or true false) ; ergibt true
(or false false) ; ergibt false
(not false) ; ergibt true

; Boolsche Werte werden auch häufig von Vergleichsoperationen zurückgegeben:

(> 10 9) ; ergibt true
(< -1 0) ; ergibt true
(= 42 9) ; ergibt false
(string=? "hello" "world") ; ergibt false

; Natürlich können Ausdrücke weiterhin beliebig verschachtelt werden, z.B. so:

(and (or (= (string-length "hello world") (string->number "11"))
         (string=? "hello world" "good morning"))
     (>= (+ (string-length "hello world") 60) 80))

; Auftreten und Umgang mit Fehlern
; ================================

; Was passiert wenn ein Operand übergeben wird, der nicht den erwarteten Typ hat?
; In diesem Fall wird die Auswertung abgebrochen und eine Fehlermeldung produziert.
; Zum Beispiel (number->string "asdf") ergibt die Fehlermeldung 
;          number->string: expects a number; given "asdf"

; Manchmal stimmt zwar der Datentyp des Operanden, aber trotzdem 'passt' der Operand
; in irgendeiner Weise nicht. Betrachten Sie beispielsweise den Aufruf:

(string->number "hallo")

; In BSL ist der Wert dieses Ausdrucks "false", ein sogenannter Boolscher Wert (Wahrheitswert),
; um zu signalisieren dass der String keine Zahl repräsentiert. Manchmal kann das "nicht passen"
; eines Operanden auch durch eine Fehlermeldung signalisiert werden, die die Berechung abbricht,
; zum Beispiel
; (/ 1 0)   ergibt den Fehler "/: division by zero".



