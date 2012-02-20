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

