#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Batchprogramme und interaktive Programme}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 2}

Ein Programm, wie wir es bisher kennen, besteht immer aus Ausdrücken, Funktionsdefinitionen und Variablendefinitionen.
Aus der Perspektive der Benutzung eines Programms können wir jedoch mehrere Unterarten dieser Programme
unterscheiden. Zwei wichtige Unterarten sind @italic{Batchprogramme} und @italic{interaktive Programme}.



@section{Batchprogramme}
    

@italic{Batchprogramme} bestehen aus einer Hauptfunktion. Die Hauptfunktion nutzt Hilfsfunktionen, die wiederum weitere Hilfsfunktionen
benutzen können. Ein Batchprogramm aufzurufen bedeutet, dass die Hauptfunktion mit der Eingabe des Programms
aufgerufen wird und nach Beendigung der Hauptfunktion mit einem Ergebnis zurückkehrt. Während die Hauptfunktion ausgewertet wird,
gibt es keine Interaktion mit dem Benutzer mehr.

Häufig wird Batchprogrammen ihre Eingabe in Form von Kommandozeilenparametern oder Standardströmen übergeben. 
Weitere Eingaben können aus Dateien kommen, deren Name beispielsweise als Kommandozeilenparameter übergeben wurde.
Die Ausgabe wird häufig über den Standardausgabestrom (stdout) ausgegeben.
Ein gutes Beispiel für Batchprogramme sind die Kommandozeilen-Tools der Unix Welt, wie zum Beispiel @italic{ls}, @italic{grep} oder
@italic{find}. Auch DrRacket gibt es in einer Version als Batchprogramm, @italic{raco}, siehe @url{http://docs.racket-lang.org/raco/index.html}.
@margin-note{Recherchieren Sie, was Standardströme (@italic{standard streams}) wie @italic{stdin} und @italic{stdout} in Unix sind.}

Batchprogramme verhalten sich also in gewissem Sinne wie unsere Funktionen, die wir definieren: 
Wenn man Ihnen eine Eingabe gibt, rechnen sie eine Zeit lang selbstständig und geben dann das Ergebnis zurück. 
Sie wissen bereits, dass Funktionen sehr flexibel miteinander verknüpft und kombiniert werden können. So ist es daher auch
mit Batchprogrammen, die sehr häufig in Skripten oder auf der Kommandozeile miteinander kombiniert werden.
Möchte man beispielsweise in einem Unix Shell die Liste aller Dateien des aktuellen Verzeichnisses wissen, die
im März modifiziert wurden und diese dann nach ihrer Größe sortieren, so können wir das erreichen, indem wir
einfache Batchprogramme wie @tt{ls}, @tt{grep} und @tt{sort} miteinander verknüpfen:

@verbatim|{
$ ls -l | grep "Mar" | sort +4n
-rwxr-xr-x  1 klaus    Admin      193 Mar  7 19:50 sync.bat
-rw-r--r--  1 klaus    Admin      317 Mar 30 12:53 10-modules.bak
-rw-r--r--  1 klaus    Admin      348 Mar  8 12:30 arrow-big.png
-rw-r--r--  1 klaus    Admin      550 Mar 30 13:02 10-modules.scrbl
-rw-r--r--  1 klaus    Admin     3611 Mar  8 12:35 arrow.png
-rw-r--r--  1 klaus    Admin     4083 Mar 27 15:16 marburg-utils.rkt
-rw-r--r--  1 klaus    Admin     4267 Mar 27 15:15 marburg-utils.bak
-rw-r--r--  1 klaus    Admin     7782 Mar 30 13:02 10-modules.html
-rw-r--r--  1 klaus    Admin    14952 Mar  2 13:49 rocket-s.jpg
}|

Ein gutes Beispiel für ein Batchprogramm in BSL ist die @racket[letter] Funktion aus Abschnitt @secref{letterprogram}.
Wir rufen es im Interaktionsbereich mit der gewünschten Eingabe auf und erhalten dann die Ausgabe. Es ist möglich,
dieses Programm auch außerhalb von DrRacket als Batchprogramm zu verwenden. Allerdings muss in diesem Fall die
Eingabe anders übergeben werden, nämlich in Form von Kommandozeilenparametern. Wie dies aussehen kann,
zeigt das folgende Programm. Um auf die Kommandozeilenparameter zuzugreifen, sind 
einige Sprachkonstrukte notwendig, die Sie bisher noch nicht kennen. Versuchen Sie daher nicht, die Details
des unten stehenden Programms zu verstehen; es soll lediglich illustrieren, dass man Racket-Programme ohne
DrRacket als Batchprogramme ausführen kann.

@racketblock[
(require racket/base)

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
   "Koenig Zabutu aus dem Tacka-Tucka-Land ist gestorben.\n"
   "Er hat Sie, "  fst  ", in seinem Testament als Alleinerben eingesetzt.\n"
   "Lieber " fst " " lst ", gegen eine kleine Bearbeitungsgebuehr\n ueberweise ich das Vermoegen."))
 
(define (closing signature-name)
  (string-append
   "Mit freundlichen Gruessen,\n"
   signature-name))

(define args (current-command-line-arguments))

(if (= (vector-length args) 3)
    (display (letter (vector-ref args 0)
                     (vector-ref args 1)
                     (vector-ref args 2)))
    (error "Bitte genau drei Parameter uebergeben"))
]    
    
Falls dieses Programm in der Datei @tt{letter.rkt} abgespeichert ist, so können Sie dieses zum Beispiel in einer
Unix oder DOS Shell so aufrufen:

@verbatim|{
$ racket letter.rkt Tillmann Rendel Klaus
}|
und erhalten dann die folgende Ausgabe:

@verbatim|{
Sehr geehrte(r) Herr/Frau Rendel,
Koenig Zabutu aus dem Tacka-Tucka-Land ist gestorben.
Er hat Sie, Tillmann, in seinem Testament als Alleinerben eingesetzt.
Lieber Tillmann Rendel, gegen eine kleine Bearbeitungsgebuehr
 ueberweise ich das Vermoegen.
Mit freundlichen Gruessen,
Klaus
}|

Dieses Batchprogramm können Sie nun auch mit anderen Kommandozeilenprogrammen verknüpfen; beispielsweise 
können Sie es mit dem @italic{wc} Programm verknüpfen um die die Anzahl der Wörter in dem generierten Text zu bestimmen:

@verbatim|{
$ racket letter.rkt Tillmann Rendel Klaus | wc -w
     36
}|

Das Programm @tt{racket}, das in diesem Befehl aufgerufen wird, ist die Kommandozeilenversion von DrRacket.
Sie können Racket Programme aber natürlich auch ganz ohne die Racket Umgebung ausführen. Beispielsweise
können Sie mit dem Menüeintrag Racket -> Programmdatei eine ausführbare Datei erzeugen, die unabhängig von DrRacket 
auf jedem Rechner mit dem passenden Betriebssystem ausgeführt werden kann.

Wenn Sie in Ihren BSL Programmen Dateien lesen oder schreiben möchten, können Sie hierzu das Teachpack @racket[2htdp/batch-io]
verwenden.

Zusammenfassend läßt sich sagen, dass die Stärke von Batchprogrammen ist, dass sie sich leicht und auf vielfältige
Weise mit anderen Batchprogrammen kombinieren lassen, entweder auf der Kommandozeile oder auch automatisiert 
innerhalb von Batch-Files oder Shell-Skripten. Allerdings sind Batchprogramme definitionsgemäß nicht
für Interaktionen mit dem Benutzer geeignet.

@section{Interaktive Programme}

Interaktive Programme bestehen aus mehreren Hauptfunktionen sowie einem Ausdruck, der den Computer
informiert, welche dieser Funktionen welche Art von Eingaben verarbeitet und welche der Funktionen
Ausgaben produziert. Jeder dieser Hauptfunktionen kann natürlich wieder Hilfsfunktionen verwenden.
Wir werden zur Konstruktion interaktiver Programme das Universe Teachpack verwenden.

@subsection[#:tag "universeteachpack"]{Das Universe Teachpack}

Das "universe" Teachpack unterstützt die Konstruktion interaktiver Programme: Programme, die auf Zeitsignale,
Mausklicks, Tastatureingaben oder Netzwerkverkehr reagieren und grafische Ausgaben erzeugen.
Wir werden die Funktionsweise dieses Teachpacks anhand des folgenden Beispiels erklären. Bitte probieren
Sie vor dem weiterlesen aus, was dieses Programm macht.

@#reader scribble/comment-reader
(racketblock
; WorldState is a number
; interp. the countdown when the bomb explodes

; WorldState -> WorldState
; reduces countdown by one whenever a clock tick occurs
(check-expect (on-tick-event 42) 41)

(define (on-tick-event world) (- world 1))


; WorldState Number Number MouseEvent -> WorldState
; decreases countdown by 100 when mouse button is pressed
(check-expect (on-mouse-event 300 12 27 "button-down") 200)
(check-expect (on-mouse-event 300 12 27 "button-up") 300)

(define (on-mouse-event world mouse-x mouse-y mouse-event)
  (if (string=? mouse-event "button-down")
      (- world 100)
      world))

      
; WorldState -> Image
; renders the current countdown t as an image
(check-expect (image? (render 100)) true)
(check-expect (image? (render 0)) true)
(define (render world)
  (if (> countdown 0)
      (above (text (string-append "Countdown for the bomb: " 
                                  (number->string world)) 
                   30 "red")
             (text "Click to disarm!" 30 "red")) 
      (text "Boooom!!" 60 "red")))

      
; WorldState -> Boolean
; the program is over when the countdown is <= 0
(check-expect (end-of-the-world 42) false)
(check-expect (end-of-the-world 0) true)
(define (end-of-the-world world) (<= world 0))

; install all event handlers; initialize world state to 500
(big-bang 500
          (on-tick on-tick-event 0.1)
          (on-mouse on-mouse-event)
          (to-draw render)
          (stop-when end-of-the-world))
)          

Ein zentrales Konzept bei interaktiven Programmen ist das des @italic{WorldState}s.
Eine interaktive Anwendung befindet sich typischerweise in einem bestimmten @italic{Zustand},
den wir WorldState nennen. 
@margin-note{Falls Sie Pacman nicht kennen, suchen Sie bitte unverzüglich eine Online-Version
             von Pacman im Internet und spielen eine Runde!}
Der Zustand bei einem Pacman-Spiel umfasst beispielsweise die gegenwärtige Position
aller Spielfiguren und den aktuellen Punktestand. Im Programm oben besteht der aktuelle
Zustand lediglich aus einer Zahl, die den aktuellen Countdown zur Bombenexplosion enthält.

Der Zustand eines interaktiven Programms ändert sich, wenn bestimmte @italic{Ereignisse} eintreten.
Ereignisse können zum Beispiel das Drücken von Tasten auf der Tastatur, Aktionen mit der Maus oder
der Ablauf bestimmter Zeitintervalle (@italic{Timer}) sein. Auf Ereignisse wird in Form von @italic{Event Handlern}
reagiert. Ein Event Handler ist eine Funktion, die den gegenwärtigen WorldState sowie weitere
Informationen über das eingetretene Ereignis als Eingabe erhält und einen neuen WorldState als
Ausgabe produziert.
                        
Im Beispiel oben werden zwei Event Handler definiert, @racket[on-mouse-event] für Mausevents 
und @racket[on-tick-event] für Timer-Ereignisse. Das Intervall zwischen zwei Timer-Ereignissen
werden wir später auf 0.1 Sekunden festlegen. In der Anwendung oben haben wir uns dafür entschieden,
jedesmal beim Eintreten eines Timer-Events den aktuellen Countdown um eins zu reduzieren. 

Die @racket{on-mouse-event} Funktion bekommt als Eingabe außer dem aktuellen WorldState noch die
Position der Maus sowie die genaue Art des Mausereignisses als Eingabe. In unserem Beispiel wird
der Countdown bei jedem Klick auf die Maus um 100 erniedrigt.

Die @racket[render] Funktion produziert aus dem aktuellen WorldState ein Bild, welches den
aktuellen Zustand des Programms grafisch darstellt. Diese Funktion wird jedesmal aufgerufen, wenn
sich der aktuelle WorldState geändert hat.

Die letzte Funktion, @racket[end-of-the-world], dient dazu, zu überprüfen, ob die Anwendung beendet
werden soll. Hierzu prüft sie den WorldState und produziert als Ergebnis einen Wahrheitswert.
In unserem Beispiel möchten wir die Anwendung beenden, nachdem die Bombe explodiert ist.

Der letzte @racket[big-bang] Ausdruck dient dazu, den WorldState zu initialisieren und alle Event Handler
zu installieren. Der @racket[big-bang] Operator ist eine Sonderform, also keine normale BSL Funktion.
Das erste Argument von @racket[big-bang] ist der initiale WorldState. Da wir uns dafür entschieden haben,
dass in dieser Anwendung der WorldState eine Zahl ist, die den aktuellen Countdown repräsentiert, 
wählen wir einen initialen Countdown, @racket[500].

Die Unterausdrücke wie @racket[(on-tick on-tick-event)] sind keine Funktionsaufrufe sondern spezielle
Klauseln des @racket[big-bang] Operators, in denen man die Event-Handler Funktionen sowie die Funktionen
zum Zeichnen (@racket[to-draw]) und Beenden (@racket[stop-when]) des Programms angibt. Selbstverständlich
hätten wir den Event-Handler Funktionen auch andere Namen geben können -- der @racket[big-bang] ist
die Stelle, wo definiert wird, welcher Ereignistyp mit welcher Event-Handler Funktion verknüpft wird.

Einige dieser Klauseln erwarten außer dem Namen der Event-Handler Funktion noch weitere Argumente.
So sagt beispielsweise die Zahl in der @racket[(on-tick on-tick-event 0.1)], dass alle 0,1 Sekunden
ein Timer-Event ausgelöst werden soll. Man kann diese Angabe auch weglassen; dann wird als
Standardwert 1/28 Sekunde angenommen. Es gibt noch eine Reihe weiterer Klauseln und Varianten der
oben stehenden Klauseln; für weitere Informationen dazu siehe die Dokumentation des universe Teachpacks.
Bis auf die @racket[to-draw] Klauseln sind alle diese Klauseln optional; ist für einen Eventtyp
kein Event Handler installiert, werden diese Events ignoriert.

Wird ein interaktives Programm (im folgenden auch @italic{World Programm}) beendet, so gibt der
@racket[big-bang] Ausdruck den letzten WorldState zurück.

Nehmen wir beispielsweise an, dass in folgender Reihenfolge folgende Events eintreten: 

@itemize[#:style 'ordered
       @item{ein Timerevent}
       @item{ein Timerevent}
       @item{ein Mausevent (die Maus wurde an die Position (123,456) bewegt)}
       @item{ein Mausevent (Maustaste an der Stelle (123,456) wurde gedrückt)}]

Nehmen wir an, der Zustand vor Eintreten dieser Events ist @racket[500].
Nach Eintreten des jeweiligen Ereignisses ist der aktuelle Zustand:

@itemize[#:style 'ordered
       @item{@racket[499], also @racket[(on-tick-event 500)]}
       @item{@racket[498], also @racket[(on-tick-event 499)]}
       @item{@racket[498], also @racket[(on-mouse-event 498 123 456 "move")]} 
       @item{@racket[398], also @racket[(on-mouse-event 498 123 456 "button-down")]}]
       
Da Event Handler nur Funktionen sind, kann man das letzte Resultat auch durch die Funktionskomposition der Event Handler berechnen:

@racketblock[
(on-mouse-event 
  (on-mouse-event 
    (on-tick-event 
      (on-tick-event 500)) 
    123 456 "move") 
  123 456 "button-down")] 
               
Die Reihenfolge der Ereignisse bestimmt also die Reihenfolge, in der die Event Handler Funktionen hintereinandergeschaltet werden.
Möchte man, zum Beispiel in Tests, das Eintreten einer bestimmten Sequenz von Ereignissen simulieren, so kann man dies
also einfach durch Komposition der entsprechenden Event Handler tun. Dies ist wichtig, denn viele interessante Situationen (die man testen möchte)
ergeben sich erst in der Komposition verschiedener Ereignisse.
