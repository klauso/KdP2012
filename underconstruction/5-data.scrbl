#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Summen}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 2}

Die Datentypen, die wir bisher kennengelernt und genutzt haben, umfassen Zahlen, Strings, Wahrheitswerte
sowie in informellen Datendefinitionen definierte Datentypen wie @racket[Temperatur].

Um realistische Anwendungen zu programmieren, ist es hilfreich, ein größeres und flexibel erweiterbares
Repertoire an Datentypen zu haben. In diesem Kapitel schauen wir uns an, 
wie man mit Datentypen unterschiedliche Situationen unterscheiden kann und wie diese Datentypen
den Entwurf von Programmen beeinflussen.

@section{Auzählungstypen}
Häufig hat man den Fall, dass ein Datentyp nur eine endliche aufzählbare Menge von Werten enthält.
Wir nennen solche Datentypen @italic{Aufzählungstypen} oder @italic{Enumerationstypen}.
Hier ist ein Beispiel wie ein Aufzählungstyp definiert wird:

@#reader scribble/comment-reader
(racketblock
; A TrafficLight shows one of three colors:
; – "red"
; – "green"
; – "yellow"
; interp. each element of TrafficLight represents which colored
; bulb is currently turned on
  )

Das Programmieren mit Aufzählungstypen ist sehr einfach. Wenn eine Funktion einen Parameter hat, der einen Aufzählungstyp  
hat, dann enthält das Programm typischerweise eine Fallunterscheidung, in denen alle Alternativen des Aufzählungstyps
separat voneinander definiert werden. Hier ein Beispiel:

@#reader scribble/comment-reader
(racketblock
; TrafficLight -> TrafficLight
; given state s, determine the next state of the traffic light
 
(check-expect (traffic-light-next "red") "green")
 
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))  
  )
    
  
Manchmal ist eine Funktion auch nur an einer Teilmenge der möglichen Werte eines Aufzählungstypen interessiert. In diesem
Fall werden nur die interessanten Fälle abgefragt und der Rest ignoriert. Ein Beispiel dafür ist die 
@racket[on-mouse-event] Funktion aus @secref{universeteachpack}, die eine Eingabe vom Aufzählungstypen @racket[MouseEvent]
erhält. @racket[MouseEvent] ist folgendermaßen definiert:

@#reader scribble/comment-reader
(racketblock
; A MouseEvt is one of these strings:
; – "button-down"
; – "button-up"
; – "drag"
; – "move"
; – "enter"
; – "leave"
)

Es kann auch sein, dass ein Funktionsparameter einen Aufzählungstyp hat, aber trotzdem keine Fallunterscheidung vornimmt, weil
hierzu eine Hilfsfunktion aufgerufen wird. Dennoch ist es eine gute Heuristik, im Schritt 3 des Entwurfsrezepts 
aus Abschnitt @secref{entwurfsrezept} mit einem Funktionstemplate zu starten, welches alle Fälle des Parameters unterscheidet.
Beispielsweise würde das Template für die @racket[traffic-light-next] Funktion von oben so aussehen:

@racketblock[
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) ...]
    [(string=? "green" s) ...]
    [(string=? "yellow" s) ...]))]

Wie sie sehen, können sie einen großen Teil einer Funktionsdefinition quasi mechanisch generieren, indem Sie systematisch den
Schritten aus dem Entwurfsrezept folgen.

@section{Intervalltypen}

Betrachten Sie ein Programm, welches ein Ufo beim landen zeigen soll.
Ein Programm hierzu könnte wie folgt aussehen:

@#reader scribble/comment-reader
(racketblock
; constants:
(define WIDTH 300)
(define HEIGHT 100)
 
; visual constants:
(define MT (empty-scene WIDTH HEIGHT))
(define UFO
  (overlay (circle 10 "solid" "green")
           (rectangle 40 2 "solid" "green")))

(define BOTTOM (- HEIGHT (/ (image-height UFO) 2)))

; A WorldState is a number. 
; interp. height of UFO (from top)

; WorldState -> WorldState
; compute next location of UFO
(define (nxt y)
  (+ y 1))
 
; WorldState -> Image
; place UFO at given height into the center of MT
(define (render y)
  (place-image UFO (/ WIDTH 2) y MT))

; WorldState -> Boolean
; returns true when UFO has reached the bottom, i.e. y >= BOTTOM
(define (end-of-the-world y) (>= y BOTTOM))

; wire everything together; start descend at position 0
(big-bang 0 (on-tick nxt) (to-draw render) (stop-when end-of-the-world))
)

Betrachten Sie nun folgende Erweiterung der Problemstellung:

@italic{The status line should say "descending" when the UFO’s height is above one third of the height of the canvas. 
It should switch to "closing in" below that. And finally, when the UFO has reached the bottom of the canvas, 
the status should notify the player that the UFO has "landed."}

Der Datentyp, die sich in dieser Problemstellung versteckt, ist kein Enumerationstyp, denn es gibt eine unendliche
Zahl unterschiedlicher Höhen (oder, sofern wir nur die ganzen Zahlen zählen, so viele unterschiedliche Höhen, dass
es unpraktisch wäre, hierfür einen Aufzählungstypen zu definieren).

Daher verwenden wir @italic{Intervalle}, um zusätzliche Struktur auf geordneten Datentypen wie Zahlen oder Strings zu definieren. 
Im folgenden konzentrieren wir uns auf (ganzzahlige oder nicht-ganzzahlige) Zahlen. Ein Intervall wird durch seine @italic{Grenzen}
definiert. Ein Intervall hat entweder eine unter und obere Grenze, oder es hat nur eine dieser Grenzen und ist zur anderen Seite offen.

In unserem Beispiel können wir die Datendefinition für den WorldState als Intervall ausdrücken, um eine Datendefinition zu
haben, die die Intention der Problemstellung besser erfasst.


@#reader scribble/comment-reader
(racketblock
; constants:
(define CLOSE (* 2 (/ HEIGHT 3)))

; A WorldState is a number. It falls into one of three intervals:
; – between 0 and CLOSE
; – between CLOSE and BOTTOM
; – at BOTTOM
; interp. height of UFO (from top)
)

Nicht alle Funktionen, die einen Intervalltypen als Argument bekommen, nutzen diese zusätzliche Struktur.
So kann zum Beispiel die @racket[render] Funktion von oben so wie sie ist, weil das Intervall nur für die 
Statusanzeige aber nicht für das Ufo relevant ist.

Funktionen, die jedoch die zusätzliche Struktur des Intervalls benötigen, enthalten typischerweise einen
@racket[cond] Ausdruck, der die unterschiedlichen Intervalle unterscheidet.

@#reader scribble/comment-reader
(racketblock
; WorldState -> Image
; add a status line to the scene create by render  
(define (render/status y)
  (cond
    [(<= 0 y CLOSE) (above (text "descending" 12 "black") (render y))]
    [(< CLOSE y BOTTOM) (above (text "closing in" 12 "black") (render y))]
    [(= y BOTTOM) (above (text "landed" 12 "black") (render y))]))
)

Es empfiehlt sich, diesen @racket[cond] Ausdruck im Rahmen der Templatekonstruktion aus dem Entwurfsrezept direkt in das Template mit
aufzunehmen. Allerdings findet die Fallunterscheidung nicht notwendigerweise als erstes statt sondern kann auch tiefer
im Funktionsbody stattfinden. Dies bietet sich in unserem Beispiel an, denn wir haben gegen das DRY Prinzip verstossen (Wenn wir
die Farbe des Textes beispielsweise auf "red" ändern möchten müssten wir drei Zeilen ändern). Deshalb ist es vorteilhaft,
den konditionalen Ausdruck nach innen zu ziehen:

@#reader scribble/comment-reader
(racketblock
; WorldState -> Image
; add a status line to the scene create by render  
(define (render/status y)
  (above
   (text 
     (cond
      [(<= 0 y CLOSE) "descending"]
      [(< CLOSE y BOTTOM) "closing in"]
      [(= y BOTTOM) "landed"])
     12 "black")
   (render y)))
)

Nun muss nur noch der @racket[big-bang] Ausdruck angepasst werden, so dass @racket[render/status] und nicht mehr @racket[render] zum Zeichnen verwendet wird.
@#reader scribble/comment-reader
(racketblock
; wire everything together; start descend at position 0
(big-bang 0 (on-tick nxt) (to-draw render/status) (stop-when end-of-the-world))
)

Intervalle liefern uns außer neuen Funktionstemplates auch Anhalte zum Testen: Typischerweise möchte man einen Testcase für jedes Intervall
und insbesondere für die Intervallgrenzen haben.

@section{Summentypen}

Intervalltypen unterscheiden unterschiedliche Teilmengen der Zahlen (oder anderer geordneter Werte).
Aufzählungstypen zählen die unterschiedlichen Elemente des Typs Wert für Wert auf.

@italic{Summentypen} verallgemeinern Intervalltypen und Aufzählungstypen.
Mit Summentypen können existierende Datentypen und individuelle 
Werte beliebig miteinander kombiniert werden. Ein Summentyp gibt verschiedene
Alternativen an, von denen jeder Wert dieses Typs genau eine Alternative erfüllt.

Betrachten wir als Beispiel die @racket[string->number] Funktion, die einen
String in eine Zahl konvertiert, falls dies möglich ist, und andernfalls @racket[false]
zurückgibt.

Hier ist die Definition eines Summentyps, der das Verhalten dieser Funktion beschreibt:

@#reader scribble/comment-reader
(racketblock
; A NorF is one of:
; – false
; – a Number
)

Damit können wir für @racket[string->number] folgende Signatur definieren:

@#reader scribble/comment-reader
(racketblock
; string->number : String -> NorF
; converts the given string into a number;
; produces false if impossible 
)


Summentypen werden manchmal auch Vereinigungstypen (union types) genannt. 
In HTDP/2e werden sie @italic{Itemizations} genannt. 

In der Dokumentation der HTDP-Sprachen ist die Signatur von 
@racket[string->number] so angegeben:

@racketblock[String -> (union Number false)]

Der Operator @racket[union] steht für die ``on-the-fly'' Konstruktion eines 
anonymen Summentyps mit der gleichen Bedeutung wie unser @racket[NorF] oben.

Was macht man mit einem Wert der einen Summentyp hat? Wie bei Aufzählungs- und Intervalltypen
auch ist typischerweise die einzige sinnvolle Operation die, welche die unterschiedlichen
Alternativen voneinander trennt, beispielsweise im Rahmen eines @racket[cond] Ausdrucks.
Hier ein Beispiel:

@#reader scribble/comment-reader
(racketblock
; NorF -> NorF
; adds 3 to a if it is a number; returns false otherwise
(check-expect (add3 5) 8)
(check-expect (add3 false) false)
(define (add3 a)
  (cond [(number? a) (+ a 3)]
        [else false]))
)        
        
Funktion mit Summentypen entscheiden sich in ihrer Entwurfsmethodik etwas von den Funktionen, die
wir bisher kennengelernt haben. Deswegen gehen wir nochmal durch unser Entwurfsrezept (@secref{entwurfsrezept}) und beschreiben,
wie sich der Entwurf von Funktionen mit Summentypen vom allgemeinen Entwurfsrezept unterscheidet.

@italic{The tax on an item is either an absolute tax of 5or 10 currency units, or a linear tax. Design a function that computes the price
of an item after applying its tax.}

Das Entwurfsrezept für den Entwurf von Funktionen ergänzen wir wie folgt:
@itemize[#:style 'ordered
       @item{Falls die Problemstellung Werte in unterschiedliche Klassen unterteilt, 
             so sollten diese Klassen durch eine Datendefinition explizit definiert werden.
             
             Beispiel: Die Problemstellung oben unterscheidet drei verschiedene Arten
             der Steuer. Dies motiviert folgende Datendefinition:
             
@#reader scribble/comment-reader
(racketblock
; A Tax is one of
; - "absolute5" 
; - "absolute10"
; - a Number representing a linear tax rate in percent
)             }
       
       @item{Im zweiten Schritt ändert sich nichts, außer dass typischerweise der
             definierte Datentyp in der Signatur verwendet wird.
             
             Beispiel:

@#reader scribble/comment-reader
(racketblock
; Number Tax -> Number
; computes price an an item after applying tax
(define (total-price itemprice tax) 0)
)             
             }
       @item{Bzgl. der Tests ist es ratsam, pro Alternative des Datentypen mindestens
             ein Beispiel zu haben. Bei Intervallen sollten die Grenzen der Intervalle
             getestet werden. Gibt es mehrere Parameter mit Summentypen, so sollten
             alle Kombinationen der Alternativen getestet werden.
             
             Beispiel:
             
@#reader scribble/comment-reader
(racketblock
(check-expect (total-price 10 "absolute5") 15)
(check-expect (total-price 10 "absolute10") 20)
(check-expect (total-price 10 25) 12.50)
)             
             }
       @item{Die größte Neuerung ist die des Templates für die Funktion. Im allgemeinen
             @italic{folgt die Struktur der Funktionen aus der Struktur der Daten}.
             Dies bedeutet, daß in den meisten Fällen der Funktionskörper mit
             einem @racket[cond] Ausdruck startet, der die unterschiedlichen Fälle
             des Summentypen unterscheidet.

@margin-note{Wieso ist es in diesem Beispiel die Reihenfolge der Zweige des @racket[cond]
                                                                            Ausdrucks wichtig?}             
@#reader scribble/comment-reader
(racketblock
(define (total-price itemprice tax)
  (cond [(number? tax) ...]
        [(string=? tax "absolute5") ...]
        [(string=? tax "absolute10") ...]))
)}
  @item{@margin-note{Diese Unterscheidung der Fälle ist ein Beispiel
        für ein allgemeineres Entwurfskonzept für komplexe Systeme, 
        welches sich @italic{separation of concerns} (Trennung der Belange) nennt.}
        Im fünften Schritt werden die @racket[...] Platzhalter durch den korrekten Code
        ersetzt. Hier ist es sinnvoll, jeden Zweig des @racket[cond] Ausdrucks einzeln
        und nacheinander durchzugehen und zu implementieren. Der Vorteil ist, daß sie
        sich bei dieser Art der Implementierung immer nur um einen der Fälle kümmern müssen
        und alle anderen Fälle ignorieren können. 
        
        Möglicherweise stellen Sie während dieses Schritts fest, dass es sinnvoll ist,
        den @racket[cond] Ausdruck nach innen zu ziehen (ähnlich wie 
        in @racket[create-rocket-scene-v6] in Abschnitt @secref{dryredux}), oder 
        vielleicht müssen Sie gar nicht alle Fälle unterscheiden, oder vielleicht 
        möchten Sie die Unterscheidung auch in eine Hilfsfunktion auslagern --- 
        dennoch ist es in der Regel sinnvoll, mit diesem Template zu starten, selbst
        wenn am Ende ihre Funktion anders aussieht.

        Beispiel:
@#reader scribble/comment-reader
(racketblock
(define (total-price itemprice tax)
  (cond [(number? tax) (* itemprice (+ 1 (/ tax 100)))]
        [(string=? tax "absolute5") (+ itemprice 5)]
        [(string=? tax "absolute10") (+ itemprice 10)]))
)
        }
  @item{Im letzten Schritt ändert sich nichts, aber überprüfen Sie,
        dass Sie Testfälle für alle Alternativen definiert haben.}                                                    
              
              ]

  
@section{Unterscheidbarkeit der Alternativen}

Was wäre, wenn wir in unserem letzten Beispiel statt zwei absoluten Steuersätzen ein kontinuierliches
Spektrum hätten? Wir könnten unsere Datendefinition wie folgt abändern:

@#reader scribble/comment-reader
(racketblock
; A Tax is one of
; - a Number representing an absolute tax in currency units 
; - a Number representing a linear tax rate in percent
) 

So weit so gut --- aber wie können wir in einem @racket[cond] Ausdruck diese Fälle unterscheiden? Wir haben
zwar das Prädikat @racket[number?], aber damit können wir nicht zwischen diesen beiden Fällen unterscheiden.

In unseren Summentypen ist es wichtig, dass man eindeutig unterscheiden kann, welche Alternative in einem Wert, der
einen Summentyp hat, gewählt wurde. Daher müssen die Mengen der möglichen Werte für jede Alternative disjunkt sein.

@margin-note{Die Variante der Summentypen, die wir verwenden, bezeichnet man deshalb auch als @italic{untagged unions}.}
Falls sie nicht disjunkt sind, muss man zu jedem Wert eine zusätzliche Information abspeichern, die aussagt, zu welcher
Alternative dieser Wert gehört: ein sogenanntes @italic{tag} ("Etikett", "Anhänger"). 
Mit den Mitteln, die wir bisher kennengelernt haben, 
können wir den oben gewünschten Datentyp nicht sinnvoll ausdrücken. Im nächsten Kapitel werden wir jedoch
ein Sprachkonstrukt kennenlernen, mit dem man solche @italic{tags} und damit auch solche Datentypen strukturiert
definieren kann.

