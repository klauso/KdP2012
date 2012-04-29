#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Programmierer entwerfen Sprachen!}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 1}

Die Programme, die Sie bisher geschrieben haben, waren im Wesentlichen
Berechnungen, wie man sie auch auf einem Taschenrechner durchführen könnte ---
allerdings mit dem großen Unterschied dass BSL die Arithmetik von vielen Arten von Werten
beherrscht und nicht nur die der Zahlen. Bei den Funktionen, die Sie in
diesen Ausdrücken verwenden können, können Sie aus einer festen Menge vordefinierter
Funktionen wählen. Das @italic{Vokabular} (Menge der Funktionen), welches sie verwenden können, ist also
fix.

Eine echte Programmiersprache unterscheidet sich von einem Taschenrechner dadurch,
dass Sie selber, auf Basis der bereits bestehenden Funktionen, neue Funktionen definieren können
und danach in Ausdrücken und Definitionen neuer Funktionen benutzen können. Im Allgemeinen können
Sie @italic{Namen} definieren und damit das Vokabular das Ihnen (und, bei Veröffentlichung ihrer Programme auch anderen)
zur Verfügung steht selber erweitern. Ein Programm ist also mehr als eine Menge von Maschineninstruktionen - 
es definiert auch eine @italic{Sprache} für die Domäne des Programms.
Sie werden sehen, dass die Möglichkeit, neue Namen zu definieren, das
Schlüsselkonzept ist, um mit der Komplexität großer Softwaresysteme umzugehen. 
 

@section[#:tag "redundanz"]{Funktionsdefinitionen}

Hier ist die Definition einer Funktion
die den Durchschnitt von zwei Zahlen berechnet.

@block[(define (average x y) (/ (+ x y) 2))]

Wenn diese Funktionsdefinition in den Definitionsbereich geschrieben und dann auf "Start" gedrückt wird, 
passiert --- nichts. Eine Funktionsdefinition ist @italic{kein} Ausdruck. Der Effekt dieser Definition
ist der, dass der Name der neuen Funktion nun in Ausdrücken verwendet werden kann:

@ex[(average 12 17)
    (average 100 200)]

Diese Werte hätten natürlich auch ohne die vorherige Funktionsdefinition berechnet werden können:

@ex[(/ (+ 12 17) 2)
    (/ (+ 100 200) 2)]

Allerdings sehen wir, dass die zweite Variante redundant ist: Der Algorithmus zur
Berechnung des Durchschnitts wurde zweimal repliziert. Sie ist auch weniger abstrakt und 
weniger leicht verständlich, denn wir müssen erst verstehen, dass der Algorithmus 
den Durchschnitt zweier Zahlen berechnet, während wir in der ersten Version dem Algorithmus
einen @italic{Namen} gegeben haben, der die Details des Algorithmus verbirgt.

Gute Programmierer versuchen im Allgemeinen, jede Art von Redundanz in Programmen zu vermeiden.
Dies wird manchmal als das DRY (Don't repeat yourself) Prinzip bezeichnet. Der Grund dafür
ist nicht nur, dass man Schreibarbeit spart, sondern auch dass redundante Programme schwerer
zu verstehen und zu warten sind: Wenn ich einen Algorithmus später ändern möchte, so muss ich
in einem redundanten Programm erst alle Kopien dieses Algorithmus finden und jede davon ändern.
Daher ist Programmieren niemals eine monotone repetitive Tätigkeit, denn wiederkehrende Muster
können in Funktionsdefinitionen (und anderen Formen der Abstraktion die sie noch kennenlernen werden)
gekapselt und wiederverwendet werden.


Im Allgemeinen Fall haben Funktionsdefinitionen diese Form:

@racketblock[(define (FunctionName InputName1 InputName2 ...) BodyExpression)]

Funktionsdefinitionen sind @italic{keine} Ausdrücke sondern eine neue Kategorie von Programmen.
Funktionsdefinitionen dürfen also beispielsweise nicht als Argument von Funktionen verwendet werden.
Eine Funktionsdefinition startet mit dem Schlüsselwort @racket[define].
Der einzige Zweck dieses Schlüsselworts ist der, Funktionsdefinitionen von
Ausdrücken unterscheiden zu können. Insbesondere darf es also keine Funktionen
geben, die @racket[define] heißen.
@racket[FunctionName] ist der name der Funktion.
Diesen benötigt man, um die Funktion in Ausdrücken benutzen (oder: @italic{aufrufen}) zu können.
@racket[InputName1], @racket[InputName2] und so weiter sind die @italic{Parameter}
der Funktion. Die Parameter repräsentieren die Eingabe der Funktion, die erst bekannt wird wenn
die Funktion aufgerufen wird. Die @racket[BodyExpression] ist ein Ausdruck der die
Ausgabe der Funktion definiert. Innerhalb der @racket[BodyExpression] werden in der Regel
die Parameter der Funktion benutzt. 

Funktionsaufrufe haben die Form:

@racketblock[(FunctionName ArgumentExpression1 ArgumentExpression1 ...)]

Ein Funktionsaufruf einer mit @racket[define] definierten (@italic{benutzerdefinierte})
Funktion sieht also genau so aus wie
das Benutzen einer fest eingebauten (@italic{primitiven}) Funktion. 
Dies ist kein Zufall. Dadurch, dass man nicht sehen kann, ob man gerade eine primitive Funktion
oder eine benutzerdefinierte Funktion aufruft, ist es leichter, die Programmiersprache
selber zu erweitern oder zu verändern. Zum Beispiel kann aus einer primitiven Funktion
eine benutzerdefinierte Funktion gemacht werden, oder ein Programmierer kann Erweiterungen
definieren die so aussehen, als wäre die Sprache um primitive Funktionen erweitert worden.

@section{Funktionen die Bilder produzieren}

Selbstverständlich können in BSL Funktionen nicht nur Zahlen sondern beliebige Werte als Eingabe
bekommen oder als Ausgabe zurückliefern. In Abschnitt @secref{arithmeticnm} haben Sie gesehen,
wie man mit @racket[place-image] ein Bild in einer Szene plaziert. Zum Beispiel erzeugen die
drei Ausdrücke 
@racketblock[
(place-image (unsyntax @ev[rocket]) 50 20 (empty-scene 100 100))
(place-image (unsyntax @ev[rocket]) 50 40 (empty-scene 100 100))
(place-image (unsyntax @ev[rocket]) 50 60 (empty-scene 100 100))]

die Bilder

@ev[(place-image rocket 50 20 (empty-scene 100 100))]
@ev[(place-image rocket 50 40 (empty-scene 100 100))]
@ev[(place-image rocket 50 60 (empty-scene 100 100))]

Offensichtlich sind diese drei Ausdrücke zusammen redundant, denn sie unterscheiden sich nur in
dem Parameter für die Höhe der Rakete. Mit einer Funktionsdefinition können wir das
Muster, welches diese drei Ausdrücke gemein haben, ausdrücken:

@racketblock[
(define (create-rocket-scene height)
  (place-image (unsyntax @ev[rocket]) 50 height (empty-scene 100 100)))]
                                                                        
@(void (interaction-eval #:eval stdeval (define (create-rocket-scene height)
  (place-image rocket 50 height (empty-scene 100 100)))))

Die drei Bilder können nun durch Aufrufe der Funktion erzeugt werden.

@ex[(create-rocket-scene 20)
    (create-rocket-scene 40)
    (create-rocket-scene 60)] 
                             
Sie können den Höhenparameter auch als Zeitparameter auffassen; die Funktion bildet also Zeitpunkte auf Bilder ab.
Solche Funktionen können wir auch als Film oder Animation auffassen, denn ein Film ist dadurch charakterisiert, dass es zu jedem Zeitpunkt ein
dazugehöriges Bild gibt.

@margin-note{Ein Teachpack ist eine Bibliothek mit Funktionen die sie in ihrem Programm verwenden können. Sie können ein Teachpack über das Menü "Sprache"->"Teachpack hinzufügen" aktivieren.}
Im Teachpack @racket[universe]  gibt es eine Funktion, die den Film, der zu einer solchen Funktion korrespondiert, auf dem Bildschirm vorführt. Diese Funktion
heißt @racket[animate]. Die Auswertung des Ausdrucks 


@racketblock[(animate create-rocket-scence)]

bewirkt, dass ein neues Fenster geöffnet wird in dem eine Animation zu sehen ist, die zeigt, wie sich die Rakete von 
oben nach unten bewegt und schließlich verschwindet. Wenn sie das Fenster schließen wird eine Zahl im Interaktionsbereich
angezeigt; diese Zahl steht für die aktuelle Höhe der Rakete zu dem Zeitpunkt als das Fenster geschlossen wurde.

Die @racket[animate] Funktion bewirkt folgendes: Eine Stoppuhr wird mit dem Wert 0 initialisiert; 28 mal pro Sekunde 
wird der Zählerwert um eins erhöht. Jedesmal wenn der Zähler um eins erhöht wird, wird die Funktion @racket[create-rocket-scene]
ausgewertet und das resultierende Bild in dem Fenster angezeigt.

@section{Bedeutung von Funktionsdefinitionen}
Um die Bedeutung von Funktionsdefinitionen zu definieren, müssen wir sagen, wie Funktionsdefinitionen und Funktionsaufrufe ausgewertet werden.
Durch Funktionsdefinitionen können Ausdrücke nicht mehr isoliert (ohne Berücksichtigung des Rests des Programms) ausgewertet werden; sie werden
im @italic{Kontext} einer Menge von Funktionsdefinitionen ausgewertet. Dieser Kontext umfasst die Menge aller Funktionsdefinitionen, die 
im Programmtext @italic{vor} dem auszuwertenden Ausdruck stehen. Um unsere formale Notation nicht unnötig schwergewichtig zu machen, 
werden wir diesen Kontext nicht explizit zur Reduktionsrelation hinzufügen; stattdessen gehen wir einfach davon aus, dass es einen globalen
Kontext mit einer Menge von Funktionsdefinitionen gibt.

@margin-note{@para{Wie wird der Ausdruck @racket[(* (- 1 1) (+ 2 3))] ausgewert? Wieviele Schritte werden benötigt?}
              @para{}
              @para{Wie wird der Ausdruck @racket[(and (= 1 2) (= 3 3))] ausgewertet? Wieviele Schritte werden benötigt?}
              @para{}
              @para{Können Sie eine Funktion @racket[mul] programmieren, die dasselbe wie @racket[*] berechnet, aber (ähnlich wie @racket[and]) weniger Schritte benötigt?}}

Die Auswertungsregeln aus @secref{semanticsofexpressions} werden nun zur Berücksichtigung von Funktionsdefinitionen wie folgt erweitert:
   @itemize[
   @item{Falls der Ausdruck die Form @racket[(f (unsyntax @v1) ... (unsyntax @vN))] hat und @racket[f] eine primitive (eingebaute) Funktion ist und
                   die Anwendung von @racket[f] auf   @v1,...,@vN den Wert @racket[v] ergibt, dann 
                   @racket[(f (unsyntax @v1) ... (unsyntax @vN))]  @step @racket[v].}
   @item{Falls der Ausdruck die Form @racket[(f (unsyntax @v1) ... (unsyntax @vN))] hat und @racket[f] @italic{keine} primitive Funktion ist und
                   der Kontext die Funktionsdefinition @racketblock[(define (f (unsyntax @x1) ... (unsyntax @xN)) BodyExpression)] enthält, 
                   dann @racket[(f (unsyntax @v1) ... (unsyntax @vN))] @step @racket[NewBody], wobei @racket[NewBody] aus @racket[BodyExpression]
                   entsteht, indem man alle Vorkommen von @xI durch @vI ersetzt (für i=1...n).}] 
Unverändert gilt die Kongruenzregel aus @secref{semanticsofexpressions}. 

@italic{Beispiel}: Unser Programm enthält folgende Funktionsdefinitionen.

@racketblock[
(define (g y z) (+ (f y) y (f z)))
(define (f x) (* x 2))                                  
]

Dann @racket[(g (+ 2 3) 4)] 
     @step @racket[(g 5 4)] 
     @step @racket[(+ (f 5) 5 (f 4))] 
     @step @racket[(+ (* 5 2) 5 (f 4))]
     @step @racket[(+ 10 5 (f 4))]
     @step @racket[(+ 10 5 8)]
     @step @racket[23]

Beachten Sie, dass während der gesamten Reduktion der Kontext sowohl @racket[f] als auch @racket[g] enthält, daher ist
ist kein Problem, dass im Body von @racket[g] die Funktion  @racket[f] aufgerufen wird, obwohl  @racket[f] erst @italic{nach}
@racket[g] definiert wird. Die Auswertung des Programms

@racketblock[
(define (g y z) (+ (f y) y (f z)))
(g (+ 2 3) 4)
(define (f x) (* x 2))                                  
]
schlägt hingegen fehl, weil der Kontext bei der Auswertung von @racket[(g (+ 2 3) 4)] nur @racket[g] aber nicht @racket[f] enthält.


@section{Konditionale Ausdrücke}

In der Animation aus dem letzten Abschnitt verschwindet die Rakete einfach irgendwann nach unten aus dem Bild.
Wie können wir es erreichen, dass die Rakete stattdessen auf dem Boden der Szene "landet"?

@subsection{Motivation}
Offensichtlich benötigen wir hierzu in unserem Programm eine Fallunterscheidung. Fallunterscheidungen
kennen sie aus zahlreichen Beispielen des realen Lebens. Beispielsweise ist das Bewertungsschema für
eine Klausur, welches jeder Punktzahl eine Note zuordnet, eine Funktion die die unterschiedlichen
Grenzen für die resultierenden Noten voneinander unterscheidet. In BSL können wir ein Notenschema, bei
dem man mindestens 90 Punkte für eine 1 benötigt und alle 10 Punkte darunter eine Note heruntergegangen wird,
wie folgt definieren:

@block[
(define (note punkte) 
  (cond
    [(>= punkte 90) 1]
    [(>= punkte 80) 2]
    [(>= punkte 70) 3]
    [(>= punkte 60) 4]
    [(>= punkte 50) 5]
    [(< punkte 50) 6]))
]

Einige Beispiele für die Benutzung des Notenschemas:
@ex[
(note 95)
(note 73)
(note 24)]

@section[#:tag "kondsem"]{Bedeutung konditionaler Ausdrücke}    
Im allgemeinen Fall sieht ein konditionaler Ausdruck wie folgt aus:

@racketblock[
    (cond
      [ConditionExpression1 ResultExpression1]
      [ConditionExpression2 ResultExpression2]
      ....
      [ConditionexpressionN ResultExpressionN])
]

Ein konditionaler Ausdruck startet also mit einer öffnenden Klammer und dem Schlüsselwort @racket[cond].
Danach folgen beliebig viele Zeilen, von denen jede zwei Ausdrücke beinhaltet. Der linke Ausdruck
wird die @italic{Bedingung} oder @italic{Kondition} und der rechte das @italic{Resultat} genannt.

Ein @racket[cond] Ausdruck wird wie folgt ausgewertet. DrRacket wertet zunächst die erste Bedingung
@racket[ConditionExpression1] aus. Ergibt diese Auswertung den Wert @racket[true], so ist der Wert
des gesamten @racket[cond] Ausdrucks der Wert von @racket[ResultExpression1]. Ergibt diese Auswertung
hingegen den Wert @racket[false], so wird mit der zweiten Zeile fortgefahren und genau so verfahren
wie mit der ersten Zeile. Wenn es keine nächste Zeile mehr gibt --- also alle Bedingungen zu @racket[false] ausgewertet wurden ---
so wird mit einer Fehlermeldung abgebrochen. Ebenso ist es ein Fehler, wenn die Auswertung einer Bedingung nicht
@racket[true] oder @racket[false] ergibt:

@ex[(cond [(< 5 3) 77]
          [(> 2 9) 88])
    (cond [(+ 2 3) 4])]

Die Reduktionsregeln für BSL müssen wir zur Berücksichtigung konditionaler Audrücke um folgende beiden Regeln ergänzen:

@racket[(cond [false (unsyntax @e)]
      [(unsyntax @e2) (unsyntax @e3)]
      ....
      [(unsyntax @eN-1) (unsyntax @eN)])]
      @step    
@racket[(cond [(unsyntax @e2) (unsyntax @e3)]
      ....
      [(unsyntax @eN-1) (unsyntax @eN)])]

und 
      
@racket[(cond [true (unsyntax @e)]
      [(unsyntax @e2) (unsyntax @e3)]
      ....
      [(unsyntax @eN-1) (unsyntax @eN)]) ]
      @step    
     @e 

Außerdem ergänzen wir die Auswertungspositionen, die in der Kongruenzregel verwendet werden können wie folgt:
In einem Ausdruck der Form 

@racketblock[
(cond [(unsyntax @e0) (unsyntax @e1)]
      [(unsyntax @e2) (unsyntax @e3)]
      ....
      [(unsyntax @eN-1) (unsyntax @eN)])]

ist der Ausdruck @e0 in einer Auswertungsposition, aber nicht @e1,...,@eN .
      
Beispiel: Betrachten wir den Aufruf @racket[(note 83)] in dem Beispiel oben. Dann
@racket[(note 83)] @step  @racket[(cond
    [(>= 83 90) 1]
    [(>= 83 80) 2]
    ...)]
    @step
@racket[(cond
    [false 1]
    [(>= 83 80) 2]
    ...)]
    @step
@racket[(cond
    [(>= 83 80) 2]
    ...)]
    @step
@racket[(cond
    [true 2]
    ...)]
    @step
    @racket[2]


@subsection{Beispiel}      
Zurück zu unserer Rakete. Offensichtlich müssen wir hier zwei Fälle unterscheiden. Während die Rakete noch oberhalb
des Bodens der Szene ist, soll sie wie gehabt sinken. Wenn die Rakete allerdings bereits auf dem Boden angekommen ist,
soll die Rakete nicht mehr weiter sinken. 

Da die Szene 100 Pixel hoch ist, können wir die Fälle unterscheiden, dass die aktuelle Höhe kleiner oder gleich 100
ist und dass die aktuelle Höhe größer als 100 ist.

@margin-note{Für die Varianten der @racket[create-rocket-scence] Funktion verwenden wir die Namenskonvention dass wir
den Varianten die Suffixe @racket[-v2], @racket[-v3] usw. geben.}  
                                                                 
                                                                 
@racketblock[
(define (create-rocket-scene-v2 height)
  (cond
    [(<= height 100)
     (place-image (unsyntax @ev[rocket]) 50 height (empty-scene 100 100))]
    [(> height 100)
     (place-image (unsyntax @ev[rocket]) 50 100 (empty-scene 100 100))]))]

     
@subsection{Etwas syntaktischer Zucker...}   

Zwei Spezialfälle konditionaler Ausdrücke sind so häufig, dass es in BSL eine eigene Syntax dafür gibt, die für diese
Spezialfälle optimiert ist.

Der erste Spezialfall ist der, dass man einen Zweig der Kondition haben möchte, der immer dann genommen wird, wenn alle anderen
Zweige nicht anwendbar sind. In diesem Fall kann man statt der Kondition das Schlüsselword @racket[else] verwenden. Das Beispiel von oben könnten wir daher auch so formulieren:
 
@racketblock[
(define (note punkte) 
  (cond
    [(>= punkte 90) 1]
    [(>= punkte 80) 2]
    [(>= punkte 70) 3]
    [(>= punkte 60) 4]
    [(>= punkte 50) 5]
    [else 6]))
] 

Die @racket[else] Klausel allerdings darf nur im letzten Zweig eines @racket[cond] Ausdrucks verwendet werden:

@ex[
(cond [(> 3 2) 5]
      [else 7]
      [(< 2 1) 13])]

      
Der @racket[else] Zwei ist äquivalent zu einem Zweig mit der immer erfüllten Bedingung @racket[true], daher ist im Allgemeinen Fall
die Bedeutung von

@racketblock[
(cond [(unsyntax @e0) (unsyntax @e1)]
      [(unsyntax @e2) (unsyntax @e3)]
      ....
      [else (unsyntax @eN)])]
      
definiert als die Bedeutung von      

@racketblock[
(cond [(unsyntax @e0) (unsyntax @e1)]
      [(unsyntax @e2) (unsyntax @e3)]
      ....
      [true (unsyntax @eN)])]
 
Wir geben also in diesem Fall keine Reduktionsregeln für dieses Sprachkonstrukt an, sondern stattdessen eine Transformation, die
die Bedeutung transformiert. Wenn Sprachkonstrukte "nichts neues" hinzufügen sondern lediglich eine Abkürzung für eine bestimmte
Benutzung bestehender Sprachkonstrukte sind, so nennt man solche Sprachkonstrukte auch @italic{syntaktischen Zucker}. 

Ein anderer Spezialfall konditionaler Ausdrücke ist der, dass es nur eine Bedingung gibt, die überprüft werden soll, und je nachdem ob diese Bedingung wahr oder falsch ist soll ein anderer Ausdruck ausgewählt werden. Für diesen Fall gibt es das @racket[if] Konstrukt.


Beispiel:

@block[
(define (aggregatzustand temperatur) 
  (if (< temperatur 0) "gefroren" "flüssig"))]
@ex[(aggregatzustand -5)]

Im Allgemeinen hat ein @racket[if] Ausdruck folgende Form:

@racketblock[(if CondExpression ThenExpression ElseExpression)]

Ein @racket[if] Ausdruck ist syntaktischer Zucker; die Bedeutung wird durch die Transformation in diesen Ausdruck festgelegt:

@racketblock[
(cond [CondExpression ThenExpression]
      [else ElseExpression])]

Im Allgemeinen eignet sich @racket[if] für Situationen, in denen wir so etwas wie "entweder das eine oder das andere" sagen wollen. Die @racket[cond] Ausdrücke eignen sich dann, wenn man mehr als zwei Situationen unterscheiden möchten.

Obwohl es zunächst so aussieht, als sei @racket[if] ein Spezialfall von @racket[cond], kann man allerdings auch jeden @racket[cond] 
Ausdruck durch einen geschachtelten @racket[if] Ausdruck ersetzen. Beispielsweise kann die Funktion von oben auch so geschrieben werden:

@racketblock[
(define (note punkte) 
  (if (>= punkte 90) 
      1
     (if (>= punkte 80) 
         2
         (if (>= punkte 70) 
             3
             (if (>= punkte 60) 
                 4
                 (if (>= punkte 50) 
                     5
                     6))))))]
                     
In solchen Fällen ist offensichtlich das @racket[cond] Konstrukt besser geeignet, weil man keine tief geschachtelten Ausdrücke benötigt. Dennoch kann man festhalten, dass @racket[cond] und @racket[if] gleichmächtig sind, weil das eine in das andere
so transformiert werden kann, dass die Bedeutung gleich bleibt.

      
     

@subsection{Auswertung konditionaler Ausdrücke}
In @secref{kondsem} haben wir definiert, dass in einem konditionalen Ausdruck

@racketblock[
(cond [(unsyntax @e0) (unsyntax @e1)]
      [(unsyntax @e2) (unsyntax @e3)]
      ....
      [(unsyntax @eN-1) (unsyntax @eN)])]

nur der Ausdruck @e0 in einer Auswertungsposition ist, aber nicht @e1,...,@eN . Wieso diese Einschränkung --- wieso nicht auch
die Auswertung von @e1,...,@eN erlauben? Betrachten Sie folgendes Beispiel:

@racketblock[
(cond [(= 5 7) (/ 1 0)]
      [(= 3 3) 42]
      [(/ 1 0) 17])]

Gemäß unserer Auswertungsregeln gilt:      

@racketblock[
(cond [(= 5 7) (/ 1 0)]
      [(= 3 3) 42]
      [(/ 1 0) 17])]

@(hspace 5) @step

@racketblock[
(cond [false (/ 1 0)]
      [(= 3 3) 42]
      [(/ 1 0) 17])]

   @(hspace 5) @step

@racketblock[
(cond [(= 3 3) 42]
      [(/ 1 0) 17])]

   @(hspace 5) @step
      
@racketblock[
(cond [true 42]
      [(/ 1 0) 17])]

   @(hspace 5) @step
      
 @(hspace 2) @racketblock[42]

Wenn es erlaubt wäre, auch auf den anderen Positionen auszuwerten, müssten wir gemäß unserer Regeln die Berechnung in dem Beispiel mit einem Fehler abbrechen, sobald wir einen der @racket[(/ 1 0)] Ausdrücke auswerten. Gemäß der Terminologie aus 
@secref{semanticsofexpressions} geht uns die Konfluenz-Eigenschaft verloren und der Wert eins Ausdrucks ist nicht mehr eindeutig.

Das Beispiel oben ist sehr künstlich, aber wir werden später sehen, dass konditionale Ausdrücke häufig verwendet werden, um die Terminierung einer Funktion sicherzustellen, und dafür ist es essentiell, dass nicht auf allen Positionen ausgewertet werden darf.
Operatoren wie @racket[cond], bei denen die Auswertung beliebiger Argumente nicht erlaubt ist, nennt man auch @italic{nicht-strikt}.
Normale Funktionen, bei deren Aufruf alle Argumente ausgewertet werden bevor die Funktion angewendet wird, nennt man hingegen @italic{strikt}. 



     
@section{Definition von Variablen}

Wenn wir uns @racket[(animate create-rocket-scene-v2)] anschauen, stellen wir fest, dass die Animation noch immer nicht befriedigend
ist, denn die Rakete versinkt halb im Boden. Der Grund dafür ist, dass @racket[place-image] das Zentrum des Bildes an dem vorgegebenen
Punkt plaziert. Damit die Rakete sauber auf dem Boden landet, muss das Zentrum also überhalb des Bodens sein. Mit etwas Überlegung
wird schnell klar, dass die Rakete nur bis zu der Höhe

@racketblock[ (- 100 (/ (image-height (unsyntax @ev[rocket])) 2))]

absinken sollte. Das bedeutet, dass wir unsere @racket[create-rocket-scene-v2] Funktion wie folgt modifizieren müssen:

@racketblock[
(define (create-rocket-scene-v3 height)
  (cond
    [(<= height (- 100 (/ (image-height  (unsyntax @ev[rocket])) 2)))
     (place-image  (unsyntax @ev[rocket]) 50 height (empty-scene 100 100))]
    [(> height (- 100 (/ (image-height  (unsyntax @ev[rocket])) 2)))
     (place-image  (unsyntax @ev[rocket]) 50 (- 100 (/ (image-height  (unsyntax @ev[rocket])) 2))
                  (empty-scene 100 100))]))]

@section{DRY: Don't Repeat Yourself!}

Ein Aufruf von @racket[(animate create-rocket-scene-v3)] illustriert, dass die Rakete nun wie von uns gewünscht landet.
Allerdings ist offensichtlich, dass @racket[create-rocket-scene-v3] gegen das im Abschnitt @secref{redundanz} angesprochene Prinzip
verstößt, dass gute Programme keine Redundanz enthalten. Im Programmiererjargon wird dieses Prinzip auch häufig
DRY-Prinzip --- Don't Repeat Yourself --- genannt. 

@subsection{DRY durch Variablendefinitionen}

@margin-note{Konstante Werte wie @racket[100] in Programmtexten werden von Programmierern häufig abfällig als @italic{magic numbers} bezeichnet.}
Eine Art von Redundanz, die in @racket[create-rocket-scene-v3] auftritt, ist die, dass die Höhe und Breite der Szene 
sehr häufig wiederholt wird. Stellen Sie sich vor, sie möchten statt einer 100 mal 100 Szene eine 200 mal 400 Szene haben.
Zu diesem Zweck müssen Sie alle Vorkommen der alten Höhe und Breite finden, jeweils herausfinden ob sie für die Breite
oder die Höhe oder noch etwas anderes stehen (deshalb ist das Problem auch nicht mit maschineller Textersetzung lösbar), 
und je nachdem durch den neuen Wert ersetzen. Der Aufwand ist bei @racket[create-rocket-scene-v3] zwar noch überschaubar, 
aber wenn Sie Programme mit vielen tausend Codezeilen betrachten
wird schnell klar, dass dies ein großes Problem ist. 

Idealerweise sollte die Beziehung zwischen den Anforderungen an ein Programm und dem Programmtext @italic{stetig} sein: 
Ein kleiner Änderungswunsch an den Anforderungen für ein Programm sollte auch nur eine kleine Änderung am Programmtext erfordern.
In unserem konkreten Beispiel können wir dieses Problem mit @racket[define] lösen. Mit @racket[define] können nämlich
nicht nur Funktionen, sondern auch @italic{Variablen} definiert werden. Beispielsweise können wir in unser Programm diese
Definition hineinschreiben:

@margin-note{Für die Bedeutung des Programms spielt es keine Rolle dass der Name der Variablen nur aus Großbuchstaben besteht.
Dies ist lediglich eine Namenskonvention, anhand derer Programmierer leicht erkennen können, welche Namen sich auf Konstanten beziehen.}
@racketblock[(define HEIGHT 100)]

Die Bedeutung einer solchen Definition ist, dass im Rest des Programms @racket[HEIGHT] ein gültiger Ausdruck ist, der bei Auswertung den
Wert @racket[100] hat. Wenn wir im Programm alle Vorkommen von @racket[100], die für die Höhe stehen, durch @racket[HEIGHT] ersetzen, und
das gleiche für @racket[WIDTH] machen, erhalten wir diese Variante von @racket[create-rocket-scene]:

@racketblock[
(define WIDTH 100)             
(define HEIGHT 100)
(define (create-rocket-scene-v4 height)
  (cond
    [(<= height (- HEIGHT (/ (image-height  (unsyntax @ev[rocket])) 2)))
     (place-image  (unsyntax @ev[rocket]) 50 height (empty-scene WIDTH HEIGHT))]
    [(> height (- HEIGHT (/ (image-height  (unsyntax @ev[rocket])) 2)))
     (place-image  (unsyntax @ev[rocket]) 50 (- HEIGHT (/ (image-height  (unsyntax @ev[rocket])) 2))
                  (empty-scene WIDTH HEIGHT))]))]

Testen Sie durch @racket[(animate create-rocket-scene-v4)] dass das Programm weiterhin funktioniert. Experimentieren Sie mit anderen Werten für 
@racket[WIDTH] und @racket[HEIGHT] um zu sehen, dass diese kleine Änderung wirklich genügt um die Größe der Szene zu ändern.

Im Programmiererjargon nennen sich Programmänderungen, die die Struktur des Programms verändern ohne sein Verhalten zu verändern, @italic{Refactorings}.
Häufig werden Refactorings durchgeführt um die Wartbarkeit, Lesbarkeit, oder Erweiterbarkeit des Programms zu verbessern. In unserem Fall haben wir
sowohl Wartbarkeit als auch Lesbarkeit durch dieses Refactoring verbessert. Die verbesserte Wartbarkeit haben wir bereits illustriert; die verbesserte
Lesbarkeit rührt daher, dass wir an Namen wie @racket[WIDTH] die Bedeutung der Variablen ablesen können, während wir bei magic numbers wie @racket[100]
diese Bedeutung erst durch genaue Analyse des Programms herausfinden müssen (im Programmiererjargon auch @italic{reverse engineering} genannt).

Es spielt übrigens keine Rolle, ob die Definitionen der Variablen oberhalb oder unterhalb der @racket[create-rocket-scene] Definition stehen. Die Variablen
sind innerhalb der gesamten Programmdatei sichtbar. Man sagt, die Variablen haben @italic{globalen Scope}. Um die Definitionen der Variablen nicht
im Programmtext suchen zu müssen, ist es sinnvoll, diese immer an der gleichen Stelle zu definieren. In vielen Programmiersprachen gibt es die Konvention,
dass Variablendefinitionen immer am Anfang des Programmtextes stehen, daher werden auch wir uns an diese Konvention halten.

Allerdings verstößt @racket[create-rocket-scene-v4] immer noch gegen das DRY-Prinzip. Beispielsweise kommt der Ausdruck 

@racketblock[(- HEIGHT (/ (image-height  (unsyntax @ev[rocket])) 2))]

mehrfach vor. Diese Redundanz kann ebenfalls mit @racket[define] beseitigt werden, denn der Wert, mit dem eine Variable belegt wird, kann durch
einen beliebig komplexen Ausdruck beschrieben werden. Im Allgemeinen haben Variablendefinitionen die folgende Form: 

@racketblock[(define VARIABLENAME VARIABLEExpression)]
    
Im vorherigen Beispiel können wir die Variable zum Beispiel @racket[ROCKET-CENTER-TO-BOTTOM] nennen. Beachten Sie,
wie durch die Wahl guter Namen die Bedeutung des Programms viel offensichtlicher wird. Ohne diesen Namen müssten wir jedesmal, wenn wir 
den komplexen Ausdruck oben lesen und verstehen wollen, wieder herausfinden, dass hier die gewünschte Distanz des Zentrums der Rakete zum Boden
berechnet wird. 

@margin-note{In der Tradition der Familie von Programmiersprachen, aus der BSL stammt, ist es üblich, die englische Aussprache der Buchstaben
des Alphabets zu verwenden um Namen abzukürzen.  @racket[MTSCN] spricht man daher "empty scene".}
Das gleiche können wir mit dem mehrfach vorkommenden Ausdruck @racket[(empty-scene WIDTH HEIGHT)] machen. Wir geben ihm den Namen @racket[MTSCN].

Auch die Zahl @racket[50] im Programmtext ist eine @italic{magic number}, allerdings hat sie eine andere Qualität als @racket[WIDTH] und @racket[HEIGHT]:
Sie ist nämlich abhängig von dem Wert anderer Konstanten, in diesem Fall @racket[WIDTH]. Da diese Konstante für die horizontale Mitte steht, 
definieren wir sie als @racket[(define MIDDLE (/ WIDTH 2))].

Die letzte Art der Redundanz, die nun noch vorkommt, ist, dass die Rakete selber mehrfach im Programmtext vorkommt. Die Rakete ist zwar kein Zahlenliteral
und daher keine @italic{magic number}, aber ein @italic{magic image} --- mit genau den gleichen Nachteilen wie @italic{magic numbers}.
Daher definieren wir auch für das Bild eine Variable @racket[ROCKET]. Das Programm, welches alle diese @italic{Refactorings} beinhaltet,
sieht nun so aus:

@racketblock[
(define WIDTH 100)             
(define HEIGHT 100)
(define MIDDLE (/ WIDTH 2))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define ROCKET (unsyntax @ev[rocket]))
(define ROCKET-CENTER-TO-BOTTOM (- HEIGHT (/ (image-height ROCKET) 2)))
(define (create-rocket-scene-v5 height)
  (cond
    [(<= height ROCKET-CENTER-TO-BOTTOM)
     (place-image ROCKET MIDDLE height MTSCN)]
    [(> height ROCKET-CENTER-TO-BOTTOM)
     (place-image ROCKET MIDDLE ROCKET-CENTER-TO-BOTTOM MTSCN)]))]

@subsection[#:tag "dryredux"]{DRY Redux}
Halt! Auch @racket[create-rocket-scene-v5] verstößt noch gegen das DRY-Prinzip. Allerdings werden wir die verbliebenen Redundanzen 
nicht durch Funktions- oder Variablendefinitionen eliminieren.

Eine Redundanz ist die, dass die Kondition @racket[(> height ROCKET-CENTER-TO-BOTTOM)] genau dann wahr ist wenn 
@racket[(<= height ROCKET-CENTER-TO-BOTTOM)] falsch ist. Diese Information steht jedoch nicht direkt im Programmtext; stattdessen 
wird die Kondition wiederholt und negiert. Eine Möglichkeit wäre, eine Funktion zu schreiben, die diese Kondition abhängig vom 
@racket[height] Parameter berechnet und diese Funktion dann in beiden Zweigen der Kondition aufzurufen (und einmal zu negieren). 
In diesem Fall bietet sich allerdings eine einfachere Lösung an, nämlich 
statt @racket[cond] @racket[if] zu verwenden. 
Damit können wir diese Redundanz eliminieren:

@racketblock[
(define WIDTH 100)             
(define HEIGHT 100)
(define MIDDLE (/ WIDTH 2))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define ROCKET (unsyntax @ev[rocket]))
(define ROCKET-CENTER-TO-BOTTOM (- HEIGHT (/ (image-height ROCKET) 2)))
(define (create-rocket-scene-v6 height)
  (if
    (<= height ROCKET-CENTER-TO-BOTTOM)
    (place-image ROCKET MIDDLE height MTSCN)
    (place-image ROCKET MIDDLE ROCKET-CENTER-TO-BOTTOM MTSCN)))]

Die letzte Redundanz, die wir in @racket[create-rocket-scene-v6] eliminieren wollen, ist die, dass die beiden Aufrufe von 
@racket[place-image] bis auf einen Parameter identisch sind. Falls in einem konditionalen Ausdruck die Bodies aller Zweige
bis auf einen Unterausdruck identisch sind, können wir die Kondition in den Audruck @italic{hineinziehen}, und zwar so:

@racketblock[
(define WIDTH 100)             
(define HEIGHT 100)
(define MIDDLE (/ WIDTH 2))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define ROCKET (unsyntax @ev[rocket]))
(define ROCKET-CENTER-TO-BOTTOM (- HEIGHT (/ (image-height ROCKET) 2)))
(define (create-rocket-scene-v7 height)
  (place-image 
    ROCKET 
    MIDDLE 
    (if (<= height ROCKET-CENTER-TO-BOTTOM) 
        height 
        ROCKET-CENTER-TO-BOTTOM) MTSCN))]

@section{Bedeutung von Funktions- und Variablendefinitionen}

Wir haben oben gesagt, dass es keine Rolle spielt, ob die Variablen oberhalb oder unterhalb der Funktionsdefinition definiert werden.
Allerdings spielt es sehr wohl eine Rolle, in welcher Reihenfolge diese Variablen definiert werden. Wie sie sehen, verwenden 
einige der Variablendefinitionen andere Variablen. Zum Beispiel verwendet die Definition von @racket[MTSCN] @racket[WIDTH]. Dies ist
auch sinnvoll, denn andernfalls hätte man weiterhin die Redundanz die man eigentlich eliminieren wollte. 

DrRacket wertet ein Programm von oben nach unten aus. Wenn es auf eine Variablendefinition trifft, so wird sofort der Wert
des Ausdrucks, an den der Name gebunden werden soll (die @racket[VARIABLEExpression]), berechnet. Wenn in diesem Ausdruck eine Variable vorkommt, die DrRacket
noch nicht kennt, so gibt es einen Fehler:

@ex[(define A (+ B 1))
    (define B 42)]

Daher dürfen in Variablendefinitionen nur solche Variablen (und Funktionen) verwendet werden, die oberhalb der Definition bereits definiert wurden.

Tritt dieses Problem auch bei Funktionen auf? Hier ein Versuch:

@block[(define (add6 x) (add3 (add3 x))) 
    (define (add3 x) (+ x 3))]
@ex[(add6 5)]

Der Grund, wieso die Reihenfolge von Funktionsdefinitionen nicht wichtig ist, ist der, dass DrRacket bei Auswertung einer Funktionsdefinition lediglich
registriert, dass es eine neue Funktion des angegebenen Namens gibt, jedoch im Unterschied zu Variablendefinitionen die @racket[BodyExpression] der Funktion nicht auswertet.

Etwas formaler können wir die Bedeutung von Programmen mit Funktions- und Variablendefinitionen so definieren:

@itemize[
 @item{Ein Programm ist eine Sequenz von Ausdrücken, Variablendefinitionen und Funktionsdefinitionen. Diese können in beliebiger Reihenfolge auftreten.}
 @item{Ein Kontext ist eine Menge von Funktions- und Variablendefinitionen. Der Kontext ist am Anfang der Programmausführung leer.}
 @item{Ein Programm wird von links nach rechts (bzw. oben nach unten) ausgewertet. Hier sind nun drei Fälle zu unterscheiden.
     @itemize[
       @item{Ist das nächste Programmelement ein Ausdruck, so wird dieser gemäß der bekannten Reduktionsregeln im aktuellen Kontext zu einem Wert ausgewertet.
             Für die Auswertung von Variablen gilt hierbei @racket[x] @step @racket[v], falls der Kontext die Definition @racket[(define x v)] enthält.}
       @item{Ist das nächste Programmelement eine Funktionsdefinition, so wird diese Funktionsdefinition dem aktuellen Kontext hinzugefügt.}
       @item{Ist das nächste Programmelement eine Variablendefinition @racket[(define VARIABLENAME VARIABLEExpression)], 
            so wird @racket[VARIABLEExpression] im aktuellen Kontext zu einem Wert @racket[v] ausgewertet und zum Kontext die 
            Definition @racket[(define VARIABLENAME v)] hinzugefügt.}]}]

Der aktuelle Kontext wird im Stepper von DrRacket angezeigt, und zwar als die Menge der Funktions- und Variablendefinitionen, die oberhalb des aktuell zu reduzierenden 
Ausdrucks stehen. Bitte benutzen Sie den Stepper um die Reduktion des folgenden Programms zu visualisieren. Am besten versuchen Sie erst auf einem Blatt 
Papier vorherzusagen, welches die Reduktionsschritte sein werden und kontrollieren dann mit dem Stepper.

@racketblock[
(define WIDTH 100)             
(define HEIGHT 100)
(define MIDDLE (/ WIDTH 2))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define ROCKET (unsyntax @ev[rocket]))
(define ROCKET-CENTER-TO-BOTTOM (- HEIGHT (/ (image-height ROCKET) 2)))
(define (create-rocket-scene-v7 height)
  (place-image 
    ROCKET 
    MIDDLE 
    (if (<= height ROCKET-CENTER-TO-BOTTOM) 
        height 
        ROCKET-CENTER-TO-BOTTOM) MTSCN))
(create-rocket-scene-v7 42)]

Randnotiz: Zählen Sie einmal die Anzahl der Reduktionsschritte, die Sie pro Aufruf von @racket[create-rocket-scene-7] zusätzlich benötigen (also wenn Sie
noch weitere Aufrufe zum Programm hinzufügen). Wieviele zusätzliche Schritte benötigen Sie, wenn Sie stattdessen @racket[create-rocket-scene-v2] verwenden? 
Wie kommt es zu den Unterschieden und was bedeuten sie?

@section{Kommentare}

Ein Programm kann neben dem eigentlichen Programmtext auch Kommentare enthalten. Kommentare haben keinen Einfluss auf die Bedeutung eines Programms und dienen nur 
der besseren Lesbarkeit eines Programms. Kommentare werden durch ein Semikolon eingeleitet; alles was in einer Zeile nach einem Semikolon steht ist ein Kommentar.
Wir werden später mehr dazu sagen, wo, wie und wie ausführlich Programme kommentiert werden sollten.

@section{Programmieren ist mehr als das Regelverstehen!}

Ein guter Schachspieler muss die Regeln des Schachspiels verstehen. Aber nicht jeder, der die Schachregeln versteht ist auch ein guter Schachspieler.
Die Schachregeln verraten nichts darüber, wie man eine gute Partie Schach spielt. Das Verstehen der Regeln ist nur ein erster kleiner Schritt auf dem Weg dahin.

Jeder Programmierer muss die "Mechanik" der Programmiersprache beherrschen: Was gibt es für Konstrukte in der Programmiersprache und was bedeuten sie?
Was gibt es für vordefinierte Funktionen und Bibliotheken?

Trotzdem ist man dann noch lange kein guter Programmierer. Viele Anfängerbücher (und leider auch viele Anfängerkurse an Universitäten) 
fürs Programmieren sind so gehalten, dass sie sich @italic{nur} auf diese mechanischen Aspekte der Programmierung fokussieren. Noch schlimmer, sie
lernen nicht einmal, was genau ihre Programme bedeuten, sondern sie lernen im Wesentlichen nur die Syntax einer Programmiersprache und einige ihrer
Bibliotheken.

Das liegt daran, dass einige Programmiersprachen, die in Anfängerkursen verwendet werden, so kompliziert sind, dass man den Großteil des Semesters damit verbringt, 
nur die Syntax der Sprache zu lernen. Unsere Sprache, BSL, ist so einfach, dass Sie bereits jetzt die Mechanik dieser Sprache verstehen --- ein Grund
dafür, wieso wir uns für diese Sprache entschieden haben.
Wir werden zwar noch einige weitere Sprachfeatures einführen, aber im größten Teil dieser Vorlesung geht es um den interessanteren Teil der
Programmierung: Wie kommt man von einer Problembeschreibung systematisch zu einem guten Programm? Was für Arten der Abstraktion gibt es und 
wie kann man sie einsetzen? Wie gehe ich mit Fehlern um? Wie strukturiere ich mein Programm so, dass es lesbar, wartbar und wiederverwendbar ist?
Wie kann ich die Komplexität sehr großer Programme beherrschen?

Die Antworten, die wir auf diese Fragen geben, werden Ihnen in @italic{allen} Programmiersprachen, die sie verwenden werden, nutzen.
Darum wird es in diesem Kurs gehen.