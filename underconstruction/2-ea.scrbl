#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")

   
@title[#:version ""]{Funktionen und Animationen}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 1}

@section{Funktionsdefinitionen}
Die Programme, die Sie bisher geschrieben haben, waren im Wesentlichen
Berechnungen, wie man sie auch auf einem Taschenrechner durchführen könnte ---
allerdings mit dem großen Unterschied dass BSL die Arithmetik von vielen Arten von Werten
beherrscht und nicht nur die der Zahlen. Bei den Operatoren, die Sie in
diesen Ausdrücken verwenden können, können Sie aus einer festen Menge vordefinierter
Operatoren wählen.

Eine echte Programmiersprache unterscheidet sich von einem Taschenrechner dadurch,
dass Sie selber, auf Basis der bereits bestehenden Operatoren, neue Operatoren definieren können
und danach in Ausdrücken und Definitionen neuer Operatoren benutzen können.

In BSL nennt man Operatoren auch @italic{Funktionen}. Hier ist die Definition einer Funktion
die den Durchschnitt von zwei Zahlen berechnet.

@block[(define (average x y) (/ (+ x y) 2))]

Wenn diese Funktionsdefinition in den Definitionsbereich geschrieben und dann auf "Start" gedrückt wird, 
passiert --- nichts. Eine Funktionsdefinition ist @italic{kein} Ausdruck. Der Effekt dieser Definition
ist der, dass die neue Funktion nun als Operator in Ausdrücken verwendet werden kann:

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

Eine Funktionsdefinition startet also mit dem Schlüsselwort @racket[define].
Der einzige Zweck dieses Schlüsselworts ist der, Funktionsdefinitionen von
Ausdrücken unterscheiden zu können. Insbesondere darf es also keine Operationen
geben, die @racket[define] heißen. @racket[FunctionName] ist der name der Funktion.
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
das Benutzen einer fest eingebauten (@italic{primitiven}) Funktion/Operation. 
Dies ist kein Zufall. Dadurch, dass man nicht sehen kann, ob man gerade eine primitive Funktion
oder eine benutzerdefinierte Funktion aufruft, ist es leichter, die Programmiersprache
selber zu erweitern oder zu verändern. Zum Beispiel kann aus einer primitiven Operation
eine benutzerdefinierte Funktion gemacht werden, oder ein Programmierer kann Erweiterungen
definieren die so aussehen, als wäre die Sprache um primitive Funktionen erweitert worden.

@section{Funktionen die Bilder produzieren}

Selbstverständlich können in BSL Funktionen nicht nur Zahlen sondern beliebige Werte als Eingabe
bekommen oder als Ausgabe zurückliefern.

@racketblock[
(define (create-rocket-scene height)
  (place-image (unsyntax @ev[rocket]) 50 height (empty-scene 100 100)))]