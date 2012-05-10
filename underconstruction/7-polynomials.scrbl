#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Polynome}

@margin-note{Dieser Teil des Skripts basiert teilweise auf [HTDP/2e] Kapitel 2.6}

In den letzten beiden Kapiteln haben wir zwei neue Arten von Datendefinitionen
kennengelernt: Summentypen (in Form von Aufzählungen und Intervallen), 
mit denen man zwischen verschiedenen
Möglichkeiten auswählen kann. Produkttypen (in Form von Datendefinitionen für Strukturen), 
mit denen man Daten zusammenfassen und gruppieren kann. 

In vielen Fällen ist es sinnvoll, Summen und Produkte miteinander zu kombinieren, also zum 
Beispiel Summentypen zu definieren, bei denen einige Alternativen einen Produkttyp haben; oder Produkttypen
bei denen einige Felder Summentypen haben.

@margin-note{Die Analogie zu Polynomen in der Algebra geht allerdings noch viel weiter --- wir werden später noch etwas mehr dazu sagen.}
In der Algebra nennt man Ausdrücke, bei denen Variablen und Konstanten durch Summen und Produkte
kombiniert werden, @italic{Polynome}. In Analogie dazu werden wir von @italic{Polynomtypen} reden.

@section{Beispiel: Kollisionen zwischen Shapes}

Nehmen Sie an, sie möchten ein Computerspiel programmieren, in dem Spielfiguren in unterschiedlichen
geometrischen Formen vorkommen, zum Beispiel Kreise und Rechtecke. Diese können wir beispielsweise
so modellieren: @margin-note{Wir wählen den Namen @racket[gcircle] (für @italic{geometric} circle) um
keinen Namenskonflikt mit der @racket[circle] Funktion aus dem image.ss Teachpack zu haben.}}

@#reader scribble/comment-reader
(racketblock
(define-struct gcircle (center radius))
; A GCircle is (make-gcircle Posn Number)
; interp. the geometrical representation of a circle

(define-struct grectangle (corner-ul corner-dr))
; A GRrectangle is (make-grectangle Posn Posn)
; interp. the geometrical representation of a rectangle
; where corner-ul is the upper left corner and corner-dr the down right corner
)

Nehmen wir der Einfachheit halber an, es gibt nur eine dieser Spielfiguren, aber welche Form sie hat, 
ändert sich im Laufe des Spiels. Wie müsste die Definition des @racket[WorldState] bei Benutzung 
des "universe" Teachpacks sein? Offensichtlich ist es sinnvoll, einen Datentyp zu definieren,
der @racket[gcircle] und @racket[grectangle] in Form einer Summe zusammenfasst:

@#reader scribble/comment-reader
(racketblock
; A Shape is either:
; - a GCircle 
; - a GRectangle
; interp. a geometrical shape, either a circle or a rectangle
)

@#reader scribble/comment-reader
(racketblock
; Shape Posn -> Boolean
; Determines whether a point is inside a shape
(define (point-inside shape point)
  (cond [(gcircle? shape) (point-inside-circle shape point)]
        [(grectangle? shape) (point-inside-rectangle shape point)]))
)

@margin-note{Ergänzen Sie die Definitionen für @racket[vector-length] und @racket[posn-]!}
@#reader scribble/comment-reader
(racketblock
(define (point-inside-circle circle point)
  (<= (vector-length (posn- (gcircle-center circle) point)) (gcircle-radius circle)))

(define (point-inside-rectangle rectangle point)
  (and
   (<= (posn-x (grectangle-corner-ul rectangle))
       (posn-x point)
       (posn-x (grectangle-corner-dr rectangle)))
   (<= (posn-y (grectangle-corner-ul rectangle))
       (posn-y point)
       (posn-y (grectangle-corner-dr rectangle)))))
)


@#reader scribble/comment-reader
(racketblock
; Shape Shape -> Boolean
; determines whether shape1 overlaps with shape2
(define (collides shape1 shape2)
  (cond [(and (gcircle? shape1) (gcircle? shape2)) (collides-circle-circle shape1 shape2)]
        [(and (grectangle? shape1) (grectangle? shape2))
         (collides-rectangle-rectangle shape1 shape2)]
        [(and (grectangle? shape1) (gcircle? shape2))
         (collides-rectangle-circle shape1 shape2)]
        [(and (gcircle? shape1) (grectangle? shape2))
         (collides-rectangle-circle shape2 shape1)]))
)

@margin-note{Wenn Sie Spass an Geometrie haben, ergänzen Sie die 
             Implementation von @racket[collides-rectangle-circle]}
@#reader scribble/comment-reader
(racketblock
; GCircle GCircle -> Boolean
; determines whether c1 overlaps with c2
(define (collides-circle-circle c1 c2)
  (<= (vector-length (posn- (gcircle-center c1) (gcircle-center c2))) 
      (+ (gcircle-radius c1) (gcircle-radius c2))))

; GRectangle GRectangle -> Boolean
; determines whether r1 overlaps with r2
(define (collides-rectangle-rectangle r1 r2)
  (or
   (point-inside-rectangle r1 (grectangle-corner-ul r2))
   (point-inside-rectangle r1 (grectangle-corner-dr r2))
   (point-inside-rectangle r1 (make-posn 
                                   (posn-x (grectangle-corner-ul r2))
                                   (posn-y (grectangle-corner-dr r2))))
   (point-inside-rectangle r1 (make-posn 
                                   (posn-x (grectangle-corner-dr r2))
                                   (posn-y (grectangle-corner-ul r2))))))

; GRectangle GCircle -> Boolean
; determines whether r overlaps with c
(define (collides-rectangle-circle r c) ...)
)
