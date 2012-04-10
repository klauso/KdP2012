#lang scribble/manual
@(require scribble/eval)

   
@title[#:version ""]{Module}

@margin-note{Dieser Teil des Skripts basiert auf Kap. 6 des "Racket Guide"}

Der Zweck von Modulen ist, ein Programm in sinnvolle und wiederverwendbare Einheiten zu zerlegen.

@codeblock|{
#lang racket
 
(provide print-cake)
 
; draws a cake with n candles
(define (print-cake n)
  (show "   ~a   " n #\.)
  (show " .-~a-. " n #\|)
  (show " | ~a | " n #\space)
  (show "---~a---" n #\-))
 
(define (show fmt n ch)
  (printf fmt (make-string n ch))
  (newline))
}|



