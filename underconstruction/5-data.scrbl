#lang scribble/manual
@(require scribble/eval)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
   
@title[#:version ""]{Datenmodellierung}

@margin-note{Dieser Teil des Skripts basiert auf [HTDP/2e] Kapitel 2}

@section{Auzählungstypen}


@#reader scribble/comment-reader
(racketblock
; A TrafficLight shows one of three colors:
; – "red"
; – "green"
; – "yellow"
; interp. each element of TrafficLight represents which colored
; bulb is currently turned on
  )
  
  
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
    