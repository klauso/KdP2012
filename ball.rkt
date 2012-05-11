;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ball) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
(define WIDTH 200)
(define HEIGHT 200)
(define BALL-IMG (circle 10 "solid" "red"))
(define BALL-RADIUS (/ (image-width BALL-IMG) 2))

(define-struct vel (delta-x delta-y))
; a Vel is (make-vel Number Number)
; interp. the velocity vector of a moving object

(define-struct ball (loc velocity))
; a Ball is (make-ball Posn Vel)
; interp. the position and velocity of a object 

; Posn Vel -> Posn
; applies q to p and simulates the movement in one clock tick
(check-expect (posn+vel (make-posn 5 6) (make-vel 1 2)) 
              (make-posn 6 8))
(define (posn+vel p q)
  (make-posn (+ (posn-x p) (vel-delta-x q)) 
             (+ (posn-y p) (vel-delta-y q))))


; Ball -> Ball
; computes movement of ball in one clock tick
(check-expect (move-ball (make-ball (make-posn 20 30) 
                                    (make-vel 5 10))) 
              (make-ball (make-posn 25 40) 
                         (make-vel 5 10)))
(define (move-ball ball)
  (make-ball (posn+vel (ball-loc ball) 
                       (ball-velocity ball))
             (ball-velocity ball)))

; A Collision is either
; - "top"
; - "down"
; - "left"
; - "right"
; - "none"
; interp. the location where a ball collides with a wall

; Posn -> Collision
; detects with which of the walls (if any) the ball collides
(check-expect (collision (make-posn 0 12))  "left")
(check-expect (collision (make-posn 15 HEIGHT)) "down")
(check-expect (collision (make-posn WIDTH 12))  "right")
(check-expect (collision (make-posn 15 0)) "top")
(check-expect (collision (make-posn 55 55)) "none")
(define (collision posn)
  (cond 
    [(<= (posn-x posn) BALL-RADIUS) "left"]
    [(<= (posn-y posn) BALL-RADIUS)  "top"]
    [(>= (posn-x posn) (- WIDTH BALL-RADIUS)) "right"]
    [(>= (posn-y posn) (- HEIGHT BALL-RADIUS)) "down"]
    [else "none"]))
  
; Vel Collision -> Vel  
; computes the velocity of an object after a collision
(check-expect (bounce (make-vel 3 4) "left") 
              (make-vel -3 4))
(check-expect (bounce (make-vel 3 4) "top") 
              (make-vel 3 -4))
(check-expect (bounce (make-vel 3 4) "none") 
              (make-vel 3 4))
(define (bounce vel collision)
  (cond [(or (string=? collision "left") 
             (string=? collision "right")) 
         (make-vel (- (vel-delta-x vel)) 
                   (vel-delta-y vel))]
        [(or (string=? collision "down") 
             (string=? collision "top")) 
         (make-vel (vel-delta-x vel) 
                   (- (vel-delta-y vel)))]
        [else vel]))
        
; WorldState is a Ball

; WorldState -> Image
; renders ball at its position
(check-expect (image? (render INITIAL-BALL)) true)
(define (render ball)
  (place-image BALL-IMG 
               (posn-x (ball-loc ball)) 
               (posn-y (ball-loc ball)) 
               (empty-scene WIDTH HEIGHT)))


; WorldState -> WorldState
; moves ball to its next location
(check-expect (tick (make-ball (make-posn 20 12) (make-vel 1 2))) 
              (make-ball (make-posn 21 14) (make-vel 1 2)))
(define (tick ball)
  (move-ball (make-ball (ball-loc ball) 
                        (bounce (ball-velocity ball) 
                                (collision (ball-loc ball))))))

(define INITIAL-BALL (make-ball (make-posn 20 12)
                                (make-vel 1 2)))

(define (main ws) 
  (big-bang ws (on-tick tick 0.01) (to-draw render)))

; start with: (main initial-ball)