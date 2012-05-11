;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname animation) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; time is a number
; interp. how many frames since start of animation

; x-coordinate is a number
; interp. horizontal position in an image, growing to the right, in pixels

; y-coordinate is a number
; interp. position in an image, growing to the bottom, in pixels

; angle is a number
; interp. angle in radians, 0 is facing straight right


; image
; simple image for testing
(define TEST-IMAGE (center-pinhole (circle 20 "solid" "red")))

; number
; accepted difference for testing with inexact numbers
(define EPSILON 0.001)

; time
; length of the animation
(define DURATION (* 20 28))

; x-coordinate
; width of the scene
(define WIDTH 300)

; y-coordinate
; height of the scene
(define HEIGHT 300)

; angle distance image -> image
; move the image in the specified direction
(check-expect (move 15 0 TEST-IMAGE) (center-pinhole TEST-IMAGE))
(check-expect (move 22 -20 (move 22 20 TEST-IMAGE)) (center-pinhole TEST-IMAGE))
(define (move alpha distance image)
  (put-pinhole (- (pinhole-x image) (* distance (cos alpha)))
               (+ (pinhole-y image) (* distance (sin alpha)))
               image))

; angle distance image -> image
; place five copies of the image on a circle
(define (scatter alpha distance image)
  (overlay/pinhole (move (+ alpha (* 0.0 pi)) distance image)
                   (move (+ alpha (* 0.4 pi)) distance image)
                   (move (+ alpha (* 0.8 pi)) distance image)
                   (move (+ alpha (* 1.2 pi)) distance image)
                   (move (+ alpha (* 1.6 pi)) distance image)))

; number number time time -> number
; oscillates between min and max with a given period
(check-within (oscillate 5 10 20 00) 05.0 EPSILON)
(check-within (oscillate 5 10 20 05) 07.5 EPSILON)
(check-within (oscillate 5 10 20 10) 10.0 EPSILON)
(check-within (oscillate 5 10 20 15) 07.5 EPSILON)
(check-within (oscillate 5 10 20 20) 05.0 EPSILON)
(define (oscillate min max period t)
  (+ min (* 0.5 (- max min) (+ 1 (cos (+ pi (* 2 pi (/ t period))))))))

; time -> image
; draws one frame of the animation, without a scene
(define (outer-image t)
  (scatter (+ (* 0.5 pi) (* 2 pi (/ t DURATION)))
           (oscillate 50 100 DURATION t)
           (inner-image t)))

; time -> image
; draws one frame of one of the five sub-animations
(define (inner-image t)
  (scatter (+ (* 0.5 pi) (* 10 pi (/ t DURATION)))
           (oscillate 10 20 (/ DURATION 5) t)
           (innermost-image t)))

; time -> image
; draws one of the triangles
(define (innermost-image t)
  (center-pinhole (triangle (oscillate 5 20 (/ DURATION 5) t) "solid" "red")))

; time -> image
; draws one frame of the animation, including a scene
(define (create-scene t)
  (place-image/align (outer-image t) 
                     (/ WIDTH 2) 
                     (/ HEIGHT 2)
                     "pinhole"
                     "pinhole"
                     (empty-scene WIDTH HEIGHT)))

(animate create-scene)