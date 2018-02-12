;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q2.rkt : solution for q2 in the problem set 06

;; (squashpractice1)

;; squash simulation
;; game starts with the ready-to-serve state with ball and the racket at
;; the same position.
;; the user can change the state by pressing a space bar.
;; moving from ready-to-serve state to rally state will start the game and
;; the ball moves in the court and collides with walls and racket to change
;; the position and velocity.
;; changing the state from rally to paused will pause the game for three seconds
;; and will go to ready-to-serve state again

;; (squashpractice2)

;; squash simulation
;; like squashpractice1, but user can drag the racket with the mouse in the
;; rally state. button-down to select, drag to move, button up to release.
;; when selected a blue circle as a mouse pointer will be there.
;; racket can be selected by positioning mouse no more than 25 pixels away
;; from center of the racket

;; (SquashPractice3)

;; squash simulation
;; improved version of squashpractice2
;; like squashpractice2, but the user can press the "b" key and create a
;; a new ball in the rally state, the new balls will behave like the ball in
;; squashpractice2. Ball when collides with the back wall, disappears and
;; when all the balls are disappeared the rally state ends to paused state.
;; the racket and ball collision is improved for better user experience. 

;; START:
;; can be started with any PosReal number
;; NOTE:
;; greater number e.g. 1, will result in very slow motion.
;; for realistic behavior use 1/24
;; start with (simulation 1/24)

;; including required libraries
(require rackunit)              ;; for check-equal? 
(require "extras.rkt")          ;; for begin-for-test, check-location 
(require 2htdp/universe)        ;; for creating interactive, graphical programs
(require 2htdp/image)           ;; for different image functions used

;; to see if the file is correctly named and in the right location
(check-location "06" "q2.rkt")

;; to require the file and check all functions are provided
(provide
 simulation
 initial-world
 world-ready-to-serve?
 world-after-tick
 world-after-key-event
 world-racket
 ball-x
 ball-y
 racket-x
 racket-y
 ball-vx
 ball-vy
 racket-vx
 racket-vy
 world-after-mouse-event
 racket-after-mouse-event
 racket-selected?
 world-balls
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION

;; simulation : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick
;;        (so larger numbers run slower)
;; EFFECT: runs the simulation, starting with the initial world
;; RETURNS: the final state of the world

;; EXAMPLES:
;; (simulation 1) runs in super slow motion
;; (simulation 1/24) runs at a more realistic speed

;; DESIGN STRATEGY: combine simpler functions

(define (simulation speed)
  (big-bang (initial-world speed)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions of the court
;; court width in pixels
(define COURT-WIDTH 425)
;; court height in pixels
(define COURT-HEIGHT 649)

;; radius of the ball
(define BALL-RADIUS 3)
;; image of the ball
(define BALL-IMAGE (circle BALL-RADIUS "solid" "black"))

;; positioning of mouse to select racket
(define MOUSE-SELECT-POS 25)

;; width of racket
(define RACKET-WIDTH 47)
;; height of racket
(define RACKET-HEIGHT 7)
;; image of the racket
(define RACKET-IMAGE (rectangle RACKET-WIDTH RACKET-HEIGHT "solid" "green"))
;; racket half width
(define RACKET-HALF-WIDTH (/ RACKET-WIDTH 2))

;; radius of pointer circle
(define POINTER-CIRCLE-RADIUS 4)
;; image of pointer circle
(define POINTER-CIRCLE-IMAGE (circle POINTER-CIRCLE-RADIUS "solid" "blue"))

;; the simulation start coordinates for location
(define START-X 330)
(define START-Y 384)

;; total time for the paused world-state
(define PAUSED-WAIT-TIME 3)

;; initial value of counter for ticks
(define INITIAL-COUNTER-VALUE 1)

;; examples KeyEvent for testing
(define STATE-KEY-EVENT " ")
(define LEFT-ARROW-KEY-EVENT "left")
(define RIGHT-ARROW-KEY-EVENT "right")
(define UP-ARROW-KEY-EVENT "up")
(define DOWN-ARROW-KEY-EVENT "down")
(define B-KEY-EVENT "b")
(define OTHER-EVENT "q")

;; example MouseEvents for testing:
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")
(define OTHER-MOUSE-EVENT "enter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;; Ball

;; REPRESENTATION:
;; a Ball is represented as a struct
;; (make-ball x y vx vy)
;; with the following fields:
;; x, y       : Integer is the position of the center of the ball in
;;                      the scene
;; vx         : Integer is the horizontal component of the velocity of the ball
;;                      which gives the pixels, the ball move in x direction
;; vy         : Integer is the vertical component of the velocity of the ball
;;                      which gives the pixels, the ball move in y direction

;; IMPLEMENTATION:
(define-struct ball (x y vx vy))

;; CONSTRUCTOR TEMPLATE:
;; (make-ball Integer Integer Integer Integer)

;; OBSERVER TEMPLATE:
;; ball-fn : Ball -> ?
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))

;; Balls

;; Balls is represented as a list of Ball which keeps on adding when "b" key
;; is pressed, with one entry per ball in the court

;; CONSTRUCTOR TEMPLATES:
;; empty
;; (cons b bs)
;; -- where
;; b is a Ball
;; bs is Balls

;; OBSERVER TEMPLATE:
;; balls-fn : BallS -> ?
#;
(define (balls-fn bs)
  (cond
    [(empty? bs) ...]
    [else (...
           (ball-fn (first bs))
           (balls-fn (rest bs)))]))

;; Racket

;; REPRESENTATION:
;; a Racket is represented as a struct
;; (make-racket x y vx vy mx my selected?)
;; with the following fields:
;; x, y      : Integer is the position of the center of the racket in
;;                     the scene
;; vx        : Integer is the horizontal component of the velocity of the racket
;;                     which gives the pixels, the racket move in x direction
;; vy        : Integer is the vertical component of the velocity of the racket
;;                     which gives the pixels, the racket move in y direction
;; mx, my    : Integer is the x and y coordinates of the mouse when the
;;                     racket is selected
;; selected? : Boolean describes whether or not the racket is selected

;; IMPLEMENTATION:
(define-struct racket (x y vx vy mx my selected?))

;; CONSTRUCTOR TEMPLATE:
;; (make-racket Integer Integer Integer Integer
;;              Integer Integer Boolean)

;; OBSERVER TEMPLATE:
;; racket-fn : Racket -> ?
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       (racket-mx r)
       (racket-my r)
       (racket-selected? r)))

;; State

;; REPRESENTATION:
;; a State is represented by one of the Strings
;; -- "ready-to-serve"
;; -- "rally"
;; -- "paused"
;; INTERPRETATION: the different states of the simulation

;; EXAMPLES:
(define READY-TO-SERVE "ready-to-serve")
(define RALLY "rally")
(define PAUSED "paused")

;; OBSERVER TEMPLATE:
;; state-fn : State -> ?
#;
(define (state-fn s)
  (cond [(string=? s READY-TO-SERVE) ...]
        [(string=? s RALLY) ...]
        [(string=? s PAUSED) ...]))

;; BgColor

;; REPRESENTATION:
;; a BgColor is represented by one of the Strings
;; -- "white"
;; -- "yellow"
;; INTERPRETATION: self-evident

;; EXAMPLES:
(define BG-COLOR-WHITE "white")
(define BG-COLOR-YELLOW "yellow")


;; OBSERVER TEMPLATE:
;; bg-color-fn : BgColor -> ?
#;
(define (bg-color-fn c)
  (cond [(string=? c BG-COLOR-WHITE) ...]
        [(string=? c BG-COLOR-YELLOW) ...]))

;; World

;; REPRESENTATION:
;; a World is represented as a struct
;; (make-world balls racket state bg-color tick-counter simulation-speed)
;; with the following fields:
;; balls             : Balls   the list of balls in the world
;; racket            : Racket  the racket in the world
;; state             : State   the state of the simulation
;; bg-color          : BgColor the background color of the world
;; tick-counter      : PosInt  is the counter for the clock tick
;; simulation-speed  : PosReal the speed of the simulation, in seconds per
;;                             clock tick
;;                             (a larger number run slower)

;; IMPLEMENTATION:
(define-struct world (balls
                      racket
                      state
                      bg-color
                      tick-counter
                      simulation-speed))

;; CONSTRUCTOR TEMPLATE:
;;(make-world Balls Racket State BgColor PosInt PosReal)

;; OBSERVER TEMPLATE:
;; world-fn : World -> ?
(define (world-fn w)
  (... (world-balls w)
       (world-racket w)
       (world-state w)
       (world-bg-color w)
       (world-tick-counter w)
       (world-simulation-speed w)))

;; examples of balls, for testing

;; balls in the initial world
(define INITIAL-WORLD-BALLS
  (list (make-ball START-X START-Y 0 0)))

;; ball in the start of the rally world state
(define INITIAL-RALLY-WORLD-BALLS
  (list (make-ball START-X START-Y 3 -9)))

;; random balls in rally state
(define RANDOM-SINGLE-BALL-330-384
  (make-ball 330 384 3 -9))
(define RANDOM-SINGLE-BALL-330-380
  (make-ball 330 380 3 -9))
(define RANDOM-BALL-330-384
  (list (make-ball 330 384 3 -9)))
(define EMPTY-BALL-LIST
  (list))
(define MULTIPLE-LIST-RANDOM-BALL
  (list (make-ball 424 96 -3 -9) (make-ball 417 123 3 -9)))
(define B-KEY-RANDOM-BALL-330-384
  (list (make-ball 330 384 3 -9) (make-ball START-X START-Y 3 -9)))
(define NEXT-RANDOM-BALL-333-375
  (list (make-ball 333 375 3 -9)))
(define NEXT-RANDOM-SINGLE-BALL-333-375
  (make-ball 333 375 3 -9))

;; ball which will collide with the front wall
(define BALL-FRONT-COLLIDE-Y-6
  (make-ball 380 6 -3 -9))
(define BALLS-FRONT-COLLIDE-Y-6
  (list (make-ball 380 6 -3 -9)))
(define NEXT-BALL-FRONT-COLLIDE-Y-6
  (make-ball 377 3 -3 9))
(define NEXT-BALLS-FRONT-COLLIDE-Y-6
  (list (make-ball 377 3 -3 9)))

;; ball which will collide the right wall
(define BALL-RIGHT-COLLIDE-X-423
  (make-ball 423 132 3 -9))
(define NEXT-BALL-RIGHT-COLLIDE-X-423
  (make-ball 424 123 -3 -9))

;; ball which will collide with the left wall
(define BALL-LEFT-COLLIDE-x-2
  (make-ball 2 350 -3 9))
(define NEXT-BALL-LEFT-COLLIDE-x-2
  (make-ball 1 359 3 9))

;; ball which will collide with the back wall
(define BALL-BACK-COLLIDE-Y-643
  (list (make-ball 200 643 -3 9)))
(define SINGLE-BALL-BACK-COLLIDE-Y-643
  (make-ball 200 643 -3 9))

;; ball in the start of the rally world state
(define INITIAL-RALLY-WORLD-BALL
  (make-ball START-X START-Y 3 -9))

;; random ball in paused state
(define BALL-PAUSED-STATE-200-643
  (list (make-ball 200 643 -3 9)))

;; examples of rackets, for testing

;; racket in the initial world
(define INITIAL-WORLD-RACKET
  (make-racket START-X START-Y 0 0 0 0 false))

;; random racket in paused state
(define RACKET-PAUSED-STATE-149-384
  (make-racket 149 384 -1 0 0 0 false))

;; random racket in rally state and not selected
(define PREVIOUS-RANDOM-RACKET-149-384
  (make-racket 149 384 -1 0 0 0 false))
(define RANDOM-RACKET-149-384
  (make-racket 149 384 -1 0 0 0 false))
(define NEXT-RANDOM-RACKET-148-384
  (make-racket 148 384 -1 0 0 0 false))
(define NEXT-RANDOM-RACKET-151-384
  (make-racket 151 384 -1 0 0 0 false))

;; random racket selected in rally state
(define SELECTED-RANDOM-RACKET-152-384
  (make-racket 152 384 -2 0 0 0 true))
(define NEXT-SELECTED-RANDOM-RACKET-152-384
  (make-racket 352 684 -2 0 200 300 true))
(define BUTTON-UP-RANDOM-RACKET-152-384
  (make-racket 152 384 -2 0 0 0 false))

;; racket colliding with the ball
(define RACKET-BALL-COLLIDE
  (make-racket 248 380 0 0 0 0 false))
(define NEXT-RACKET-BALL-COLLIDE
  (make-racket 248 380 0 0 0 0 false))
(define RACKET-BALL-COLLIDE-NEG-VY
  (make-racket 248 380 0 -3 0 0 false))
(define NEXT-RACKET-BALL-COLLIDE-NEG-VY
  (make-racket 248 380 0 0 0 0 false))

;; with mx and my
(define SELECTED-RANDOM-RACKET-152-384-140-200
  (make-racket 152 384 -2 0 140 200 true))
(define SELECTED-RANDOM-RACKET-152-384-152-384
  (make-racket 152 384 -2 0 152 384 true))

;; random racket with key events in rally state
(define LEFT-RANDOM-RACKET-149-384
  (make-racket 149 384 -2 0 0 0 false))
(define RIGHT-RANDOM-RACKET-149-384
  (make-racket 149 384 0 0 0 0 false))
(define UP-RANDOM-RACKET-149-384
  (make-racket 149 384 -1 -1 0 0 false))
(define DOWN-RANDOM-RACKET-149-384
  (make-racket 149 384 -1 1 0 0 false))

;; racket which will collide the right wall
(define RACKET-RIGHT-COLLIDE-X-424
  (make-racket 424 384 1 0 0 0 false))
(define NEXT-RACKET-RIGHT-COLLIDE-X-424
  (make-racket 402 384 1 0 0 0 false))

;; racket which will collide the left wall
(define RACKET-LEFT-COLLIDE-X-3
  (make-racket 3 390 -4 0 0 0 false))
(define NEXT-RACKET-LEFT-COLLIDE-X-3
  (make-racket 24 390 -4 0 0 0 false))

;; racket which will collide with the front wall
(define RACKET-FRONT-COLLIDE-Y-4
  (make-racket 330 4 0 -5 0 0 false))

;; racket which will collide with the back wall
(define RACKET-BACK-COLLIDE-Y-642
  (make-racket 335 642 0 8 0 0 false))

;; ball colliding with the racket
(define BALL-RACKET-COLLIDE
  (list (make-ball 249 378 -3 4)))
(define SINGLE-BALL-RACKET-COLLIDE
  (make-ball 249 378 -3 4))
(define NEXT-BALL-RACKET-COLLIDE
  (make-ball 249 378 -3 -4))
(define NEXT-BALLS-RACKET-COLLIDE
  (list (make-ball 249 378 -3 -4)))

;; ball colliding with the racket second time
(define BALL-RACKET-SECOND-COLLIDE
  (list (make-ball 249 378 -3 4)))
(define NEXT-BALL-RACKET-SECOND-COLLIDE
  (make-ball 246 382 -3 4))

;; initial world
(define INITIAL-STATE-WORLD
  (make-world
   INITIAL-WORLD-BALLS
   INITIAL-WORLD-RACKET
   READY-TO-SERVE
   BG-COLOR-WHITE
   INITIAL-COUNTER-VALUE 1))

;; world in rally state
(define WORLD-RALLY-STATE
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-WHITE
   1
   1/24))
(define NEXT-WORLD-RALLY-STATE
  (make-world
   NEXT-RANDOM-BALL-333-375
   NEXT-RANDOM-RACKET-148-384
   RALLY
   BG-COLOR-WHITE
   1
   1/24))

;; world when mouse "button-down" is pressed
(define AFTER-BUTTON-DOWN-EVENT
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-WHITE
   1
   1/24))

;; after the left key event
(define NEXT-ON-RIGHT-KEY
  (make-world
   RANDOM-BALL-330-384
   RIGHT-RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-WHITE
   1
   1/24))

;; after the up key event
(define NEXT-ON-UP-KEY
  (make-world
   RANDOM-BALL-330-384
   UP-RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-WHITE
   1
   1/24))

;; after the down key event
(define NEXT-ON-DOWN-KEY
  (make-world
   RANDOM-BALL-330-384
   DOWN-RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-WHITE
   1
   1/24))

;; worlds in ready-to-serve state
(define WORLD-READY-TO-SERVE
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   READY-TO-SERVE
   BG-COLOR-YELLOW
   3
   1))
(define NEXT-WORLD-READY-TO-SERVE
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-YELLOW
   3
   1))

;; after the space key event
(define NEXT-RALLY-WORLD-STATE
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   PAUSED
   BG-COLOR-WHITE
   1
   1/24))

;; after the left key event
(define NEXT-ON-LEFT-KEY
  (make-world
   RANDOM-BALL-330-384
   LEFT-RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-WHITE
   1
   1/24))

;; worlds with counter 4 and speed 1 in paused state
(define WORLD-PAUSED-COUNTER-C-4-S-1
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   PAUSED
   BG-COLOR-YELLOW
   4
   1))
(define NEXT-WORLD-PAUSED-COUNTER-C-4-S-1
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   READY-TO-SERVE
   BG-COLOR-YELLOW
   4
   1))

;; worlds with a paused state

;; worlds with consecutive counter in paused state
(define WORLD-PAUSED-COUNTER-5
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   PAUSED
   BG-COLOR-YELLOW
   5
   1/24))
(define WORLD-PAUSED-COUNTER-4
  (make-world
   RANDOM-BALL-330-384
   RANDOM-RACKET-149-384
   PAUSED
   BG-COLOR-YELLOW
   4
   1/24))

;; worlds with counter 1 and speed 1 in paused state
(define WORLD-PAUSED-COUNTER-C-1-S-1
  (make-world
   INITIAL-WORLD-BALLS
   INITIAL-WORLD-RACKET
   READY-TO-SERVE
   BG-COLOR-WHITE
   1
   1))

;; world to end rally state

;; world when ball collides the back wall
(define END-RALLY-STATE-BALL-BACK
  (make-world
   EMPTY-BALL-LIST
   RANDOM-RACKET-149-384
   RALLY
   BG-COLOR-WHITE
   1
   1))

;; world when racket collides the front wall
(define END-RALLY-STATE-RACKET-FRONT
  (make-world
   RANDOM-BALL-330-384
   RACKET-FRONT-COLLIDE-Y-4
   RALLY
   BG-COLOR-WHITE
   1
   1))

;; world when racket collides the back wall
(define END-RALLY-STATE-RACKET-BACK
  (make-world
   RANDOM-BALL-330-384
   RACKET-BACK-COLLIDE-Y-642
   RALLY
   BG-COLOR-WHITE
   1
   1))

;; paused world after rally
(define PAUSED-STATE-AFTER-RALLY
  (make-world
   EMPTY-BALL-LIST
   RACKET-PAUSED-STATE-149-384
   PAUSED
   BG-COLOR-YELLOW
   2
   1))


;; image for racket 149 384 in scene
(define RACKET-SCENE-IMAGE (place-image RACKET-IMAGE
                                        149 384
                                        (empty-scene
                                         COURT-WIDTH
                                         COURT-HEIGHT
                                         BG-COLOR-WHITE)))

;; image of ball racket 152 384 in scene
(define RACKET-BALL-SCENE-IMAGE (place-image POINTER-CIRCLE-IMAGE
                                             140 200
                                             (place-image RACKET-IMAGE
                                                          152 384
                                                          (empty-scene
                                                           COURT-WIDTH
                                                           COURT-HEIGHT
                                                           BG-COLOR-WHITE))))

;; image of multiple balls 424 96 and 417 123 in racket scene 149 384
(define MULTIPLE-BALL-SCENE-IMAGE
  (place-image BALL-IMAGE
               424 96
               (place-image BALL-IMAGE
                            417 123
                            (place-image
                             RACKET-IMAGE
                             149 384
                             (empty-scene
                              COURT-WIDTH
                              COURT-HEIGHT
                              BG-COLOR-WHITE)))))


;; image for ball 330 384 and racket 152 384 in paused state
(define IMAGE-RANDOM-PAUSED
  (place-image BALL-IMAGE
               330 384
               (place-image RACKET-IMAGE
                            152 384
                            (empty-scene
                             COURT-WIDTH
                             COURT-HEIGHT
                             BG-COLOR-YELLOW))))

;; image for ball 330 384 and racket 149 384 in paused state
(define IMAGE-RANDOM-PAUSED-149-384
  (place-image BALL-IMAGE
               330 384
               (place-image RACKET-IMAGE
                            149 384
                            (empty-scene
                             COURT-WIDTH
                             COURT-HEIGHT
                             BG-COLOR-YELLOW))))

;; scene for ball-scene
(define BALL-IMAGE-SCENE
  (place-image RACKET-IMAGE
               152 384
               (empty-scene
                COURT-WIDTH
                COURT-HEIGHT
                BG-COLOR-YELLOW)))

;; scene for racket-scene
(define RACKET-IMAGE-SCENE (empty-scene
                            COURT-WIDTH
                            COURT-HEIGHT
                            BG-COLOR-WHITE))

;; image for ball 330 384 and racket 149 384 in rally state
(define IMAGE-RANDOM-RALLY
  (place-image BALL-IMAGE
               330 384
               (place-image RACKET-IMAGE
                            149 384
                            (empty-scene
                             COURT-WIDTH
                             COURT-HEIGHT
                             BG-COLOR-WHITE))))

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : PosReal -> World
;; GIVEN: the speed of the simulation, in seconds per tick
;;        (so larger numbers run slower)
;; RETURNS: the ready-to-serve state of the world

;; EXAMPLE:
;; (initial-world 1) = (make-world INITIAL-WORLD-BALL INITIAL-WORLD-RACKET
;;                                 READY-TO-SERVE BG-COLOR-WHITE
;;                                 INITIAL-COUNTER-VALUE 1) 

;; DESIGN STRATEGY: use constructor template for world

(define (initial-world speed)
  (make-world
   INITIAL-WORLD-BALLS
   INITIAL-WORLD-RACKET
   READY-TO-SERVE
   BG-COLOR-WHITE
   INITIAL-COUNTER-VALUE
   speed))

;; TESTS:
(begin-for-test
  (check-equal?
   (initial-world 1)
   (make-world
    INITIAL-WORLD-BALLS
    INITIAL-WORLD-RACKET
    READY-TO-SERVE BG-COLOR-WHITE
    INITIAL-COUNTER-VALUE 1)
   " the initial state is not returning the world that should follow"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : World -> World
;; GIVEN: any world that's possible for the simulation
;; RETURNS: the world that should follow the given world
;;          after a tick

;; EXAMPLES:
;; (world-after-tick WORLD-PAUSED-COUNTER-4) = WORLD-PAUSED-COUNTER-5
;; (world-after-tick WORLD-READY-TO-SERVE) = INITIAL-STATE-WORLD
;; (world-after-tick WORLD-RALLY-STATE) = NEXT-WORLD-RALLY-STATE
;; (world-after-tick END-RALLY-STATE-BALL-BACK) = PAUSED-STATE-AFTER-RALLY

;; DESIGN STRATEGY: cases on state of the world

(define (world-after-tick w)
  (cond [(world-paused? w) (world-after-paused-state w)]
        [(world-ready-to-serve? w) (initial-world (world-simulation-speed w))]
        [(world-rally? w) (world-in-rally-state w)]))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-tick WORLD-PAUSED-COUNTER-4)
                WORLD-PAUSED-COUNTER-5
                "the world is not in the paused state")
  (check-equal? (world-after-tick WORLD-READY-TO-SERVE)
                INITIAL-STATE-WORLD
                "the word is not made as the initial world")
  (check-equal? (world-after-tick WORLD-RALLY-STATE)
                NEXT-WORLD-RALLY-STATE
                "the world state is not the one in the rally state")
  (check-equal? (world-after-tick END-RALLY-STATE-BALL-BACK)
                PAUSED-STATE-AFTER-RALLY
                "the word is not made as the paused world after end rally"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-balls : World -> Balls
;; GIVEN: a world
;; RETURNS: a list of the balls that are present in the world
;;         (but does not include any balls that have disappeared
;;          by colliding with the back wall)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-racket : World -> Racket
;; GIVEN: a world
;; RETURNS: the racket that's present in the world

;; NOTE: world-racket is a selector defined by "define-struct world"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-x : Ball -> Integer
;; ball-y : Ball -> Integer
;; racket-x : Racket -> Integer
;; racket-y : Racket -> Integer
;; GIVEN: a racket or ball
;; RETURNS: the x or y coordinate of that item's position,
;;          in graphics coordinates

;; NOTE: ball-x and ball-y are selector defined by "define-struct ball"
;;       racket-x and racket-y are selector define by "define-struct racket"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-vx : Ball -> Integer
;; ball-vy : Ball -> Integer
;; racket-vx : Racket -> Integer
;; racket-vy : Racket -> Integer
;; GIVEN: a racket or ball
;; RETURNS: the vx or vy component of that item's velocity,
;;          in pixels per tick

;; NOTE: ball-vx and ball-vy are selector defined by "define-struct ball"
;;       racket-vx and racket-vy are selector define by "define-struct racket"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-selected? : Racket-> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the racket is selected

;; NOTE: racket-selected? is a selector defined by "define-struct racket"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-in-rally-state : World -> World
;; GIVEN:  a world
;; RETURNS: the world which follow the given world when the state is rally

;; EXAMPLES:
;; (world-after-tick WORLD-RALLY-STATE) = NEXT-WORLD-RALLY-STATE

;; DESIGN STRATEGY: case on the end of rally state 

(define (world-in-rally-state w)
(if (end-rally-state? w)
    (world-after-paused-state w)
    (world-change-in-rally-state w)))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-tick WORLD-RALLY-STATE)
                NEXT-WORLD-RALLY-STATE
                "the world state is not the one in the rally state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-change-in-rally-state : World -> World
;; GIVEN:  a world
;; RETURNS: the world which follow the given world when the state is rally

;; EXAMPLES:
;; (world-change-in-rally-state WORLD-RALLY-STATE) = NEXT-WORLD-RALLY-STATE

;; DESIGN STRATEGY: use constructor template for world 

(define (world-change-in-rally-state w)
  (make-world
   (balls-after-tick (world-balls w) (world-racket w))
   (racket-after-tick (world-racket w) (world-balls w))
   (world-state w)
   (world-bg-color w)
   (world-tick-counter w)
   (world-simulation-speed w)))

;; TESTS:
(begin-for-test
  (check-equal? (world-change-in-rally-state WORLD-RALLY-STATE)
                NEXT-WORLD-RALLY-STATE
                "the world state is not the one in the rally state"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-paused-state : World -> World
;; GIVEN: world with the paused world-state
;; RETURNS: the world which follow the given world when the state is paused
;;          after a tick and after three seconds of real time reset the
;;          simulation to ready-to-serve state

;; EXAMPLES:
;; (world-after-paused-state WORLD-PAUSED-COUNTER-C-4-S-1)
;;  = WORLD-PAUSED-COUNTER-C-1-S-1
;; (world-after-paused-state WORLD-PAUSED-COUNTER-4) = WORLD-PAUSED-COUNTER-5

;; DESIGN STRATEGY: combine simpler functions

(define (world-after-paused-state w)
  (if (paused-time-counter? (world-tick-counter w) (world-simulation-speed w))
      (world-with-paused-state w)
      (initial-world (world-simulation-speed w))))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-paused-state WORLD-PAUSED-COUNTER-C-4-S-1)
                WORLD-PAUSED-COUNTER-C-1-S-1
                "the world is not changed to ready to serve state after
three seconds")
  (check-equal? (world-after-paused-state WORLD-PAUSED-COUNTER-4)
                WORLD-PAUSED-COUNTER-5
                "the tick-counter of the world is not increased by 1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; paused-time-counter? : PosInt PosReal -> Boolean
;; GIVEN: the tick counter and simulation speed of world
;; RETURNS: true iff, the number if tick counter is less than or equal to ticks
;;          required in real world three seconds 

;; EXAMPLES:
;; (paused-time-counter? 4 1/24)
;;  = true

;; DESIGN STRATEGY: use simple function for comparison

(define (paused-time-counter? tc s)
  (<=
   tc
   (/ PAUSED-WAIT-TIME s)))

;; TESTS:
(begin-for-test
(check-equal? (paused-time-counter? 4 1/24)
                true
                "the tick-counter of the world should be true as less than
the ticks of three seconds"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-paused-state : World -> World
;; GIVEN: world with the paused world-state
;; RETURNS: the world which follow the given world for three seconds of
;;          real time when the state is paused

;; EXAMPLES:
;; (world-after-paused-state WORLD-PAUSED-COUNTER-4) = WORLD-PAUSED-COUNTER-5

;; DESIGN STRATEGY: use constructor template for World on w

(define (world-with-paused-state w)
  (make-world
   (world-balls w)
   (world-racket w)
   PAUSED
   BG-COLOR-YELLOW
   (+ (world-tick-counter w) 1)
   (world-simulation-speed w)))

;; TESTS:
(begin-for-test
  (check-equal? (world-with-paused-state WORLD-PAUSED-COUNTER-4)
                WORLD-PAUSED-COUNTER-5
                "the world counter is not increased by 1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-paused? : World -> Boolean
;; GIVEN: a world
;; RETURNS: true iff the world is in its paused state

;; EXAMPLES:
;; (world-paused? WORLD-PAUSED-COUNTER-4) = true
;; (world-paused? INITIAL-STATE-WORLD) = false

;; DESIGN STRATEGY: use of simple function to equate the value

(define (world-paused? w)
  (string=? (world-state w) PAUSED))

;; TESTS:
(begin-for-test
  (check-equal? (world-paused? WORLD-PAUSED-COUNTER-4)
                true
                "the world is not in a pause state")
  (check-equal? (world-paused? INITIAL-STATE-WORLD)
                false
                "the world is not in a pause state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-ready-to-serve? : World -> Boolean
;; GIVEN: a world
;; RETURNS: true iff the world is in its ready-to-serve state, otherwise false

;; EXAMPLES:
;; (world-ready-to-serve? WORLD-READY-TO-SERVE) = true
;; (world-ready-to-serve? WORLD-RALLY-STATE) = false

;; DESIGN STRATEGY: use of simple function to equate the value

(define (world-ready-to-serve? w)
  (string=? (world-state w) READY-TO-SERVE))

;; TESTS:
(begin-for-test
  (check-equal? (world-ready-to-serve? WORLD-READY-TO-SERVE)
                true
                "the world is not in ready-to-serve state")
  (check-equal? (world-ready-to-serve? WORLD-RALLY-STATE)
                false
                "the world is not in ready-to-serve state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; end-rally-state? World -> Boolean
;; GIVEN: a world
;; RETURNS: true iff the world do not wants to end the rally state, otherwise
;;          false

;; EXAMPLES:
;; (end-rally-state? END-RALLY-STATE-RACKET-FRONT)  = true
;; (end-rally-state? END-RALLY-STATE-RACKET-BACK)  = true

;; DESIGN STRATEGY: combine simpler functions

(define (end-rally-state? w)
  (or
   (balls-empty? (world-balls w))
   (racket-front-wall-collision? (world-racket w))
   (racket-back-wall-collision? (world-racket w))))

;; TESTS:
(begin-for-test
  (check-equal? (end-rally-state? END-RALLY-STATE-RACKET-FRONT)
                true
                "the rally state is not ending")
  (check-equal? (end-rally-state? END-RALLY-STATE-RACKET-BACK)
                true
                "the rally state is not ending"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-empty? : Balls -> Boolean
;; GIVEN: a list of the balls that are present in the world and not disappeared
;; RETURNS: true iff all the balls are disappeared and the list is empty,
;;          false otherwise

;; EXAMPLES:
;; (balls-empty? EMPTY-BALL-LIST) = true

;; DESIGN STRATEGY:  use of simple function

(define (balls-empty? bs)
  (empty? bs))
  
;; TESTS:
(begin-for-test
  (check-equal? (balls-empty? EMPTY-BALL-LIST)
                true
                "the list is empty the result should have been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-front-wall-collision? : Racket -> Boolean
;; GIVEN: the state of the racket r
;; RETURNS: true iff, the racket collides with the front wall

;; EXAMPLES:
;; (racket-front-wall-collision? RACKET-FRONT-COLLIDE-Y-4) = true

;; DESIGN STRATEGY: call a more general function

(define (racket-front-wall-collision? r)
  (racket-wall-collision? r < 0))

#;(< (tentative-pos (racket-y r)
                    (racket-vy r))
     0)

;; TESTS:
(begin-for-test
  (check-equal? (racket-front-wall-collision? RACKET-FRONT-COLLIDE-Y-4)
                true
                "the racket is colliding with the front wall the result should
true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-back-wall-collision? : Racket -> Boolean
;; GIVEN: the state of the racket r
;; RETURNS: true iff, the racket collides with the back wall

;; EXAMPLES:
;; (racket-back-wall-collision? RACKET-BACK-COLLIDE-Y-642) = true

;; DESIGN STRATEGY: call a more general function 

(define (racket-back-wall-collision? r)
  (racket-wall-collision? r > COURT-HEIGHT))

#;(> (tentative-pos (racket-y r)
                    (racket-vy r))
     COURT-HEIGHT)

;; TESTS:
(begin-for-test
  (check-equal? (racket-back-wall-collision? RACKET-BACK-COLLIDE-Y-642)
                true
                "the racket is colliding with the back wall the result should
true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-wall-collision? : Racket (Real -> Real) Real -> Boolean
;; GIVEN: a state of the racket r, a function and a real number 
;; RETURNS: true iff, the racket collides with the wall according to given
;;          function

;; EXAMPLES:
;; (racket-wall-collision? RACKET-BACK-COLLIDE-Y-642 > COURT-HEIGHT) = true

;; DESIGN STRATEGY: use simple function

(define (racket-wall-collision? r fn rn)
  (fn (tentative-pos (racket-y r)
                     (racket-vy r))
      rn))

;;TESTS:
(begin-for-test
  (check-equal? (racket-wall-collision? RACKET-BACK-COLLIDE-Y-642
                                        >
                                        COURT-HEIGHT)
                true
                "the racket is colliding with the back wall the result should
true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-rally? : World -> Boolean
;; GIVEN: a world
;; RETURNS: true iff the world is in its rally state

;; EXAMPLES:
;; (world-rally? WORLD-RALLY-STATE) = true
;; (world-rally? WORLD-READY-TO-SERVE) = false

;; DESIGN STRATEGY: use of simpler function to equate the value

(define (world-rally? w)
  (string=? (world-state w) RALLY))

;; TESTS:
(begin-for-test
  (check-equal? (world-rally? WORLD-RALLY-STATE)
                true
                "the world is not in rally state")
  (check-equal? (world-rally? WORLD-READY-TO-SERVE)
                false
                "the world is not in rally state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-after-tick : Balls Racket -> Balls
;; GIVEN: the state of list of the balls bs that are present in the world
;;        which are not disappeared and racket r, in rally state world
;; RETURNS: the state of the list of the balls after a tick

;; EXAMPLES:
;; (ball-after-tick BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
;;  = NEXT-BALLS-RACKET-COLLIDE
;; (ball-after-tick BALLS-FRONT-COLLIDE-Y-6 RANDOM-RACKET-149-384
;;  = NEXT-BALLS-FRONT-COLLIDE-Y-6

;; DESIGN STRATEGY: combine simpler functions

(define (balls-after-tick bs r)
  (cond [(any-ball-racket-collide? bs (racket-on-next-tick r))
         (balls-after-racket-collide bs (racket-on-next-tick r))]
        [(any-ball-out-of-court? bs)
         (remove-all empty (balls-wall-collide bs))]
        [else
         (next-state-of-balls bs)]))

;; TESTS:
(begin-for-test
  (check-equal? (balls-after-tick BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
                NEXT-BALLS-RACKET-COLLIDE
                "the change in ball after collision with racket is not done")
  (check-equal? (balls-after-tick BALLS-FRONT-COLLIDE-Y-6 RANDOM-RACKET-149-384)
                NEXT-BALLS-FRONT-COLLIDE-Y-6
                "the change in the ball after collision with ball is not done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-on-next-tick : Racket -> Racket
;; GIVEN: a racket r
;; RETURNS: the state of racket after a tick

;; EXAMPLES:
;; (racket-on-next-tick RANDOM-RACKET-149-384) = NEXT-RANDOM-RACKET-148-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-on-next-tick r)
(make-racket
 (+ (racket-x r) (racket-vx r))
 (+ (racket-y r) (racket-vy r))
 (racket-vx r)
 (racket-vy r)
 (racket-mx r)
 (racket-my r)
 (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-on-next-tick
                 RANDOM-RACKET-149-384)
                NEXT-RANDOM-RACKET-148-384
                "the racket after the next tick should have been different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-ball-racket-collide? : Balls Racket -> Boolean
;; GIVEN: a list of the balls that are present in the world which are not
;;        disappeared and a racket
;; RETURNS: true iff any one of the ball in the list is colliding with the
;;          racket, false otherwise

;; EXAMPLES:
;; (any-ball-racket-collide? BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
;;  = true

;; DESIGN STRATEGY: use HOF ormap on bs

(define (any-ball-racket-collide? bs r)
  (ormap
   ;; Ball -> Boolean
   ;; GIVEN: a ball
   ;; RETURNS: true iff the ball collides with racket, false otherwise
   (lambda
       (b)
     (check-ball-racket-collide? b r))
   bs))

#;(cond
    [(empty? bs) false]
    [else (or
           (check-ball-racket-collide? (first bs) r)
           (any-ball-racket-collide? (rest bs) r))])

;; TESTS:
(begin-for-test
  (check-equal? (any-ball-racket-collide?
                 BALL-RACKET-COLLIDE
                 RACKET-BALL-COLLIDE)
                true
                "the balls list have some ball which are colliding with the
racket, the value should have been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-ball-racket-collide? : Ball Racket -> Boolean
;; GIVEN: the state of a ball b and racket r, in rally state world
;; RETURNS: true iff the ball is not in the initial rally state and the
;;          ball collides with the racket, false otherwise

;; EXAMPLES:
;; (check-ball-racket-collide? BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE) = true
;; (check-ball-racket-collide? INITIAL-RALLY-WORLD-BALL
;;                             INITIAL-WORLD-RACKET) = false

;; DESIGN STRATEGY: combine simpler functions

(define (check-ball-racket-collide? b r)
  (and
   (ball-velocity-not-negative? b)
   (ball-racket-collide-call? b r)))

;; TESTS:
(begin-for-test
  (check-equal? (check-ball-racket-collide?
                 SINGLE-BALL-RACKET-COLLIDE
                 RACKET-BALL-COLLIDE)
                true
                "the ball and racket do not collide")
  (check-equal? (check-ball-racket-collide?
                 INITIAL-RALLY-WORLD-BALL
                 INITIAL-WORLD-RACKET)
                false
                "the ball and the racket are colliding"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-not-on-racket? : Ball Racket -> Boolean
;; GIVEN: the state of a ball b and racket r, in rally state world
;; RETURNS: true iff, ball is not present of the racket in the previous tick

;; EXAMPLES:
;; (ball-not-on-racket? RANDOM-SINGLE-BALL-330-384 RACKET-BALL-COLLIDE)
;;  = true
;; (ball-not-on-racket? RANDOM-SINGLE-BALL-330-380 RACKET-BALL-COLLIDE)
;;  = true

;; DESIGN STRATEGY: combine simpler functions

(define (ball-not-on-racket? b r)
  (not
    (and
     (equal? (ball-y b) (racket-y r))
     (<= (racket-left-x (racket-x r))
         (ball-x b)
         (racket-right-x (racket-x r))))))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-not-on-racket? RANDOM-SINGLE-BALL-330-384 RACKET-BALL-COLLIDE)
   true
   "the ball is not on racket the value should have been true")
  (check-equal?
   (ball-not-on-racket? RANDOM-SINGLE-BALL-330-380 RACKET-BALL-COLLIDE)
   true
   "the ball is not on racket the value should have been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-velocity-not-negative? : Ball -> Boolean
;; GIVEN: the state of a ball b
;; RETURNS: true iff, the balls velocity is not negative, otherwise false

;; EXAMPLES:
;; (ball-velocity-not-negative? INITIAL-RALLY-WORLD-BALL)
;;  = false
;; (ball-velocity-not-negative? NEXT-BALL-FRONT-COLLIDE-Y-6)
;;  = true

;; DESIGN STRATEGY: use simple function for comparison

(define (ball-velocity-not-negative? b)
  (>= (ball-vy b) 0))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-velocity-not-negative? INITIAL-RALLY-WORLD-BALL)
   false
   "the ball vy component of velocity is positive the value should have been
false")
  (check-equal?
   (ball-velocity-not-negative? NEXT-BALL-FRONT-COLLIDE-Y-6) 
   true
   "the ball vy component of velocity is positive the value should have been
true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-racket-collide-call? : Ball Racket -> Boolean
;; GIVEN: the state of a ball b and racket r, in rally state world
;; RETURNS: true iff, ball temporary position and current position path
;;          intersect each other

;; EXAMPLES:
;; (ball-racket-collide-call? SINGLE-BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
;;  = true

;; DESIGN STRATEGY: use simple function

(define (ball-racket-collide-call? b r)
  (ball-racket-collide? (ball-x b)
                        (ball-y b)
                        (tentative-pos (ball-x b)
                                       (ball-vx b))
                        (tentative-pos (ball-y b)
                                       (ball-vy b))
                        (racket-left-x (racket-x r))
                        (racket-right-x (racket-x r))
                        (racket-y r)))
  
;; TESTS:
(begin-for-test
  (check-equal?
   (ball-racket-collide-call? SINGLE-BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
   true
   "the ball and the racket collide with each other the value should have been
true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-after-racket-collide : Balls Racket -> Balls
;; GIVEN: a list of the balls that are present in the world which are not
;;        disappeared and a racket
;; RETURNS: the state of the balls list on the next tick after collision
;;          with the racket

;; EXAMPLES:
;; (balls-after-racket-collide EMPTY-BALL-LIST RACKET-BALL-COLLIDE)
;;  = EMPTY-BALL-LIST
;; (balls-after-racket-collide BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
;;  = NEXT-BALLS-RACKET-COLLIDE
;; (balls-after-racket-collide RANDOM-BALL-330-384 RACKET-BALL-COLLIDE)
;;  = NEXT-RANDOM-BALL-333-375

;; DESIGN STRATEGY: use HOF foldr on bs

(define (balls-after-racket-collide bs r)
  (foldr
   ;; Ball Balls -> Balls
   ;; GIVEN: a ball and a balls list
   ;; RETURNS: the state of the balls list on the next tick after collision
   ;;          with the racket
   (lambda
       (ball base)
     (if (check-ball-racket-collide? ball r)
         (cons (ball-after-racket-collide ball r) base)
         (cons (next-state-of-ball ball) base)))
   empty
   bs))

#;(cond
    [(empty? bs) empty]
    [(check-ball-racket-collide? (first bs) r)
     (cons
      (ball-after-racket-collide (first bs) r)
      (balls-after-racket-collide (rest bs) r))]
    [else (cons
           (next-state-of-ball(first bs))
           (balls-after-racket-collide (rest bs) r))])

;; TESTS: 
(begin-for-test
  (check-equal?
   (balls-after-racket-collide EMPTY-BALL-LIST RACKET-BALL-COLLIDE)
   EMPTY-BALL-LIST
   "the empty list of balls should have retuned an empty list")
  (check-equal?
   (balls-after-racket-collide BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
   NEXT-BALLS-RACKET-COLLIDE
   "the next state list of balls after racket collision should have
been different")
  (check-equal?
   (balls-after-racket-collide RANDOM-BALL-330-384 RACKET-BALL-COLLIDE)
   NEXT-RANDOM-BALL-333-375
   "the next state list of balls after collision with the wall should have
been different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-racket-collide : Ball Racket -> Ball
;; GIVEN: the state of a ball b and racket r, in rally state world
;; RETURNS: the state of the ball after a tick when collided with the racket

;; EXAMPLES:
;; (ball-after-racket-collide SINGLE-BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
;;  = NEXT-BALL-RACKET-COLLIDE 

;; DESIGN STRATEGY: use of constructor template for Ball on b

(define (ball-after-racket-collide b r)
  (make-ball (ball-x b)
             (ball-y b)
             (ball-vx b)
             (- (racket-vy r) (ball-vy b))))

;; TESTS:
(begin-for-test
  (check-equal?
   (ball-after-racket-collide SINGLE-BALL-RACKET-COLLIDE RACKET-BALL-COLLIDE)
   NEXT-BALL-RACKET-COLLIDE
   "the state was ball after ball and racket collision should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tentative-pos : Integer Integer -> Integer
;; GIVEN: two integers o and t
;; RETURNS: sum of two given integers

;; EXAMPLES:
;; (tentative-pos 5 6) = 11

;; DESIGN STRATEGY: transcribe formula

(define (tentative-pos o t)
  (+ o t))

;; TESTS:
(begin-for-test
  (check-equal?
   (tentative-pos 5 6)
   11
   "the tentative position calculated is not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-left-x : Integer -> Integer
;; GIVEN: the x position of the racket 
;; RETURNS: the x position of the extreme left of the racket,
;;          with given width 47
;; the racket's position corresponds to the center of its displayed rectangle

;; EXAMPLES:
;; (racket-left-x 30) = 6.5

;; DESIGN STRATEGY: transcribe formula 

(define (racket-left-x x)
  (- x RACKET-HALF-WIDTH))

;; TESTS:
(begin-for-test
  (check-equal? (racket-left-x 30)
                6.5
                "the value of the left position of racket is not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-right-x : Integer -> Integer
;; GIVEN: the x position of the racket 
;; RETURNS: the x position of the extreme right of the racket,
;;          with given width 47
;; the racket's position corresponds to the center of its displayed rectangle

;; EXAMPLES:
;; (racket-right-x 20) = 43.5

;; DESIGN STRATEGY: transcribe formula

(define (racket-right-x x)
  (+ x RACKET-HALF-WIDTH))

;; TESTS:
(begin-for-test
  (check-equal? (racket-right-x 20)
                43.5
                "the value of the right position of racket is not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-racket-collide? : Integer Integer Integer Integer Integer Integer
;;                        Integer -> Boolean
;; GIVEN: x, y and tentative x , y position of the ball and the position of the
;;        racket extreme points
;; the ball's position corresponds to the center of its displayed circle, and
;; the racket's position corresponds to the center of its displayed rectangle
;; and the rectangle is considered as a line segment
;; where the racket as a segment and the segment joining the points of
;; position of ball and tentative position of ball

;; EXAMPLES:
;; (ball-racket-collide? 4 1 6 6 10 3 3) = true
;; (ball-racket-collide? 10 15 10 20 5 6 3) = false

;; DESIGN STRATEGY: combine simpler functions

(define (ball-racket-collide? bx by btx bty rlx rrx ry)
  (and
   (not
    (equal?
     (positioning bx by btx bty rlx ry)
     (positioning bx by btx bty rrx ry)))
   (not
    (equal?
     (positioning rlx ry rrx ry bx by)
     (positioning rlx ry rrx ry btx bty)))))

;; TESTS:
(begin-for-test
  (check-equal? (ball-racket-collide? 4 1 6 6 10 3 3)
                true
                "the given points are not intersecting")
  (check-equal? (ball-racket-collide? 10 15 10 20 5 6 3)
                false
                "the given points are intersecting"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; positioning :  Integer Integer Integer Integer Integer Integer -> PosInteger
;; GIVEN: the different coordinates of the of three lines
;; RETURNS: a value to check the positioning of the coordinates of two
;;          line segments taking three at a time

;; EXAMPLES:
;; (positioning 4 1 6 6 10 3) = 2
;; (positioning 10 15 10 20 5 3) = 3
;; (positioning 10 15 10 10 10 3) = 1

;; DESIGN STRATEGY: transcribe formula 

(define (positioning x y a b r l)
  (cond
    [(= (- (* (- b y) (- r a))
           (* (- a x) (- l b))) 0) 1]
    [(> (- (* (- b y) (- r a))
           (* (- a x) (- l b))) 0) 2]
    [else 3]))

;; TESTS:
(begin-for-test
  (check-equal? (positioning 4 1 6 6 10 3)
                2
                "the value of the positioning of the points is not 2")
  (check-equal? (positioning 10 15 10 20 5 3)
                3
                "the value of the positioning of the points is not 3")
  (check-equal? (positioning 10 15 10 10 10 3)
                1
                "the value of the positioning of the points is not 1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-ball-out-of-court? : Balls -> Boolean
;; GIVEN: a list of the balls that are present in the world which are not
;;        disappeared
;; RETURNS: true iff any one of the ball in the list is out of the court,
;;          false otherwise

;; EXAMPLES:
;; (any-ball-out-of-court? BALLS-FRONT-COLLIDE-Y-6) = true

;; DESIGN STRATEGY: use HOF ormap on bs

(define (any-ball-out-of-court? bs)
  (ormap
   ball-not-in-court?
   bs))

#;(cond
    [(empty? bs) false]
    [else (or
           (ball-not-in-court? (first bs))
           (any-ball-out-of-court? (rest bs)))])

;; TESTS:
(begin-for-test
  (check-equal? (any-ball-out-of-court? BALLS-FRONT-COLLIDE-Y-6)
                true
                "the ball is outside the court in the next tick the value
should have been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-not-in-court? : Ball -> Boolean
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: true iff the ball position is outside the court, false otherwise

;; EXAMPLES:
;; (ball-not-in-court? BALL-FRONT-COLLIDE-Y-6) = true
;; (ball-not-in-court? BALL-RACKET-COLLIDE) = false

;; DESIGN STRATEGY: use simpler functions

(define (ball-not-in-court? b)
  (not
   (and
   (<= 0 (tentative-pos (ball-x b) (ball-vx b)) COURT-WIDTH)
   (<= 0 (tentative-pos (ball-y b) (ball-vy b)) COURT-HEIGHT))))

;; TESTS:
(begin-for-test
  (check-equal? (ball-not-in-court? BALL-FRONT-COLLIDE-Y-6)
                true
                "the given value of ball is outside the court")
  (check-equal? (ball-not-in-court? SINGLE-BALL-RACKET-COLLIDE)
                false
                "the given value of ball is inside the court"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-wall-collide : Balls -> Balls
;; GIVEN: a list of the balls that are present in the world which are not
;;        disappeared
;; RETURNS: the list of balls after collision with different walls of the court.
;;          will return empty if the ball collides the back wall and disappears

;; EXAMPLES:
;; (balls-wall-collide BALL-BACK-COLLIDE-Y-643)
;;  = (list EMPTY-BALL-LIST)
;; (balls-wall-collide RANDOM-BALL-330-384)
;;  = NEXT-RANDOM-BALL-333-375 

;; DESIGN STRATEGY: use HOF foldr on bs

(define (balls-wall-collide bs)
  (foldr
   ;; Ball Balls -> Balls
   ;; GIVEN: a ball and a balls list
   ;; RETURNS: the list of balls after collision with different walls of
   ;;          the court, will return empty if the ball collides the back wall
   ;;          and disappears
   (lambda
       (ball base)
     (if (ball-not-in-court? ball)
         (cons (ball-wall-collide ball) base)
         (cons (next-state-of-ball ball) base)))
   empty
   bs))

#;(cond
    [(empty? bs) empty]
    [(ball-not-in-court? (first bs))
     (cons
      (ball-wall-collide (first bs))
      (balls-wall-collide (rest bs)))]
    [else (cons
           (next-state-of-ball (first bs))
           (balls-wall-collide (rest bs)))])

;; TESTS:
(begin-for-test
  (check-equal? (balls-wall-collide BALL-BACK-COLLIDE-Y-643)
                (list EMPTY-BALL-LIST)
                "the ball after collision with back wall should have been
empty")
  (check-equal? (balls-wall-collide RANDOM-BALL-330-384)
                NEXT-RANDOM-BALL-333-375
                "the ball next state after the tick should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-wall-collide : Ball -> Ball
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: the state of the ball b after a tick

;; EXAMPLES:
;; (ball-wall-collide BALL-RIGHT-COLLIDE-X-423)
;;  = NEXT-BALL-RIGHT-COLLIDE-X-423
;; (ball-wall-collide BALL-LEFT-COLLIDE-x-2)
;;  = NEXT-BALL-LEFT-COLLIDE-x-2
;; (ball-wall-collide BALL-FRONT-COLLIDE-Y-6)
;;  = NEXT-BALL-FRONT-COLLIDE-Y-6

;; DESIGN STRATEGY: cases on collision of ball with different walls

(define (ball-wall-collide b)
  (cond [(ball-collide-the-front-wall? b) (ball-after-front-wall-collision b)]
        [(ball-collide-the-left-wall? b) (ball-after-left-wall-collision b)]
        [(ball-collide-the-right-wall? b) (ball-after-right-wall-collision b)]
        [(ball-collide-the-back-wall? b) empty]))

;; TESTS:
(begin-for-test
  (check-equal? (ball-wall-collide BALL-RIGHT-COLLIDE-X-423)
                NEXT-BALL-RIGHT-COLLIDE-X-423
                "the change in the ball after collision with right wall is
not done")
  (check-equal? (ball-wall-collide BALL-LEFT-COLLIDE-x-2)
                NEXT-BALL-LEFT-COLLIDE-x-2
                "the change in the ball after collision with left wall is
not done")
  (check-equal? (ball-wall-collide BALL-FRONT-COLLIDE-Y-6)
                NEXT-BALL-FRONT-COLLIDE-Y-6
                "the change in the ball after collision with front wall is
not done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collide-the-front-wall? : Ball -> Boolean
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: true iff the ball collides with the front wall in the next tick

;; EXAMPLES:
;; (ball-collide-the-front-wall? BALL-FRONT-COLLIDE-Y-6) = true

;; DESIGN STRATEGY: use simple function for comparison

(define (ball-collide-the-front-wall? b)
  (< (tentative-pos (ball-y b) (ball-vy b)) 0))

;; TESTS:
(begin-for-test
  (check-equal? (ball-collide-the-front-wall?
                 BALL-FRONT-COLLIDE-Y-6)
                true
                "the ball collides with the front wall the value should have
been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collide-the-left-wall? : Ball -> Boolean
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: true iff the ball collides with the left wall in the next tick

;; EXAMPLES:
;; (ball-collide-the-left-wall? BALL-LEFT-COLLIDE-x-2) = true

;; DESIGN STRATEGY: use simple function for comparison

(define (ball-collide-the-left-wall? b)
  (< (tentative-pos (ball-x b) (ball-vx b)) 0))

;; TESTS:
(begin-for-test
  (check-equal? (ball-collide-the-left-wall?
                 BALL-LEFT-COLLIDE-x-2)
                true
                "the ball collides with the left wall the value should have
been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collide-the-right-wall? : Ball -> Boolean
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: true iff the ball collides with the right wall in the next tick

;; EXAMPLES:
;; (ball-collide-the-right-wall? BALL-RIGHT-COLLIDE-X-423) = true

;; DESIGN STRATEGY: use simple function for comparison

(define (ball-collide-the-right-wall? b)
  (> (tentative-pos (ball-x b) (ball-vx b)) COURT-WIDTH))

;; TESTS:
(begin-for-test
  (check-equal? (ball-collide-the-right-wall?
                 BALL-RIGHT-COLLIDE-X-423)
                true
                "the ball collides with the right wall the value should have
been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-collide-the-back-wall? : Ball -> Boolean
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: true iff the ball collides with the back wall in the next tick

;; EXAMPLES:
;; (ball-collide-the-back-wall? SINGLE-BALL-BACK-COLLIDE-Y-643) = true

;; DESIGN STRATEGY: use simple function for comparison

(define (ball-collide-the-back-wall? b)
  (> (tentative-pos (ball-y b) (ball-vy b)) COURT-HEIGHT)) 
 
;; TESTS:
(begin-for-test
  (check-equal? (ball-collide-the-back-wall?
                 SINGLE-BALL-BACK-COLLIDE-Y-643)
                true
                "the ball collides with the back wall the value should have
been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-front-wall-collision : Ball -> Ball
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: the state of ball after a tick when it collides with the front wall

;; EXAMPLES:
;; (ball-after-front-wall-collision BALL-FRONT-COLLIDE-Y-6)
;;  = NEXT-BALL-FRONT-COLLIDE-Y-6

;; DESIGN STRATEGY: use constructor template for Ball on b

(define (ball-after-front-wall-collision b)
  (make-ball (tentative-pos (ball-x b) (ball-vx b))
             (* (tentative-pos (ball-y b) (ball-vy b)) -1)
             (ball-vx b)
             (* (ball-vy b) -1)))

;; TESTS:
(begin-for-test
  (check-equal? (ball-after-front-wall-collision
                 BALL-FRONT-COLLIDE-Y-6)
                NEXT-BALL-FRONT-COLLIDE-Y-6
                "the ball after colliding with front wall should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-left-wall-collision : Ball -> Ball
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: the state of ball after a tick when it collides with the left wall

;; EXAMPLES:
;; (ball-after-left-wall-collision BALL-LEFT-COLLIDE-x-2)
;;  = NEXT-BALL-LEFT-COLLIDE-x-2

;; DESIGN STRATEGY: use constructor template for Ball on b

(define (ball-after-left-wall-collision b)
  (make-ball (* (tentative-pos (ball-x b) (ball-vx b)) -1)
             (tentative-pos (ball-y b) (ball-vy b))
             (* (ball-vx b) -1)
             (ball-vy b)))

;; TESTS:
(begin-for-test
  (check-equal? (ball-after-left-wall-collision
                 BALL-LEFT-COLLIDE-x-2)
                NEXT-BALL-LEFT-COLLIDE-x-2
                "the ball after colliding with left wall should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ball-after-right-wall-collision : Ball -> Ball
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: the state of ball after a tick when it collides with the right wall

;; EXAMPLES:
;; (ball-after-right-wall-collision BALL-RIGHT-COLLIDE-X-423)
;;  = BALL-RIGHT-COLLIDE-X-423

;; DESIGN STRATEGY: use constructor template for Ball on b

(define (ball-after-right-wall-collision b)
  (make-ball (- COURT-WIDTH
                (- (tentative-pos (ball-x b) (ball-vx b)) COURT-WIDTH))
             (tentative-pos (ball-y b) (ball-vy b))
             (* (ball-vx b) -1)
             (ball-vy b)))

;; TESTS:
(begin-for-test
  (check-equal? (ball-after-right-wall-collision
                 BALL-RIGHT-COLLIDE-X-423)
                NEXT-BALL-RIGHT-COLLIDE-X-423
                "the ball after colliding with right wall should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-state-of-balls : Balls -> Balls
;; GIVEN: a list of the balls that are present in the world which are not
;;        disappeared
;; RETURNS: the state of list of balls after a tick

;; EXAMPLE:
;; (next-state-of-balls RANDOM-BALL-330-384) = NEXT-RANDOM-BALL-333-375 

;; DESIGN STRATEGY: use HOF foldr on bs

(define (next-state-of-balls bs)
  (foldr
   ;; Ball Balls -> Balls
   ;; GIVEN: a ball and a balls list
   ;; RETURNS: the state of list of balls after a tick
   (lambda
       (ball base)
     (cons (next-state-of-ball ball) base))
   empty
   bs))

#;(cond
    [(empty? bs) empty]
    [else (cons
           (next-state-of-ball (first bs))
           (next-state-of-balls (rest bs)))])

;; TESTS:
(begin-for-test
  (check-equal? (next-state-of-balls
                 RANDOM-BALL-330-384)
                NEXT-RANDOM-BALL-333-375
                "the next state of the balls in the rally state should have
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-state-of-ball : Ball -> Ball
;; GIVEN: the state of a ball b in rally state world
;; RETURNS: the state of the ball b after a tick

;; EXAMPLES:
;; (next-state-of-ball RANDOM-SINGLE-BALL-330-384)
;;  = NEXT-RANDOM-SINGLE-BALL-333-375

;; DESIGN STRATEGY: use constructor template for Ball on b

(define (next-state-of-ball b)
  (make-ball (tentative-pos (ball-x b) (ball-vx b))
             (tentative-pos (ball-y b) (ball-vy b))
             (ball-vx b)
             (ball-vy b)))

;; TESTS:
(begin-for-test
  (check-equal? (next-state-of-ball
                 RANDOM-SINGLE-BALL-330-384)
                NEXT-RANDOM-SINGLE-BALL-333-375
                "the next state of the ball in the rally state should have
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-tick : Racket Ball -> Racket
;; GIVEN: the state of a racket r and a ball b, in the rally state world
;; RETURNS: the state of the racket after a tick

;; EXAMPLES:
;; (racket-after-tick RACKET-BALL-COLLIDE BALL-RACKET-COLLIDE)
;;  = NEXT-RACKET-BALL-COLLIDE
;; (racket-after-tick RACKET-RIGHT-COLLIDE-X-424 RANDOM-BALL-330-384)
;; = NEXT-RACKET-RIGHT-COLLIDE-X-424
;; (racket-after-tick RANDOM-RACKET-149-384 RANDOM-BALL-330-384)
;;  = NEXT-RANDOM-RACKET-148-384
;; (racket-after-tick SELECTED-RANDOM-RACKET-152-384 RANDOM-BALL-330-384)
;;  = SELECTED-RANDOM-RACKET-152-384
;; (racket-after-tick RACKET-BALL-COLLIDE BALL-RACKET-SECOND-COLLIDE)
;;  = RACKET-BALL-COLLIDE

;; DESIGN STRATEGY: combine simpler functions

(define (racket-after-tick r bs)
  (cond [(any-ball-racket-collide? bs (racket-on-next-tick r))
         (racket-after-ball-collide (racket-on-next-tick r))]
        [(not (racket-in-court? r))
         (racket-wall-collide r)]
        [(racket-selected? r)
         (racket-after-selected r)]
        [else (next-state-of-racket r)]))

;; TESTS:
(begin-for-test
  (check-equal? (racket-after-tick RACKET-BALL-COLLIDE BALL-RACKET-COLLIDE)
                NEXT-RACKET-BALL-COLLIDE
                "the change in racket after collision with ball is not done")
  (check-equal? (racket-after-tick
                 RACKET-RIGHT-COLLIDE-X-424
                 RANDOM-BALL-330-384)
                NEXT-RACKET-RIGHT-COLLIDE-X-424
                "the change in racket after collision with right wall is
not done")
  (check-equal? (racket-after-tick
                 RANDOM-RACKET-149-384
                 RANDOM-BALL-330-384)
                NEXT-RANDOM-RACKET-148-384
                "the change in the racket in the rally state is not done")
  (check-equal? (racket-after-tick
                 SELECTED-RANDOM-RACKET-152-384
                 RANDOM-BALL-330-384)
                SELECTED-RANDOM-RACKET-152-384
                "the change in the racket when selected in the rally
state, is not done")
  (check-equal? (racket-after-tick
                 RACKET-BALL-COLLIDE
                 BALL-RACKET-SECOND-COLLIDE)
                RACKET-BALL-COLLIDE
                "the change in the racket in the rally state after first
collision with the ball is not done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-selected : Racket -> Racket
;; GIVEN: the state of racket r in rally state world when selected
;; RETURNS: the state of the racket r after a tick

;; EXAMPLES:
;; (racket-after-selected SELECTED-RANDOM-RACKET-152-384)
;;  = SELECTED-RANDOM-RACKET-152-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-selected r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (racket-vy r)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-after-selected
                 SELECTED-RANDOM-RACKET-152-384)
                SELECTED-RANDOM-RACKET-152-384
                "the change in the racket when selected in the rally
state, is not done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-state-of-racket : Racket -> Racket
;; GIVEN: the state of racket r in rally state
;; RETURNS: the next state of the racket r after a tick

;; EXAMPLES:
;; (next-state-of-racket RANDOM-RACKET-149-384) = NEXT-RANDOM-RACKET-148-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (next-state-of-racket r)
  (make-racket (tentative-pos (racket-x r) (racket-vx r))
               (tentative-pos (racket-y r) (racket-vy r))
               (racket-vx r)
               (racket-vy r)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? (next-state-of-racket
                 RANDOM-RACKET-149-384)
                NEXT-RANDOM-RACKET-148-384
                "the change in the racket in the rally state is not done"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-ball-collide : Racket -> Racket
;; GIVEN: the state of racket r in rally state world
;; RETURNS: the state of the racket r after a tick

;; EXAMPLES:
;; (racket-after-ball-collide RACKET-BALL-COLLIDE-NEG-VY)
;;  = NEXT-RACKET-BALL-COLLIDE-NEG-VY
;; (racket-after-ball-collide RANDOM-RACKET-149-384) = RANDOM-RACKET-149-384

;; DESIGN STRATEGY: case on the value of vy component of racket

(define (racket-after-ball-collide r)
  (if (< (racket-vy r) 0)
      (racket-vy-less-than-0 r)
      (racket-vy-greater-than-0 r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-after-ball-collide RACKET-BALL-COLLIDE-NEG-VY)
                NEXT-RACKET-BALL-COLLIDE-NEG-VY
                "the change in racket is not done after the ball collision")
  (check-equal? (racket-after-ball-collide RANDOM-RACKET-149-384)
                PREVIOUS-RANDOM-RACKET-149-384
                "the racket is changed even when not collided with the ball"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-vy-less-than-0 : Racket -> Racket
;; GIVEN: the state of racket r in when collided with the ball and vy
;;        less than zero
;; RETURNS: the next state of the racket r after the collision happens

;; EXAMPLES:
;; (racket-vy-less-than-0 RACKET-BALL-COLLIDE-NEG-VY)
;;  = NEXT-RACKET-BALL-COLLIDE-NEG-VY

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-vy-less-than-0 r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               0
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-vy-less-than-0 RACKET-BALL-COLLIDE-NEG-VY)
                NEXT-RACKET-BALL-COLLIDE-NEG-VY
                "the racket with vy component less than zero should have
resulted different state of racket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-vy-greater-than-0 : Racket -> Racket
;; GIVEN: the state of racket r in when collided with the ball and vy
;;        greater than zero
;; RETURNS: the next state of the racket r after the collision happens

;; EXAMPLES:
;; (racket-vy-greater-than-0 RANDOM-RACKET-149-384)
;;  = PREVIOUS-RANDOM-RACKET-149-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-vy-greater-than-0 r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (racket-vy r)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
(check-equal? (racket-vy-greater-than-0 RANDOM-RACKET-149-384)
              PREVIOUS-RANDOM-RACKET-149-384
              "the racket with vy component greater than or equal zero should
have resulted different state of racket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-in-court? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the racket position in graphical coordinate is inside
;;          the court, false otherwise

;; EXAMPLES:
;; (racket-in-court? RACKET-RIGHT-COLLIDE-X-424) = false
;; (racket-in-court? RACKET-BALL-COLLIDE) = true

;; DESIGN STRATEGY: use simpler functions

(define (racket-in-court? r)
  (and
   (racket-x-in-court? r)
   (racket-y-in-court? r)))

;;TESTS:
(begin-for-test
  (check-equal? (racket-in-court? RACKET-RIGHT-COLLIDE-X-424)
                false
                "the given value of racket is outside the court")
  (check-equal? (racket-in-court? RACKET-BALL-COLLIDE)
                true
                "the given value of racket is inside the court"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-x-in-court? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the rackets x position is inside the court, false otherwise

;; EXAMPLES:
;; (racket-x-in-court? RACKET-BALL-COLLIDE) = true

;; DESIGN STRATEGY: use simpler functions

(define (racket-x-in-court? r)
  (<= 0
      (- (tentative-pos (racket-x r) (racket-vx r))
         RACKET-HALF-WIDTH)
      (+ (tentative-pos (racket-x r) (racket-vx r))
         RACKET-HALF-WIDTH)
      COURT-WIDTH))

;; TESTS:
(begin-for-test
  (check-equal? (racket-x-in-court? RACKET-BALL-COLLIDE)
                true
                "the given value of racket x position is inside the court"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-y-in-court? : Racket -> Boolean
;; GIVEN: a racket
;; RETURNS: true iff the rackets y position is inside the court, false otherwise

;; EXAMPLES:
;; (racket-y-in-court? RACKET-BALL-COLLIDE) = true

;; DESIGN STRATEGY: use simple function for comparison

(define (racket-y-in-court? r)
  (<= 0
      (tentative-pos (racket-y r) (racket-vy r))
      COURT-HEIGHT))

;; TESTS:
(begin-for-test
  (check-equal? (racket-y-in-court? RACKET-BALL-COLLIDE)
                true
                "the given value of racket y position is inside the court"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; racket-wall-collide : Racket -> Racket
;; GIVEN: the state of racket r in rally state world
;; RETURNS: the state of the racket r after a tick

;; EXAMPLES:
;; (racket-wall-collide RACKET-RIGHT-COLLIDE-X-424)
;;  = NEXT-RACKET-RIGHT-COLLIDE-X-424
;; (racket-wall-collide RACKET-LEFT-COLLIDE-X-3)
;;  = NEXT-RACKET-LEFT-COLLIDE-X-3

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-wall-collide r)
  (if (racket-collide-left-wall? r)
      (racket-after-left-wall-collision r)
      (racket-after-right-wall-collision r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-wall-collide
                 RACKET-RIGHT-COLLIDE-X-424)
                NEXT-RACKET-RIGHT-COLLIDE-X-424
                "the next racket state after collision with right wall is
not correct")
  (check-equal? (racket-wall-collide
                 RACKET-LEFT-COLLIDE-X-3)
                NEXT-RACKET-LEFT-COLLIDE-X-3
                "the next racket state after collision with left wall is
not correct"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-collide-left-wall? : Racket -> Boolean
;; GIVEN: a Racket
;; RETURNS: true iff the racket collides with the left wall

;; EXAMPLES:
;; (racket-collide-left-wall? RACKET-LEFT-COLLIDE-X-3) = TRUE

;; DESIGN STRATEGY: use simple function for comparison

(define (racket-collide-left-wall? r)
  (<
   (- (tentative-pos (racket-x r) (racket-vx r)) RACKET-HALF-WIDTH)
   0))

;; TESTS:
(begin-for-test
  (check-equal? (racket-collide-left-wall?
                 RACKET-LEFT-COLLIDE-X-3)
                true
                "the racket should have been true as it collides with the
left wall"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-left-wall-collision : Racket -> Racket
;; GIVEN: the state of racket r in when collided with the left wall in next tick
;; RETURNS: the next state of the racket r after the collision happens

;; EXAMPLES:
;; (racket-after-left-wall-collision RACKET-LEFT-COLLIDE-X-3)
;;  = NEXT-RACKET-LEFT-COLLIDE-X-3

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-left-wall-collision r)
  (make-racket (round RACKET-HALF-WIDTH)
               (tentative-pos (racket-y r) (racket-vy r))
               (racket-vx r)
               (racket-vy r)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-after-left-wall-collision
                 RACKET-LEFT-COLLIDE-X-3)
                NEXT-RACKET-LEFT-COLLIDE-X-3
                "the next racket state after collision with left wall is
not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-right-wall-collision : Racket -> Racket
;; GIVEN: the state of racket r in when collided with the right wall in
;;        next tick
;; RETURNS: the next state of the racket r after the collision happens

;; EXAMPLES:
;; (racket-after-right-wall-collision RACKET-RIGHT-COLLIDE-X-424)
;;  = NEXT-RACKET-RIGHT-COLLIDE-X-424

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-right-wall-collision r)
  (make-racket (round (- COURT-WIDTH RACKET-HALF-WIDTH))
               (tentative-pos (racket-y r) (racket-vy r))
               (racket-vx r)
               (racket-vy r)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? (racket-after-right-wall-collision RACKET-RIGHT-COLLIDE-X-424)
                NEXT-RACKET-RIGHT-COLLIDE-X-424
                "the next racket state after collision with right wall is
not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: a world
;; Return: a scene that portrays the given world

;; EXAMPLES:
;; (world-to-scene WORLD-READY-TO-SERVE) = IMAGE-RANDOM-PAUSED-149-384
;; (world-to-scene WORLD-RALLY-STATE) = IMAGE-RANDOM-RALLY

;; DESIGN STRATEGY: combine simpler functions

(define (world-to-scene w)
  (scene-with-balls
   (world-balls w)
   (scene-with-racket
    (world-racket w)
    (empty-scene COURT-WIDTH COURT-HEIGHT (world-bg-color w)))))

;; TESTS:

;; NOTE: these only test whether world-to-scene calls place-image properly
;; it doesn't check to see whether that's the right image!

(begin-for-test
  (check-equal? 
   (world-to-scene WORLD-READY-TO-SERVE)
   IMAGE-RANDOM-PAUSED-149-384
   "(world-to-scene WORLD-READY-TO-SERVE) should display as
IMAGE-RANDOM-PAUSED-149-384")
  (check-equal? 
   (world-to-scene WORLD-RALLY-STATE)
   IMAGE-RANDOM-RALLY
   "(world-to-scene WORLD-RALLY-STATE) should display as
IMAGE-RANDOM-RALLY"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scene-with-balls : Balls Scene -> Scene
;; GIVEN: a list of the balls that are present in the world and a scene
;; RETURNS: a scene like the given one, but with the given balls painted on it

;; EXAMPLES:
;; (scene-with-balls EMPTY-BALL-LIST RACKET-SCENE-IMAGE)
;;  = RACKET-SCENE-IMAGE
;; (scene-with-balls MULTIPLE-LIST-RANDOM-BALL RACKET-SCENE-IMAGE)
;;  = MULTIPLE-BALL-SCENE-IMAGE

;; DESIGN STRATEGY: use HOF foldr on bs

(define (scene-with-balls bs s)
  (foldr
   scene-with-ball
   s
   bs))

#;(cond
    [(empty? bs) s]
    [else (scene-with-ball (first bs) (scene-with-balls (rest bs) s))])

;; TESTS:
(begin-for-test
  (check-equal? 
   (scene-with-balls EMPTY-BALL-LIST RACKET-SCENE-IMAGE)
   RACKET-SCENE-IMAGE
   "(scene-with-balls EMPTY-BALL-LIST RACKET-SCENE-IMAGE) should
display as RACKET-SCENE-IMAGE")
  (check-equal? 
   (scene-with-balls MULTIPLE-LIST-RANDOM-BALL RACKET-SCENE-IMAGE)
   MULTIPLE-BALL-SCENE-IMAGE
   "(scene-with-balls MULTIPLE-LIST-RANDOM-BALL RACKET-SCENE-IMAGE) should
display as MULTIPLE-BALL-SCENE-IMAGE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scene-with-ball : Ball Scene -> Scene
;; GIVEN: a ball and a scene
;; RETURNS: a scene like the given one, but with the given ball painted on it

;; EXAMPLES:
;; (scene-with-ball RANDOM-SINGLE-BALL-330-384 BALL-IMAGE-SCENE)
;;  = IMAGE-RANDOM-PAUSED

;; DESIGN STRATEGY: use simple function

(define (scene-with-ball b s)
  (place-image BALL-IMAGE
               (ball-x b)
               (ball-y b)
               s))

;; TESTS:

;; NOTE: these only test whether scene-with-ball calls place-image properly
;; it doesn't check to see whether that's the right image!

(begin-for-test
  (check-equal? 
   (scene-with-ball RANDOM-SINGLE-BALL-330-384 BALL-IMAGE-SCENE)
   IMAGE-RANDOM-PAUSED
   "(scene-with-ball RANDOM-SINGLE-BALL-330-384 BALL-IMAGE-SCENE) should
display as IMAGE-RANDOM-PAUSED"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scene-with-racket : Racket Scene -> Scene
;; GIVEN: a racket and a scene
;; RETURNS: a scene like the given one, but with the given racket painted on it

;; EXAMPLES:
;; (scene-with-racket RANDOM-RACKET-149-384 RACKET-IMAGE-SCENE)
;; = RACKET-SCENE-IMAGE
;; (scene-with-racket SELECTED-RANDOM-RACKET-152-384-140-200
;;                    RACKET-IMAGE-SCENE) = RACKET-BALL-SCENE-IMAGE

;; DESIGN STRATEGY: case on value of racket-selected? on r

(define (scene-with-racket r s)
  (if (racket-selected? r)
      (scene-with-racket-selected r s)
      (scene-with-racket-not-selected r s)))

;; TESTS: 

;; NOTE: these only test whether scene-with-racket calls place-image properly
;; it doesn't check to see whether that's the right image!

(begin-for-test
  (check-equal? 
   (scene-with-racket RANDOM-RACKET-149-384 RACKET-IMAGE-SCENE)
   RACKET-SCENE-IMAGE 
   "(scene-with-ball RANDOM-RACKET-149-384 RACKET-IMAGE-SCENE) should display as
RACKET-SCENE-IMAGE")
  (check-equal? 
   (scene-with-racket SELECTED-RANDOM-RACKET-152-384-140-200 RACKET-IMAGE-SCENE)
   RACKET-BALL-SCENE-IMAGE
   "(scene-with-ball SELECTED-RANDOM-RACKET-152-384-140-200 RACKET-IMAGE-SCENE)
should display as RACKET-BALL-SCENE-IMAGE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scene-with-racket-not-selected : Racket Scene -> Scene
;; GIVEN: a racket not selected by mouse and a scene
;; RETURNS: a scene like the given one, but with the given racket painted on it

;; EXAMPLES:
;; (scene-with-racket-not-selected RANDOM-RACKET-149-384 RACKET-IMAGE-SCENE)
;;  = RACKET-SCENE-IMAGE

;; DESIGN STRATEGY: use simple function

(define (scene-with-racket-not-selected r s)
  (place-image RACKET-IMAGE
               (racket-x r)
               (racket-y r)
               s))

;; TESTS:
(begin-for-test
(check-equal? 
   (scene-with-racket-not-selected RANDOM-RACKET-149-384 RACKET-IMAGE-SCENE)
   RACKET-SCENE-IMAGE 
   "(scene-with-racket-not-selected r s) should display as RACKET-SCENE-IMAGE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scene-with-racket-selected : Racket Scene -> Scene
;; GIVEN: a racket selected by mouse and a scene
;; RETURNS: a scene like the given one, but with the given racket painted on it

;; EXAMPLES:
;; (scene-with-racket-selected SELECTED-RANDOM-RACKET-152-384-140-200
;;                             RACKET-IMAGE-SCENE)
;;  = RACKET-BALL-SCENE-IMAGE

;; DESIGN STRATEGY: combine simpler function

(define (scene-with-racket-selected r s)
  (place-image POINTER-CIRCLE-IMAGE
                   (racket-mx r)
                   (racket-my r)
                   (scene-with-racket-not-selected r s)))

;; TESTS:
(begin-for-test
(check-equal? 
   (scene-with-racket-selected SELECTED-RANDOM-RACKET-152-384-140-200
                               RACKET-IMAGE-SCENE)
   RACKET-BALL-SCENE-IMAGE
   "(scene-with-ball SELECTED-RANDOM-RACKET-152-384-140-200 RACKET-IMAGE-SCENE)
should display as RACKET-BALL-SCENE-IMAGE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world after the
;;          given key event

;; EXAMPLES:
;; (world-after-key-event WORLD-READY-TO-SERVE STATE-KEY-EVENT)
;;  = NEXT-WORLD-READY-TO-SERVE
;; (world-after-key-event WORLD-RALLY-STATE LEFT-ARROW-KEY-EVENT)
;;  = NEXT-ON-LEFT-KEY
;; (world-after-key-event WORLD-READY-TO-SERVE OTHER-EVENT)
;;  = WORLD-READY-TO-SERVE

;; DESIGN STRATEGY: combine simpler function

(define (world-after-key-event w kev)
  (cond [(key=? kev STATE-KEY-EVENT) (next-world-state w)]
        [(string=? (world-state w) RALLY)
         (world-after-key-event-rally-state w kev)]
        [else w]))

;; TESTS:
(begin-for-test
  (check-equal? 
   (world-after-key-event WORLD-READY-TO-SERVE STATE-KEY-EVENT)
   NEXT-WORLD-READY-TO-SERVE
   "the world is not changed from ready-to-serve to rally after
STATE-KEY-EVENT")
  (check-equal? 
   (world-after-key-event WORLD-RALLY-STATE LEFT-ARROW-KEY-EVENT)
   NEXT-ON-LEFT-KEY
   "the world in rally state and left key event is not decreasing the
racket vx component by 1")
  (check-equal? 
   (world-after-key-event WORLD-READY-TO-SERVE OTHER-EVENT)
   WORLD-READY-TO-SERVE
   "the world is not changed after a key event, which is not described"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event-rally-state : World KeyEvent -> World
;; GIVEN: a world w in rally state
;; RETURNS: the world that should follow the given world after the
;;          given key event

;; EXAMPLES:
;; (world-after-key-event-rally-state WORLD-RALLY-STATE LEFT-ARROW-KEY-EVENT)
;;   = NEXT-ON-LEFT-KEY

;; DESIGN STRATEGY: use constructor template for World on w

(define (world-after-key-event-rally-state w kev)
(make-world (balls-after-key-event (world-balls w) kev)
            (racket-after-key-event (world-racket w) kev)
            (world-state w)
            (world-bg-color w)
            (world-tick-counter w)
            (world-simulation-speed w)))

;; TESTS:
(begin-for-test
(check-equal? 
   (world-after-key-event-rally-state WORLD-RALLY-STATE LEFT-ARROW-KEY-EVENT)
   NEXT-ON-LEFT-KEY
   "the world in rally state and left key event is not decreasing the
racket vx component by 1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-world-state : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the given world after change
;;          in world-state after the key event

;; EXAMPLES:
;; (next-world-state WORLD-READY-TO-SERVE) = NEXT-WORLD-READY-TO-SERVE
;; (next-world-state WORLD-RALLY-STATE) = NEXT-RALLY-WORLD-STATE
;; (next-world-state WORLD-PAUSED-COUNTER-C-4-S-1) = NEXT-PAUSED-COUNTER-C-4-S-1

;; DESIGN STRATEGY: cases on world-state of w

(define (next-world-state w)
  (cond [(string=? (world-state w) READY-TO-SERVE)
         (world-after-ready-to-serve-state w)]
        [(string=? (world-state w) RALLY)
         (world-after-rally-state w)]
        [(string=? (world-state w) PAUSED)
         (world-after-key-paused-state w)]))

;; TESTS:
(begin-for-test
  (check-equal? 
   (next-world-state WORLD-READY-TO-SERVE)
   NEXT-WORLD-READY-TO-SERVE
   "the world is not changed from ready-to-serve to rally")
  (check-equal? 
   (next-world-state WORLD-RALLY-STATE)
   NEXT-RALLY-WORLD-STATE
   "the world is not changed from rally to paused")
  (check-equal? 
   (next-world-state WORLD-PAUSED-COUNTER-C-4-S-1)
   NEXT-WORLD-PAUSED-COUNTER-C-4-S-1
   "the world is not changed from paused to ready-to-serve"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-ready-to-serve-state : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the world in ready-to-serve state

;; EXAMPLES:
;; (world-after-ready-to-serve-state WORLD-READY-TO-SERVE)
;;  = NEXT-WORLD-READY-TO-SERVE

;; DESIGN STRATEGY: use constructor template for World on w

(define (world-after-ready-to-serve-state w)
  (make-world INITIAL-RALLY-WORLD-BALLS
              (world-racket w)
              RALLY
              (world-bg-color w)
              (world-tick-counter w)
              (world-simulation-speed w)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (world-after-ready-to-serve-state WORLD-READY-TO-SERVE)
   NEXT-WORLD-READY-TO-SERVE
   "the world is not changed from ready-to-serve to rally"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-rally-state : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the world in rally state

;; EXAMPLES:
;; (world-after-rally-state WORLD-RALLY-STATE)
;;  = NEXT-RALLY-WORLD-STATE

;; DESIGN STRATEGY: use constructor template for World on w

(define (world-after-rally-state w)
  (make-world (world-balls w)
              (world-racket w)
              PAUSED
              (world-bg-color w)
              (world-tick-counter w)
              (world-simulation-speed w)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (world-after-rally-state WORLD-RALLY-STATE)
   NEXT-RALLY-WORLD-STATE
   "the world is not changed from rally to paused"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-paused-state : World -> World
;; GIVEN: a world w
;; RETURNS: the world that should follow the world in key paused state

;; EXAMPLES:
;; (world-after-key-paused-state WORLD-PAUSED-COUNTER-C-4-S-1)
;;  = NEXT-WORLD-PAUSED-COUNTER-C-4-S-1

;; DESIGN STRATEGY: use constructor template for World on w

(define (world-after-key-paused-state w)
  (make-world (world-balls w)
              (world-racket w)
              READY-TO-SERVE
              (world-bg-color w)
              (world-tick-counter w)
              (world-simulation-speed w)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (next-world-state WORLD-PAUSED-COUNTER-C-4-S-1)
   NEXT-WORLD-PAUSED-COUNTER-C-4-S-1
   "the world is not changed from paused to ready-to-serve"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; balls-after-key-event : Balls KeyEvent -> Balls
;; GIVEN: balls as a list and a description of a key event
;; RETURNS: the balls as a list that should follow the given key event

;; EXAMPLES:
;; (balls-after-key-event RANDOM-BALL-330-384 B-KEY-EVENT)
;;  = B-KEY-RANDOM-BALL-330-384

;; DESIGN STRATEGY: case on B-KEY-EVENT of kev

(define (balls-after-key-event bs kev)
  (if (key=? kev B-KEY-EVENT)
      (append bs INITIAL-RALLY-WORLD-BALLS)
      bs))

;; TESTS:
(begin-for-test
  (check-equal? 
   (balls-after-key-event RANDOM-BALL-330-384 B-KEY-EVENT)
   B-KEY-RANDOM-BALL-330-384
   "the B key event is not appending a new value to the list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-key-event : Racket KeyEvent -> Racket
;; GIVEN: a racket and a description of a key event
;; RETURNS: the racket that should follow the given key event

;; EXAMPLES:
;; (world-after-key-event WORLD-RALLY-STATE RIGHT-ARROW-KEY-EVENT)
;;  = NEXT-ON-RIGHT-KEY
;; (world-after-key-event WORLD-RALLY-STATE UP-ARROW-KEY-EVENT)
;;  = NEXT-ON-UP-KEY
;; (world-after-key-event WORLD-RALLY-STATE DOWN-ARROW-KEY-EVENT)
;;  = NEXT-ON-DOWN-KEY

;; DESIGN STRATEGY: cases on different event of kev

(define (racket-after-key-event r kev)
  (cond [(key=? kev LEFT-ARROW-KEY-EVENT)
         (racket-after-left-arrow-key-event r)]
        [(key=? kev RIGHT-ARROW-KEY-EVENT)
         (racket-after-right-arrow-key-event r)]
        [(key=? kev UP-ARROW-KEY-EVENT)
         (racket-after-up-arrow-key-event r)]
        [(key=? kev DOWN-ARROW-KEY-EVENT)
         (racket-after-down-arrow-key-event r)]
        [else r]))

;; TESTS:
(begin-for-test
  (check-equal? 
   (world-after-key-event WORLD-RALLY-STATE RIGHT-ARROW-KEY-EVENT)
   NEXT-ON-RIGHT-KEY
   "the world in rally state and left key event is not increasing the
racket vx component by 1")
  (check-equal? 
   (world-after-key-event WORLD-RALLY-STATE UP-ARROW-KEY-EVENT)
   NEXT-ON-UP-KEY
   "the world in rally state and up key event is not decreasing the
racket vy component by 1")
  (check-equal? 
   (world-after-key-event WORLD-RALLY-STATE DOWN-ARROW-KEY-EVENT)
   NEXT-ON-DOWN-KEY
   "the world in rally state and down key event is not increasing the
racket vy component by 1")
  (check-equal? 
   (world-after-key-event WORLD-RALLY-STATE OTHER-EVENT)
   WORLD-RALLY-STATE
   "the world in rally state and other event is not giving the same world as
input"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-left-arrow-key-event : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the left key event

;; EXAMPLES:
;; (racket-after-left-arrow-key-event RANDOM-RACKET-149-384)
;;  = LEFT-RANDOM-RACKET-149-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-left-arrow-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (- (racket-vx r) 1)
               (racket-vy r)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
(check-equal? 
   (racket-after-left-arrow-key-event RANDOM-RACKET-149-384)
   LEFT-RANDOM-RACKET-149-384
   "the racket after pressing the left arrow key event should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-right-arrow-key-event : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the right key event

;; EXAMPLES:
;; (racket-after-right-arrow-key-event RANDOM-RACKET-149-384)
;;  = RIGHT-RANDOM-RACKET-149-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-right-arrow-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (+ (racket-vx r) 1)
               (racket-vy r)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-after-right-arrow-key-event RANDOM-RACKET-149-384)
   RIGHT-RANDOM-RACKET-149-384
   "the racket after pressing the right arrow key event should have been
different"))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-up-arrow-key-event : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the up key event

;; EXAMPLES:
;; (racket-after-up-arrow-key-event RANDOM-RACKET-149-384)
;;  = UP-RANDOM-RACKET-149-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-up-arrow-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (- (racket-vy r) 1)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-after-up-arrow-key-event RANDOM-RACKET-149-384)
   UP-RANDOM-RACKET-149-384
   "the racket after pressing the up arrow key event should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-down-arrow-key-event : Racket -> Racket
;; GIVEN: a racket
;; RETURNS: the racket that should follow the down key event

;; EXAMPLES:
;; (racket-after-down-arrow-key-event RANDOM-RACKET-149-384)
;;  = DOWN-RANDOM-RACKET-149-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-down-arrow-key-event r)
  (make-racket (racket-x r)
               (racket-y r)
               (racket-vx r)
               (+ (racket-vy r) 1)
               (racket-mx r)
               (racket-my r)
               (racket-selected? r)))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-after-down-arrow-key-event RANDOM-RACKET-149-384)
   DOWN-RANDOM-RACKET-149-384
   "the racket after pressing the down arrow key event should have been
different"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Int Int MouseEvent -> World
;; GIVEN: a world, the x and y coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the world that should follow the given world after
;;          the given mouse event

;; EXAMPLES:
;; (world-after-mouse-event INITIAL-STATE-WORLD 6 7 BUTTON-UP-EVENT)
;;  = INITIAL-STATE-WORLD
;; (world-after-mouse-event WORLD-RALLY-STATE 200 300 BUTTON-DOWN-EVENT)
;; = AFTER-BUTTON-DOWN-EVENT 

;; DESIGN STRATEGY: use constructor template for World on w

(define (world-after-mouse-event w mx my mev)
  (if (string=? (world-state w) RALLY)
      (make-world
       (world-balls w)
       (racket-after-mouse-event (world-racket w) mx my mev)
       (world-state w)
       (world-bg-color w)
       (world-tick-counter w)
       (world-simulation-speed w))
      w))

;; TESTS:
(begin-for-test
  (check-equal? 
   (world-after-mouse-event INITIAL-STATE-WORLD 6 7 BUTTON-UP-EVENT)
   INITIAL-STATE-WORLD
   "the world is not returning the same world when not in rally state
and mouse event occurs")
  (check-equal? 
   (world-after-mouse-event WORLD-RALLY-STATE 200 300 BUTTON-DOWN-EVENT)
   AFTER-BUTTON-DOWN-EVENT
   "the world is not returning the correct world when in rally state
and button-down is pressed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;        and the mouse event
;; RETURNS: the racket as it should be after the given mouse event

;; EXAMPLES:
;; (racket-after-mouse-event SELECTED-RANDOM-RACKET-152-384 200 300 DRAG-EVENT)
;;  = NEXT-SELECTED-RANDOM-RACKET-152-384
;; (racket-after-mouse-event SELECTED-RANDOM-RACKET-152-384 200 300
;;                           OTHER-MOUSE-EVENT) = SELECTED-RANDOM-RACKET-152-384
;; (racket-after-mouse-event LEFT-RANDOM-RACKET-149-384 200 300
;;                           BUTTON-UP-EVENT) = LEFT-RANDOM-RACKET-149-384

;; DESIGN STRATEGY: cases on mouse event mev

(define (racket-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN-EVENT) (racket-after-button-down r mx my)]
    [(mouse=? mev DRAG-EVENT) (racket-after-drag r mx my)]
    [(mouse=? mev BUTTON-UP-EVENT) (racket-after-button-up r mx my)]
    [else r]))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-after-mouse-event SELECTED-RANDOM-RACKET-152-384 200 300 DRAG-EVENT)
   NEXT-SELECTED-RANDOM-RACKET-152-384
   "the world is not returning the expected Racket in rally state and
drag mouse event occurs")
  (check-equal? 
   (racket-after-mouse-event SELECTED-RANDOM-RACKET-152-384
                             200
                             300
                             OTHER-MOUSE-EVENT)
   SELECTED-RANDOM-RACKET-152-384
   "the world is not returning the expected Racket in rally state and
other mouse event occurs")
  (check-equal? 
   (racket-after-mouse-event LEFT-RANDOM-RACKET-149-384
                             200
                             300
                             BUTTON-UP-EVENT)
   LEFT-RANDOM-RACKET-149-384
   "the world is not returning the expected Racket in rally state and
button up mouse event occurs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-button-down : Racket Integer Integer -> Racket
;; GIVEN: a racket and the coordinate of the mouse when selected
;; RETURNS: the racket following a button-down at the given location

;; EXAMPLES:
;; (racket-after-button-down SELECTED-RANDOM-RACKET-152-384-140-200 140 200)
;;  = SELECTED-RANDOM-RACKET-152-384-140-200
;; (racket-after-button-down SELECTED-RANDOM-RACKET-152-384-140-200 152 384)
;;  = SELECTED-RANDOM-RACKET-152-384-152-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-button-down r mx my)
  (if (in-racket? r mx my)
      (make-racket (racket-x r)
                   (racket-y r)
                   (racket-vx r)
                   (racket-vy r)
                   mx
                   my
                   true)
      r))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-after-button-down SELECTED-RANDOM-RACKET-152-384-140-200 140 200)
   SELECTED-RANDOM-RACKET-152-384-140-200
   "the mouse outside the racket after the button-down is not correct")
  (check-equal? 
   (racket-after-button-down SELECTED-RANDOM-RACKET-152-384-140-200 152 384)
   SELECTED-RANDOM-RACKET-152-384-152-384
   "the mouse inside the racket after the button-down is not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-racket? : Racket Integer Integer -> Boolean
;; GIVEN: a racket and the coordinate of the mouse when selected
;; RETURNS: true iff the given coordinates is positioned no more than
;;         25 pixels away from the center of the racket, otherwise false

;; EXAMPLES:
;; (in-racket? SELECTED-RANDOM-RACKET-152-384-140-200 140 200) = false
;; (in-racket? SELECTED-RANDOM-RACKET-152-384-140-200 152 384) = true

;; DESIGN STRATEGY: combine simpler function

(define (in-racket? r mx my)
  (and
   (<=
    (racket-left-down-select (racket-x r))
    mx
    (racket-right-up-select (racket-x r)))
   (<=
    (racket-left-down-select (racket-y r))
    my
    (racket-right-up-select (racket-y r)))))

;; TESTS:
(begin-for-test
  (check-equal? 
   (in-racket? SELECTED-RANDOM-RACKET-152-384-140-200 140 200)
   false
   "the racket should be outside the racket")
  (check-equal? 
   (in-racket? SELECTED-RANDOM-RACKET-152-384-140-200 152 384)
   true
   "the racket should be inside the racket"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-left-down-select : Integer -> Integer
;; GIVEN: an integer p
;; RETURNS: the result after subtracting  p from MOUSE-SELECT-POS

;; EXAMPLES:
;; (racket-left-down-select 200) = 175

;; DESIGN STRATEGY: transcribe formula

(define (racket-left-down-select p)
  (- p MOUSE-SELECT-POS))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-left-down-select 200)
   175
   "the value of subtraction of integer to mouse-select-pos is not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-right-up-select : Integer -> Integer
;; GIVEN: an integer p
;; RETURNS: the result after adding p from MOUSE-SELECT-POS

;; EXAMPLES:
;; (racket-right-up-select 200) = 225

;; DESIGN STRATEGY: transcribe formula

(define (racket-right-up-select p)
  (+ p MOUSE-SELECT-POS))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-right-up-select 200)
   225
   "the value of addition of integer to mouse-select-pos is not correct"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-drag : Racket Integer Integer -> Racket
;; GIVEN: a racket and the coordinate of the mouse when selected
;; RETURNS: the racket following a drag at the given location

;; EXAMPLES:
;; (racket-after-drag RANDOM-RACKET-152-384 242 300)
;;  =  RANDOM-RACKET-152-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-drag r mx my)
  (if (racket-selected? r)
      (make-racket  (+ (racket-x r) (- mx (racket-mx r)))
                    (+ (racket-y r) (- my (racket-my r)))
                    (racket-vx r)
                   (racket-vy r)
                   mx
                   my
                   (racket-selected? r))
      r))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-after-drag RANDOM-RACKET-149-384 242 300)
   RANDOM-RACKET-149-384
   "racket not selected is not returning the same racket on mouse drag event")
  (check-equal? 
   (racket-after-drag SELECTED-RANDOM-RACKET-152-384 200 300)
   NEXT-SELECTED-RANDOM-RACKET-152-384
   "racket selected is not returning the desired racket state on mouse
drag event"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; racket-after-button-up : Racket Integer Integer -> Racket
;; GIVEN: a racket and the coordinate of the mouse when selected
;; RETURNS: the racket following a button-up at the given location

;; EXAMPLES:
;; (racket-after-button-up RANDOM-RACKET-149-384 242 300)
;;  = RANDOM-RACKET-149-384
;; (racket-after-button-up SELECTED-RANDOM-RACKET-152-384 200 300)
;;  = BUTTON-UP-RANDOM-RACKET-152-384

;; DESIGN STRATEGY: use constructor template for Racket on r

(define (racket-after-button-up r mx my)
  (if (racket-selected? r)
      (make-racket (racket-x r)
                   (racket-y r)
                   (racket-vx r)
                   (racket-vy r)
                   0
                   0
                   false)
      r))

;; TESTS:
(begin-for-test
  (check-equal? 
   (racket-after-button-up RANDOM-RACKET-149-384 242 300)
   RANDOM-RACKET-149-384
   "racket not selected is not returning the same racket on
mouse button-up event")
  (check-equal? 
   (racket-after-button-up SELECTED-RANDOM-RACKET-152-384 200 300)
   BUTTON-UP-RANDOM-RACKET-152-384
   "racket selected is not returning the desired racket on
mouse button-up event"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;