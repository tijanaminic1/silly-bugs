;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname silly-bugs) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; CONSTANTS
(define E-SCENE (empty-scene 1000 500))
(define DEVY (square 20 'solid 'pink))
(define BUG (circle 20 'solid 'forestgreen))


;; coord
;; structure that has x and y coordinates (integers)
(define-struct coord (x y))

;; shot
;; shot is a structure that has
;; coord
;; ticks
(define-struct shot (los tick))

;; devy
;; devy is a structure that has
;; coord which represents the coordinates of the devy
;; health which represents the number of lives the devy has
;; ticks which represents the clock after the devy performs a jump
;; shot represents a list of shots of the devy (x and y coord)
(define-struct devy (coord health ticks shot))

;; bug
;; bug is a structure that has
;; coord which represents the coordinates of the bug
;; health which represents the number of lives the bug has
;; pause which represents the time remaining for the next big shot in ticks
;; shot represents the list of shots of the bug (x and y coord)
;; big-shot represents a bigger shot of the bug (x and y coord)
(define-struct bug (coord health pause shot big-shot))


;; world
;; world is a structure that has
;; devy which represents the developer
;; bug which represents the bug
(define-struct world (devy bug))


;; INIT-WORLD
(define INIT-WORLD (make-world (make-devy (make-coord 30 250)
                                          5
                                          0
                                          (make-shot '() 0))
                               (make-bug (make-coord 975 250)
                                         3
                                         0
                                         (make-shot '() 0)
                                         '())))


;; jump-devy
;; world -> world
;; Purpose: To perform a jump for devy
(define (jump-devy a-world)
  (make-world (make-devy (make-coord (coord-x (devy-coord (world-devy a-world)))
                                     (coord-y (devy-coord (world-devy a-world))))
                         (devy-health (world-devy a-world))
                         1
                         (devy-shot (world-devy a-world)))
              (world-bug a-world)))


;; move-devy-left
;; world -> world
;; Purpose: To move devy to the right
(define (move-devy-left a-world)
  (if (<= (coord-x (devy-coord (world-devy a-world))) 10)
      a-world
      (make-world (make-devy (make-coord (- (coord-x (devy-coord (world-devy a-world))) 20)
                                         (coord-y (devy-coord (world-devy a-world))))
                             (devy-health (world-devy a-world))
                             (devy-ticks (world-devy a-world))
                             (devy-shot (world-devy a-world)))
                  (world-bug a-world))))

;; move-devy-right
;; world -> world
;; Purpose: To move devy to the right
(define (move-devy-right a-world)
  (if (>= (coord-x (devy-coord (world-devy a-world))) 990)
      a-world
      (make-world (make-devy (make-coord (+ 20 (coord-x (devy-coord (world-devy a-world))))
                                         (coord-y (devy-coord (world-devy a-world))))
                             (devy-health (world-devy a-world))
                             (devy-ticks (world-devy a-world))
                             (devy-shot (world-devy a-world)))
                  (world-bug a-world))))


;; devy-up
;; y-coord -> y-coord
;; Purpose: To move the y coordinate of devy up
(define (devy-up y)
  (if (< y 10)
      y
      (- y 16)))

;; devy-down
;; y-coord -> y-coord
;; Purpose: To move the y coordinate of devy up
(define (devy-down y)
  (if (>= y 490)
      y
      (+ y 8)))

;; move-devy-shot
;; devy -> shot
;; Purpose: To make devy shot
  

;; process-tick
;; world -> world
;; Purpose: To process tick
(define (process-tick a-world)
  (cond [(<= 1 (devy-ticks (world-devy a-world)) 5)
         (make-world (make-devy (make-coord (coord-x (devy-coord (world-devy a-world)))
                                            (devy-up (coord-y (devy-coord (world-devy a-world)))))
                                (devy-health (world-devy a-world))
                                (add1 (devy-ticks (world-devy a-world)))
                                (devy-shot (world-devy a-world)))
                     (world-bug a-world))]
        [(< 5 (devy-ticks (world-devy a-world)) 7)
         (make-world (make-devy
                      (devy-coord (world-devy a-world))
                      (devy-health (world-devy a-world))
                      (add1 (devy-ticks (world-devy a-world)))
                      (devy-shot (world-devy a-world)))
                     (world-bug a-world))]
        [(<= 7 (devy-ticks (world-devy a-world)))
         (make-world (make-devy
                      (make-coord (coord-x (devy-coord (world-devy a-world)))
                                  (devy-down (coord-y (devy-coord (world-devy a-world)))))
                      (devy-health (world-devy a-world))
                      (add1 (devy-ticks (world-devy a-world)))
                      (devy-shot (world-devy a-world)))
                     (world-bug a-world))]
        [(= 1 (shot-tick (devy-shot (world-devy a-world))))
         (make-world (make-devy
                      (devy-coord (world-devy a-world))
                      (devy-health (world-devy a-world))
                      (devy-ticks (world-devy a-world))
                      (make-devy-shot (world-devy a-world)))
                     (world-bug a-world))]         
        [else a-world]))


;; devy-shoot
;; world -> world
;; Purpose: To create new shoot
(define (devy-shoot a-world)
  (make-world (make-devy (devy-coord (world-devy a-world))
                         (devy-health (world-devy a-world))
                         (devy-ticks (world-devy a-world))
                         (make-shot (devy-shot (world-devy a-world))
                                    1))
              (world-bug a-world)))

          
;; process-key
;; a-world key-event -> a-world
;; Purpose: To process a pressed key
(define (process-key a-world a-key)
  (cond [(key=? "w" a-key)
         (jump-devy a-world)]
        [(key=? "d" a-key)
         (move-devy-right a-world)]
        [(key=? "a" a-key)
         (move-devy-left a-world)]
        [(key=? " " a-key)
         (devy-shoot a-world)]
        #;[(key=? "up" a-key)
           (move-bug-up a-world)]
        #;[(key=? "down" a-key)
           (move-bug-down a-world)]
        #;[(key=? "\r" a-key)
           (shoot-bug a-world)]
        #;[(key=? "rshift" a-key)
           (big-shot-bug a-world)]
        [else a-world]))

;; game-over?
;; world -> boolean
;; Purpose: To determine whether the game is over
#;(define (game-over? a-world)
    (all-matched? (world-loc a-world)))


;; draw-world
;; world -> world img
(define (draw-world a-world)
  (place-image DEVY
               (coord-x (devy-coord (world-devy a-world)))
               (coord-y (devy-coord (world-devy a-world)))
               (place-image BUG
                            (coord-x (bug-coord (world-bug a-world)))
                            (coord-y (bug-coord (world-bug a-world)))
                            E-SCENE)))

;; run-function
;; symbol -> world
(define (silly-bugs a-name)
  (big-bang INIT-WORLD
    [on-draw draw-world]
    [on-key process-key]
    [on-tick process-tick]
    #;[stop-when game-over? draw-last-world]
    [name a-name]))
