#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(struct world (ybar1 ybar2 sim-count ball_present x-ball y-ball vx-ball vy-ball color quit) #:mutable) ;ball_present determines if the ball is in the world at some moment
(define WORLD (world 300 300 0 #f 500 300 0 0 "yellow" #f))

(define keystates (make-hasheq))             ;Making a hashtable storing the current status of the keys to allow for simultaneous movement of the bars.
(hash-set! keystates 'up1 #f)
(hash-set! keystates 'up2 #f)
(hash-set! keystates 'dn1 #f)
(hash-set! keystates 'dn2 #f)

(define (key-press! key) (hash-set! keystates key #t))
(define (key-unpress! key) (hash-set! keystates key #f))
;(define (key-down? key) (hash-ref keystates key #f))

(define step 8)
(define delta-t 5)
(define boxb-up 10)
(define boxb-dn 600)
(define boxb-l 10)
(define boxb-r 990)
(define RADIUS 20)
(define LENGTH 100)
(define WIDTH 50)
;(define ball (circle RADIUS "solid" (world-color WORLD)))
(define bar1 (rectangle WIDTH LENGTH "solid" "green"))
(define bar2 (rectangle WIDTH LENGTH "solid" "blue"))
(define background-image (bitmap "ping-pong.png"))

(define (main x)
  (big-bang x
          (on-tick next)        ; World -> World
          (on-key press)        ; World -> World
          (on-release release)  ; World -> World
          (to-draw draw-world)  ; World -> Image
          (stop-when stop?)))   ; World -> Boolean


; Gives the next world
(define (next WORLD)               ;Important step as it is for controlling how the world changes on each clock tick, approx 1/28 s.
  (set-world-sim-count! WORLD (+ 1 (world-sim-count WORLD)))   
  (display (world-sim-count WORLD))
  (display "\n")
  (update-bars keystates WORLD)
  (update-ball)
  (update-color)
  WORLD)

(define (update-color)             ;Color change according to speed of the ball
  (define speed-square (+ (* (world-vx-ball WORLD) (world-vx-ball WORLD)) (* (world-vy-ball WORLD) (world-vy-ball WORLD))))
  (cond
    ((< speed-square 5) (set-world-color! WORLD "yellow"))
    ((< speed-square 12) (set-world-color! WORLD "orange"))
    (else (set-world-color! WORLD "red"))))

(define (update-ball)
  (cond
    ;;checking ball
    [(and (> boxb-up (- (world-y-ball WORLD) RADIUS)) (world-ball_present WORLD))           ;Reflecting the ball from collision with the upper and lower walls
     (begin
       (set-world-y-ball! WORLD (+ boxb-up RADIUS))
       (set-world-vy-ball! WORLD (* -1 (world-vy-ball WORLD))))]
    [(and (< boxb-dn (+ (world-y-ball WORLD) RADIUS)) (world-ball_present WORLD))
     (begin
       (set-world-y-ball! WORLD (- boxb-dn RADIUS))
       (set-world-vy-ball! WORLD (* -1 (world-vy-ball WORLD))))]
    [(and (> boxb-l (- (world-x-ball WORLD) RADIUS)) (world-ball_present WORLD))
     (begin
       (display "Increase the score of right player")          ;If the ball goes out of bounds, it is made to disappear from the world
       (set-world-ball_present! WORLD #f)
       (set-world-y-ball! WORLD (/ (+ boxb-up boxb-dn) 2))
       (set-world-x-ball! WORLD (/ (+ boxb-l boxb-r) 2)))]
    [(and (< boxb-r (+ (world-x-ball WORLD) RADIUS)) (world-ball_present WORLD))
     (begin
       (display "Increase the score of left player")
       (set-world-ball_present! WORLD #f)
       (set-world-y-ball! WORLD (/ (+ boxb-up boxb-dn) 2))
       (set-world-x-ball! WORLD (/ (+ boxb-l boxb-r) 2)))])

  (cond                                                            ;Changing the ball position according to the collisions with the bars
         ((and (and (<= (- (- boxb-r WIDTH) RADIUS) (world-x-ball WORLD)) (<= (world-x-ball WORLD) (- (- boxb-r WIDTH) (/ RADIUS 2))))
              (and (>= (world-y-ball WORLD) (- (world-ybar2 WORLD) (+ (/ LENGTH 2) (/ RADIUS 3))))
                   (<= (world-y-ball WORLD) (+ (world-ybar2 WORLD) (+ (/ LENGTH 2) (/ RADIUS 3))))))
          (begin
           (display "debug: inside bar2 collision")
           (display "\n")
           (set-world-vx-ball! WORLD (* -1 (world-vx-ball WORLD)))
           (set-world-vy-ball! WORLD (+ (/ (- (world-y-ball WORLD) (world-ybar2 WORLD)) 25) (world-vy-ball WORLD)))))
         
          ((and (and (>= (+ (+ boxb-l WIDTH) RADIUS) (world-x-ball WORLD)) (>= (world-x-ball WORLD) (+ (+ boxb-l WIDTH) (/ RADIUS 2))))
              (and (>= (world-y-ball WORLD) (- (world-ybar1 WORLD) (+ (/ LENGTH 2) (/ RADIUS 3))))
                   (<= (world-y-ball WORLD) (+ (world-ybar1 WORLD) (+ (/ LENGTH 2) (/ RADIUS 3))))))
         (begin
           (display "debug: inside bar1 collision")
           (display "\n")
           (set-world-vx-ball! WORLD (* -1 (world-vx-ball WORLD)))
           (set-world-vy-ball! WORLD (+ (/ (- (world-y-ball WORLD) (world-ybar1 WORLD)) 25) (world-vy-ball WORLD))))))

  ;;updating the position of the ball
  (cond
  [(world-ball_present WORLD)
   (begin
     (set-world-x-ball! WORLD (+ (world-x-ball WORLD) (* delta-t (world-vx-ball WORLD))))
     (set-world-y-ball! WORLD (+ (world-y-ball WORLD) (* delta-t (world-vy-ball WORLD)))))]))

(define (update-bars keystates WORLD)
  (display "debug: inside update-bars")
  (display "\n")
  (cond
    [(eq? (hash-ref keystates 'up1) #t)
     (begin
       (display "debug: inside update-bars cond")
       (display "\n")
       (move 1 'up))])
  (cond
    [(eq? (hash-ref keystates 'dn1) #t)
     (move 1 'down)])
  (cond
    [(eq? (hash-ref keystates 'up2) #t)
     (move 2 'up)])
  (cond
    [(eq? (hash-ref keystates 'dn2) #t)
     (move 2 'down)]))

(define (move which-bar dir)
  (display "debug: inside move")
  (display "\n")
  
  ;;checking bar
  (cond
    [(> (+ boxb-up (/ LENGTH 2)) (world-ybar1 WORLD)) (set-world-ybar1! WORLD (+ boxb-up (/ LENGTH 2)))]
    [(< (- boxb-dn (/ LENGTH 2)) (world-ybar1 WORLD)) (set-world-ybar1! WORLD (- boxb-dn (/ LENGTH 2)))]
    [(> (+ boxb-up (/ LENGTH 2)) (world-ybar2 WORLD)) (set-world-ybar2! WORLD (+ boxb-up (/ LENGTH 2)))]
    [(< (- boxb-dn (/ LENGTH 2)) (world-ybar2 WORLD)) (set-world-ybar2! WORLD (- boxb-dn (/ LENGTH 2)))])
  
  ;;updating the position of the bar
  (if (eq? which-bar 1)
      (begin
        (display "debug: inside move for bar 1")
        (display "\n")
        (if (eq? dir 'up)
            (set-world-ybar1! WORLD (- (world-ybar1 WORLD) step))
            (set-world-ybar1! WORLD (+ (world-ybar1 WORLD) step))))
      (if (eq? dir 'up)
          (set-world-ybar2! WORLD (- (world-ybar2 WORLD) step))
          (set-world-ybar2! WORLD (+ (world-ybar2 WORLD) step)))))
          

(define (stop? WORLD)
  (eq? (world-quit WORLD) #t))

; World -> Image
; Draw the current world
(define (draw-world WORLD)                                 ; To make a visual representation of the world
  (define ball (circle RADIUS "solid" (world-color WORLD)))
  (define scene-paddle (place-image bar2 (+ boxb-l (/ WIDTH 2)) (world-ybar1 WORLD)
                                    (place-image bar1 (- boxb-r (/ WIDTH 2)) (world-ybar2 WORLD)
                                                 (place-image background-image 500 300
                                                              (empty-scene 1000 600)))))   ;Nesting of images, results in the awesome graphics of the game
  (if (world-ball_present WORLD)
      (place-image ball (world-x-ball WORLD) (world-y-ball WORLD) scene-paddle)  ;Ball is visible only if the world-ball_present is true
      scene-paddle))

; World -> Boolean
; Check if this is the last world

(define (press WORLD inp-key)                        ;Checking for the key press events
  (cond
    [(key=? inp-key "w") (key-press! 'up1)] 
    [(key=? inp-key "s") (key-press! 'dn1)]
    [(key=? inp-key "up") (key-press! 'up2)]
    [(key=? inp-key "down") (key-press! 'dn2)]
    [(key=? inp-key " ")   (cond [(not (world-ball_present WORLD))       ; The space bar should bring the ball back to existence only if the ball is absent
                                 (begin
                                   (set-world-ball_present! WORLD #t)
                                   (set-world-vx-ball! WORLD (+ 2 (random 3)))
                                   (set-world-vy-ball! WORLD (random 2)))])]
    [(key=? inp-key "g") (set-world-quit! WORLD #t)])
  WORLD)

(define (release WORLD inp-key)                           ;Checking for the key unpress events
  (cond
    [(key=? inp-key "w") (key-unpress! 'up1)] 
    [(key=? inp-key "s") (key-unpress! 'dn1)]
    [(key=? inp-key "up") (key-unpress! 'up2)]
    [(key=? inp-key "down") (key-unpress! 'dn2)])
WORLD)
                          
  
(main WORLD)