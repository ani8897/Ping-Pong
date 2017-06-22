;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;functions which update the various parameters of simulation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; function to update the color change according to speed of the ball;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-color)             
  (define speed-square (+ (* (world-vx-ball WORLD) (world-vx-ball WORLD)) (* (world-vy-ball WORLD) (world-vy-ball WORLD))))
  (define green-component (- 255 (* speed-square (/ 255 20))))
  (set-world-color! WORLD (color 255 (inexact->exact (max 0 (round green-component))) 0 255)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; function to update the status of the ball;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-ball)
  (begin
    (update-vx WORLD)
    (cond
      ;;checking the positions of the ball for the boundary conditions

      ;Reflecting the ball from collision with the upper wall
      [(and (> boxb-up (- (world-y-ball WORLD) RADIUS)) (world-ball-present WORLD))
       (begin
         (set-world-y-ball! WORLD (+ boxb-up RADIUS))
         (set-world-vy-ball! WORLD (* -1 (world-vy-ball WORLD)))
         (display "dbg inside-balls-lower")
         (display "\n"))]

      ;Reflecting the ball from collision with the lower wall
      [(and (< boxb-dn (+ (world-y-ball WORLD) RADIUS)) (world-ball-present WORLD))
       (begin
         (set-world-y-ball! WORLD (- boxb-dn RADIUS))
         (set-world-vy-ball! WORLD (* -1 (world-vy-ball WORLD)))
         (display "dbg inside-balls-lower")
         (display "\n"))]

      ;Reflecting the ball from collision with the left wall
      [(and (> boxb-l (- (world-x-ball WORLD) RADIUS)) (world-ball-present WORLD))
       (begin
         (display "Increase the score of right player")  ;If the ball goes out of bounds, it is made to disappear from the world
         (display "score: ")
         (display "\n")
         (set-world-scoreR! WORLD (+ (world-scoreR WORLD) 1))
         (set-world-last-winner! WORLD 'R)
         (set-world-ball-present! WORLD #f)
         (set-world-sim-count! WORLD 0)
         (display (world-scoreL WORLD))
         (display (world-scoreR WORLD))
         (set-world-y-ball! WORLD (/ (+ boxb-up boxb-dn) 2))
         (set-world-x-ball! WORLD (/ (+ boxb-l boxb-r) 2)))]

      ;Reflecting the ball from collision with the right wall
      [(and (< boxb-r (+ (world-x-ball WORLD) RADIUS)) (world-ball-present WORLD))
       (begin
         (display "Increase the score of left player")
         (display "score: ")
         (display "\n")
         (set-world-scoreL! WORLD (+ (world-scoreL WORLD) 1))
         (set-world-last-winner! WORLD 'L)
         (set-world-ball-present! WORLD #f)
         (set-world-sim-count! WORLD 0)
         (display (world-scoreL WORLD))
         (display (world-scoreR WORLD))
         (set-world-y-ball! WORLD (/ (+ boxb-up boxb-dn) 2))
         (set-world-x-ball! WORLD (/ (+ boxb-l boxb-r) 2)))])

    ;;checking if any player won the match
    (cond [(= (world-scoreL WORLD) 10) (set-world-won! WORLD 'L)]
          [(= (world-scoreR WORLD) 10) (set-world-won! WORLD 'R)])

    ;Changing the ball's velocity according to the collisions with the bars
    ;the coefficient of collision is not set to one in order to simulate the
    ;real world collision the collision coefficient varries linearly with the
    ;separation between the centre of the ball and bar.
    (cond
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
    
    ;;updating the position of the ball at delta time step later this will be shown at next instant
    (cond
      [(world-ball-present WORLD)
       (begin
         (set-world-x-ball! WORLD (+ (world-x-ball WORLD) (* delta-t (world-vx-ball WORLD))))
         (set-world-y-ball! WORLD (+ (world-y-ball WORLD) (* delta-t (world-vy-ball WORLD)))))])))

(define (update-vx WORLD)
  (cond [(and (world-ball-present WORLD) (< (abs (world-vx-ball WORLD)) 2))
         (set-world-vx-ball! WORLD (/ (* (world-vx-ball WORLD)
                                  (+ (abs (world-vx-ball WORLD)) 0.001))
                               (abs (world-vx-ball WORLD))))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; function to update the position of the bars depending upon the keystrokes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (update-bars keystates WORLD)
  (display "debug: inside update-bars")
  (display "\n")
  (cond
    [(eq? (hash-ref keystates 'up1) #t)
     (begin
       (display "debug: inside update-bars")
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; function to move the bars on pressing of the keys;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (move which-bar dir)
  (display "debug: inside move")
  (display "\n")
  
  ;;checking bar for boundary conditions
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;