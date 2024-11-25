(import image)
(import canvas)

(define canv (make-canvas 400 400))
canv

;clock cells allow current time to be accessed from anywhere
(define clock (ref 0))
(define state-change (ref 0))

; (time) returns the time since the browser loaded, 
; (game-time) returns the time since the state most recently changed
(define time (lambda () (deref clock)))
(define game-time (lambda () (- (deref clock) (deref state-change))))

; the game is either in the menu (0), running (1), or game over (2).
; store the value in a refrence cell
(define game-state (ref 0))

; possible game states
(define state-menu 1)
(define state-running 2)
(define state-game-over 3)

; (menu?) returns #t if the current state is the menu screen
(define menu? (lambda () (equal? (deref game-state) state-menu)))
; (running?) returns #t if the game is currently in the `running` state
(define running? (lambda () (equal? (deref game-state) state-running)))
; (game-over?) returns #t if the current state is the game over screen
(define game-over? (lambda () (equal? (deref game-state) state-game-over)))

;sets the game state to a specific value and resets the game clock
(define state-set! (lambda (state) (begin (ref-set! game-state state) (ref-set! state-change (time)))))

;Functions to update and draw the different states of the game:
(define update-game
  (lambda ()
    ()))

(define draw-game
  (lambda ()
    (begin 
      (canvas-text! canv 10 20 "state: RUNNING" 20 "solid" "green")
)))

(define update-game-over-screen
  (lambda ()
    ()))

(define draw-game-over-screen
  (lambda ()
    (begin (canvas-text! canv 10 20 "state: GAME OVER" 20 "solid" "red"))))

(define update-menu-screen
  (lambda ()
      ()))

(define draw-menu-screen
  (lambda ()
    (begin (canvas-text! canv 10 20 "state: MENU" 20 "solid" "blue")
           (canvas-text! canv 100 100 (number->string (round (time))) 50 "outline" "white")
           (canvas-text! canv 100 200 (number->string (round (game-time))) 50 "outline" "white"))))

;this is the main update function that updates the clock and draws the correct screen depending on what state the game is in
(define run-game-loop
  (lambda (t)
    (begin
      (ref-set! clock t)
      (canvas-rectangle! canv 0 0 400 400 "solid" "black")
      (cond
        [(menu?)
         (begin (update-menu-screen) (draw-menu-screen))]
        [(running?)
         (begin (update-game) (draw-game))]
        [(game-over?)
         (begin (update-game-over-screen) (draw-game-over-screen))]
        [else
          (state-set! state-menu)])
      (canvas-drawing! canv 0 300
        (beside (solid-rectangle 125 100 "blue")
                (solid-rectangle 150 100 "green")
                (solid-rectangle 150 100 "red")))
      #t)))

;Temporary buttons to cycle between states
(canvas-onclick! canv 
  (lambda (x y)
    (cond
      [(< y 300) void]
      [(< x 125) (state-set! state-menu)]
      [(> x 275) (state-set! state-game-over)]
        [else    (state-set! state-running)])))

(animate-with run-game-loop)