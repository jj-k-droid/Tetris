(import image)
(import canvas)
(import html)
(import reactive)


;=====================================================================
; [ Grid Functions ]
(struct grid (width height contents))

(define create-grid
  (lambda (width height)
    (grid width height (make-vector (* width height) 0))))

(define grid-ref 
  (lambda (grid x y)
    (vector-ref (grid-contents grid) (+ x (* y (grid-width grid))))))

(define grid-set!
  (lambda (grid x y value)
    (vector-set! (grid-contents grid) (+ x (* y (grid-width grid))) value )))

(define grid-fill!
  (lambda (grid value)
    (vector-fill! (grid-contents grid) value)))

(define get-row-helper 
  (lambda (grid y i)
    (if (>= i (grid-width grid))
        null
        (cons (grid-ref grid i y) (get-row-helper grid y (+ i 1))))))

(define get-row
  (lambda (grid y)
    (get-row-helper grid y 0)))

;=========================================================================
; [ Grid/Block collision detection ]
(define block-collision-helper
  (lambda (game-grid block-grid block-x block-y x y)
    (cond [(>= y (grid-height block-grid)) #f]
          [(>= x (grid-width block-grid))
           (block-collision-helper game-grid block-grid block-x block-y 0 (+ y 1))] 
          [(not (grid-in-bounds? game-grid (+ block-x x) (+ block-y y))) #t]
          [(or (equal? (grid-ref game-grid (+ block-x x) (+ block-y y)) 0)
               (equal? (grid-ref block-grid x y) 0))
           (block-collision-helper game-grid block-grid block-x block-y (+ x 1) y)]
          [else #t])))

(define block-collision?
  (lambda (game-grid block-grid block-x block-y)
    (block-collision-helper game-grid block-grid block-x block-y 0 0)))
;=========================================================================
; [ Function to detect and clear filled rows ]
(define detect-full-row
  (lambda (grid y x)
    (cond
      [(>= x (grid-width grid)) #t]             
      [(equal? (grid-ref grid x y) 0) #f]          
      [else (detect-full-row grid y (+ x 1))]))) 


(define clear-row
  (lambda (grid y x)
    (cond
      [(>= x (grid-width grid)) #t]                 
      [else
       (begin
         (grid-set! grid x y 0)                     
         (clear-row grid y (+ x 1)))])))  


(define clear-full-rows
  (lambda (grid y)
    (cond
      [(< y 0) #t] 
      [(detect-full-row grid y 0) 
       (begin
         (score-increase 10)
         (clear-row grid y 0) 
         (clear-full-rows grid (- y 1)))] 
      [else
       (clear-full-rows grid (- y 1))])))
;=========================================================================
; [ Square Drawing Functions ]
(define colors (list
                (rgb 0 0 0)
                 (rgb 255 0 0)
                 (rgb 255 127 0)
                 (rgb 255 255 0)
                 (rgb 0 255 0)
                 (rgb 0 255 255)
                 (rgb 0 0 255)
                 (rgb 255 0 255)))
(define shadow 
  (lambda (color)
    (let* ([c (rgb->hsv color)]
           [h (hsv-hue c)]
           [s (hsv-saturation c)]
           [v (hsv-value c)]) 
       (hsv h s (* v 0.7)))))

(define highlight 
  (lambda (color)
    (let* ([c (rgb->hsv color)]
           [h (hsv-hue c)]
           [s (hsv-saturation c)]
           [v (hsv-value c)]) 
       (hsv h (* s 0.5) v))))

(define tetris-square 
  (lambda (size color)
    (let* ([shadow (shadow color)]
           [highlight (highlight color)])
    (overlay (solid-square (* size 0.75) color)
             (path size size (list (pair 0 0) (pair 0 size) (pair size 0)) "solid" highlight)
             (solid-square size shadow)))))

;========================================================================
; [ grid->drawing and helper functions ]
(define grid-value->tetris-square
  (lambda (value size)
    (if (equal? value 0)
        (solid-square size (rgb 0 0 0 0))
        (tetris-square size (list-ref colors value)))))

(define row->drawing
  (lambda (row size)
    (apply beside
      (map (section grid-value->tetris-square _ size) row ))))

(define grid->drawing-helper
  (lambda (grid size i so-far)
    (if (< i 0)
        (apply above so-far)
        (grid->drawing-helper grid size (- i 1) (cons (row->drawing (get-row grid i) size) so-far)))))

(define grid->drawing 
  (lambda (grid pixel-size)
    (grid->drawing-helper grid pixel-size (- (grid-height grid) 1) null)))

(define canvas-grid! 
  (lambda (canvas x y grid pixel-size)
    (canvas-drawing! canvas x y (grid->drawing grid pixel-size))))
;========================================================================
; [ Game Items ]

;Score cell and functions for manipulating it
(define score-cell (ref 0))
(define score (lambda () (deref score-cell)))
(define score-set (lambda (value) (ref-set! score-cell value)))
(define score-increase (lambda (value) (score-set (+ (score) value))))





;Game Grid
(define tetris-grid (create-grid 10 20))

;Title screen grid (18 x 5)
(define title-grid (grid 18 5 (vector  1 1 1 2 2 2 3 3 3 4 4 0 5 5 5 0 6 6
                                       0 1 0 2 0 0 0 3 0 4 0 4 0 5 0 6 0 0
                                       0 1 0 2 2 0 0 3 0 4 4 0 0 5 0 0 6 0
                                       0 1 0 2 0 0 0 3 0 4 0 4 0 5 0 0 0 6
                                       0 1 0 2 2 2 0 3 0 4 0 4 5 5 5 6 6 0)))

(define game-over-image (overlay (text "GAME OVER" 20 "red")
                                 (solid-rectangle 150 40 (rgb 0 0 0 0))))

(define play-again-image (overlay (text "Play Again?" 20 (rgb 191 0 255))
                                 (solid-rectangle 150 40 (rgb 39 39 39)))) 
(define play-button-image 
  (lambda (hue)
    (overlay (text "PLAY" 20 (hsv->rgb (hsv hue 100 100)))
             (solid-rectangle 150 40 (rgb 39 39 39)))))
;========================================================================
;; [ Update/ Draw Functions ]


(define reset-game
  (lambda ()
    (begin
      (grid-fill! tetris-grid 0)
      (score-set 0))))


(define update-game
  (lambda ()
    (begin
      (grid-set! tetris-grid (random (grid-width tetris-grid)) 
                             (random (grid-height tetris-grid)) 
                             (random 8))
      (clear-full-rows tetris-grid (grid-height tetris-grid))
      )))

(define draw-game
  (lambda ()
    (begin 
      (canvas-rectangle! canv 0 0 100 400 "solid" (rgb 0 0 128))
      (canvas-rectangle! canv 300 0 100 400 "solid" (rgb 0 0 128))
      (canvas-text! canv 5 20 (string-append "Score:\n" (number->string (score))) 20 "solid" (rgb 128 128 128))
      (canvas-grid! canv 100 0 tetris-grid 20) 
)))

(define update-game-over-screen
  (lambda ()
    ()))

(define draw-game-over-screen
  (lambda ()
    (begin (draw-game)
           (canvas-rectangle! canv 0 0 400 400 "solid" (rgb 0 0 0 (round (min 150 (/ (game-time) 3)))))
           (canvas-drawing! canv 125 (round (min 150 (/ (game-time) 3))) game-over-image)
           (canvas-drawing! canv 125 (- 400 (round (min 200 (/ (game-time) 2.25)))) play-again-image)
           )))

(define update-menu-screen
  (lambda ()
      ()))

(define draw-menu-screen
  (lambda ()
    (begin (canvas-grid! canv 20 50 title-grid 20)
           (canvas-text! canv 200 170 "made in scamper™" 20 "solid" (rgb 94 17 158))
           (canvas-drawing! canv 125 290 (play-button-image (remainder (round (/ (game-time) 10)) 360))))))

(define recieve-mouse-hover
  (lambda (x y)
    ()))

(define recieve-mouse-click
  (lambda (x y)
    ()))

(define recieve-key-press
  (lambda (key)
    ()))
;========================================================================
;; [ Main Game Loop Control ]

;clock cells allow current time to be accessed from anywhere
(define clock (ref 0))
(define state-change (ref 0))

; (time) returns the time since the window loaded, 
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
(define state-set! (lambda (state) (begin (ref-set! game-state state) 
                                          (ref-set! state-change (time))
                                          (if (running?) (reset-game) void))))
             
;========================================================================


;this is the main update function that updates the clock and draws the correct screen depending on what state the game is in
(define update-game-loop
  (lambda (t)
    (begin
      (ref-set! clock t)
      (cond
        [(menu?)
         (update-menu-screen)]
        [(running?)
         (update-game)]
        [(game-over?)
         (update-game-over-screen)]
        [else
          (state-set! state-menu)])
      #t)))

(define draw-screen
  (lambda (state canvas)
    (begin
      (canvas-rectangle! canv 0 0 400 400 "solid" "black")
      (cond
        [(menu?)
         (draw-menu-screen)]
        [(running?)
         (draw-game)]
        [(game-over?)
         (draw-game-over-screen)]
        [else
          (state-set! state-menu)])
      #t)))



(define handle-event
  (lambda (event state)
    (match event
      [(event-timer t _) (update-game-loop t)]
      [(event-mouse-hover x y) (recieve-mouse-hover x y)]
      [(event-mouse-click b x y) (recieve-mouse-click x y)]
      [(event-key-down key) (recieve-key-press key)])))

(define canv (reactive-canvas 400 400 #t draw-screen handle-event 
                              (on-timer (/ 1000 60)) 
                              (on-key-down) 
                              (on-mouse-click) 
                              (on-mouse-hover)))

;Temprary buttons for changing game states
(button "Menu" (lambda () (state-set! state-menu)))
(button "Play" (lambda () (state-set! state-running)))
(button "Game Over" (lambda () (state-set! state-game-over)))
canv
