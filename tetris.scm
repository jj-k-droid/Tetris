; tetris.scm
(import image)
(import canvas)
(import html)
(import reactive)

;=========================================================================
;                        [ Grid Functions ]
;=========================================================================
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
(define grid-in-bounds?
  (lambda (grid x y)
    (and (>= x 0) (>= y 0) (< x (grid-width grid)) (< y (grid-height grid)))))
(define get-row-helper 
  (lambda (grid y i)
    (if (>= i (grid-width grid))
        null
        (cons (grid-ref grid i y) (get-row-helper grid y (+ i 1))))))
(define get-row
  (lambda (grid y)
    (get-row-helper grid y 0)))
(define grid-rotate-helper
  (lambda (grid x y new-grid)
    (cond 
      [(>= y (grid-height grid)) void]
      [(>= x (grid-width grid)) (grid-rotate-helper grid 0 (+ y 1) new-grid)]
      [else 
        (begin 
            (grid-set! new-grid y (- (grid-height grid) x 1) (grid-ref grid x y))
            (grid-rotate-helper grid (+ x 1) y new-grid))])))
(define grid-rotate
  (lambda (grid)
    (let* ([new-grid (create-grid (grid-width grid) (grid-height grid))])
      (begin
        (grid-rotate-helper grid 0 0 new-grid)
        new-grid))))


;=========================================================================
;                   [ Square Drawing Functions ]
;=========================================================================
(define colors (list
                (rgb 0 0 0)
                 (rgb 254 17 60)
                 (rgb 255 115 9)
                 (rgb 255 222 2)
                 (rgb 102 253 3)
                 (rgb 1 230 254)
                 (rgb 24 1 255)
                 (rgb 184 3 253)))
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
;=========================================================================
;                  [ Grid/Block collision detection ]
;=========================================================================
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


(define mouse-rectangle-collision?
  (lambda (mx my rx ry rw rh)
    (and (> mx rx) (> my ry) (< mx (+ rx rw)) (< my (+ ry rh)))))
;=========================================================================
;             [ Function to detect and clear filled rows ]
;=========================================================================
(define detect-full-row
  (lambda (grid y x)
    (cond
      [(>= x (grid-width grid)) #t]             
      [(equal? (grid-ref grid x y) 0) #f]          
      [else (detect-full-row grid y (+ x 1))]))) 


(define clear-row-helper
  (lambda (grid i y)
    (cond
      [(< i 0) void]                 
      [else
       (begin
         (if (>= i (grid-width grid))
             (vector-set! (grid-contents grid) i 
                          (vector-ref (grid-contents grid) (- i (grid-width grid)))) 
             (vector-set! (grid-contents grid) i 0))
         (clear-row-helper grid (- i 1) y))])))  

(define clear-row
  (lambda (grid y)
    (clear-row-helper grid (* y (grid-width grid)) y)))

(define clear-full-rows
  (lambda (grid y)
    (cond
      [(>= y (grid-height grid)) #t] 
      [(detect-full-row grid y 0) 
       (begin
         (score-increase 10)
         (clear-row grid y) 
         (clear-full-rows grid (+ y 1)))] 
      [else
       (clear-full-rows grid (+ y 1))])))
;=========================================================================
;             [ functions to display a grid on a canvas ]
;=========================================================================
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
;=========================================================================
;                           [ Game Items ]
;=========================================================================
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

;============================================================================
;                               [ shapes ]
;============================================================================
(struct tetromino
  (x y rotations))

(define make-rotations!
  (lambda (grid)
    (vector grid 
            (grid-rotate grid) 
            ((o grid-rotate grid-rotate) grid)
            ((o grid-rotate grid-rotate grid-rotate) grid))))

(define shape->drawing
  (lambda (shape index)
    (grid->drawing (vector-ref (tetromino-rotations shape) index) 20)))

(define I
  (let* ([shape-grid (create-grid 4 4)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 0 1 5)
                         (grid-set! shape-grid 1 1 5)
                         (grid-set! shape-grid 2 1 5)
                         (grid-set! shape-grid 3 1 5)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(vector-map (section grid->drawing _ 20) (tetromino-rotations I))

(define J
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 0 0 6)
                         (grid-set! shape-grid 0 1 6)
                         (grid-set! shape-grid 1 1 6)
                         (grid-set! shape-grid 2 1 6)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(vector-map (section grid->drawing _ 20) (tetromino-rotations J))

(define L
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 3 0 2)
                         (grid-set! shape-grid 3 1 2)
                         (grid-set! shape-grid 2 1 2)
                         (grid-set! shape-grid 1 1 2)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(vector-map (section grid->drawing _ 20) (tetromino-rotations L))

(define O
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 1 1 3)
                         (grid-set! shape-grid 2 1 3)
                         (grid-set! shape-grid 1 2 3)
                         (grid-set! shape-grid 2 2 3)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(vector-map (section grid->drawing _ 20) (tetromino-rotations O))

(define S
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 1 0 4)
                         (grid-set! shape-grid 2 0 4)
                         (grid-set! shape-grid 0 1 4)
                         (grid-set! shape-grid 1 1 4)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(vector-map (section grid->drawing _ 20) (tetromino-rotations S))

(define Z
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 0 0 1)
                         (grid-set! shape-grid 1 0 1)
                         (grid-set! shape-grid 1 1 1)
                         (grid-set! shape-grid 2 1 1)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(vector-map (section grid->drawing _ 20) (tetromino-rotations Z))

(define T
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 1 0 7)
                         (grid-set! shape-grid 0 1 7)
                         (grid-set! shape-grid 1 1 7)
                         (grid-set! shape-grid 2 1 7)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(vector-map (section grid->drawing _ 20) (tetromino-rotations T))


;=========================================================================
;                         [ spawing shapes ]
;=========================================================================
(define spawnable-shapes
  (vector (pair I (list 0 2))
          (pair J (list 0 2))
          (pair L (list 1 2))
          (pair O (list 0 1 2 3))
          (pair S (list 0 2))
          (pair Z (list 0 2))
          (pair T (list 1))))

(define rotation
  (ref "none"))

(define shape-index
  (ref 0))

(define current-shape
  (ref void))

(define x-position
  (ref 200))

(define y-position
  (ref 0))

(define shape-spawn-time
  (ref 0))

(define queue
  (ref (vector-range 10)))

(define queue-index
  (ref 0))

(define spawn-shape!
  (lambda ()
    (ignore
      (let* ([shape (vector-ref spawnable-shapes 
                                (random (vector-length spawnable-shapes)))]
             [indeces (cdr shape)])
            (begin
              (ref-set! current-shape (car shape))
              (ref-set! shape-index (list-ref indeces (random (length indeces))))
              (canvas-drawing! canv (deref x-position) 
                                    (deref y-position) 
                                    (shape->drawing (deref current-shape) 
                                                    (deref shape-index))))))))
;=========================================================================
;;                     [ Update/ Draw Functions ]
;=========================================================================

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
      (clear-full-rows tetris-grid 0)
      (spawn-shape!))))

(define draw-game
  (lambda ()
    (begin 
      (canvas-rectangle! canv 0 0 100 400 "solid" (rgb 0 0 128))
      (canvas-rectangle! canv 300 0 100 400 "solid" (rgb 0 0 128))
      (canvas-text! canv 5 20 (string-append "Score:\n" (number->string (score))) 20 "solid" (rgb 128 128 128))
      (canvas-grid! canv 100 0 tetris-grid 20) 
      (canvas-drawing! canv (deref x-position) (deref y-position) current-shape)
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
    (cond
      [(menu?) 
       (if (mouse-rectangle-collision? x y 125 290 150 40) (state-set! state-running) void)]
      [(running?) 
       void]
      [(game-over?)
       (if (mouse-rectangle-collision? x y 125 200 150 40) (state-set! state-running) void)])))

(define recieve-key-press
  (lambda (key)
    (cond
        [(and (or (equal? key "x") (equal? key "z")) (equal? (deref rotation) "none"))
            (begin 
              (ref-set! rotation "rotating")
              (rotate-shape key canv))]
        [(or (equal? key "ArrowRight") (equal? key "ArrowLeft") (equal? key "ArrowDown"))
          (move key canv)]
        [else void])))
;=========================================================================
;;                    [ Main Game Loop Control ]
;=========================================================================
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
             
;=========================================================================
;                     [ Reactive Canvas Functions ]
;=========================================================================
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


(define rotate-shape
  (lambda (key canvas)
    (let* ([i (+ (deref shape-index) 4)])
         (begin
            (if (equal? key "x")
                (begin
                  (ref-set! shape-index (remainder (+ i 1) 4)))
                (begin
                  (ref-set! shape-index (remainder (- i 1) 4))))
            (ref-set! rotation "none")))))            

(define move
  (lambda (key canvas)
    (let
      ([x-pos (deref x-position)]
       [y-pos (deref y-position)])
      (begin
          (cond 
            [(equal? key "ArrowRight")
              (begin
                  (ref-set! x-position (+ x-pos 20)))]
            [(equal? key "ArrowLeft")
              (begin
                (ref-set! x-position (- x-pos 20)))]
            [else 
              (begin
                (ref-set! y-position (+ y-pos 20)))])))))                          

(ignore
  (animate-with
    (lambda (time)
           (begin
             (ref-set! y-position (remainder (* (quotient (round (- time (deref shape-spawn-time))) 750) 20) 400))
             ; (canvas-drawing! canv (deref x-position) (deref y-position) (shape->drawing (deref current-shape) (deref shape-index)))
             (spawn-shape!)
             #t))))

canv