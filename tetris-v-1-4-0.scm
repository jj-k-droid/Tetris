;;; tetris.scm
;;; CSC-151-02 F24 Final Project
;;; v.1.4.0
;;; Submitted on 12/11/24 
;;; Made by The Tetromino Titans:
;;;  - Steph Kama-Kama
;;;  - Rayane Nossair
;;;  - Dimitra Giantsidi
;;;  - Leo Eakin
;;;  - Aarav Sharma
;;;
;;; Acknowledgements:
;;; https://scamper.cs.grinnell.edu/2.8.7/docs.html
;;; https://osera.cs.grinnell.edu/csc151/about.html
;;; https://tetris.wiki/Tetris_Guideline
;;; https://tetris.wiki/Tetris.wiki
;;;
;;; Note: There is an error called when the game is first run about `zones`.
;;;       When it comes up, just hit run again and play the game :)

(import image)
(import canvas)
(import html)
(import reactive)
(import music)

;========================================================================
;                         [ Music & Sound Effects ]
;========================================================================

;;; Imports of instruments used:
;;; Square Lead:
(ignore
  (load-instrument 81))

;;; Saw Wave Lead:
(ignore
  (load-instrument 82))

;;; Bass Drum for the block-ground effect:
(ignore
  (load-percussion 35))

;;; Gunshot:
(ignore
  (load-instrument 128))

;;; Tubular Bells:
(ignore
  (load-instrument 14))

;;; Acoustic Grand Piano:
(ignore
  (load-instrument 0))

;;; Enablement of high quality instruments:
(ignore
  (use-high-quality-instruments #t))

;;; Definitions of the durations used.
;;; The duration of 1/8.
(define en-note en)

;;; The duration of 1/4.
(define qn-note qn)

;;; The duration of 1/2.
(define hn-note hn)

;;; The duration of 1.
(define wn-note wn)

;;; The sound effect for freezing a Tetromino.
(define block-ground-effect
   (seq (mod percussion (note 35 en-note))   
        (rest en-note)))                 

;;; The sound effect for clearing a line.
 (define line-clear-effect
   (mod (instrument 82)                      
        (par (seq (note 76 qn-note)         
                  (rest en-note))
             (seq (note 79 qn-note)          
                  (rest en-note))
             (seq (note 84 qn-note)       
                  (rest en-note)))))

;;; The sound effect for when a Tetromino moves.
(define piece-move
  (mod (instrument 81)
       (seq (note 60 sn)   
            (note 62 sn)))) 

;;; The sound effect for when a Tetromino rotates.
(define piece-rotate
  (mod (instrument 81)
       (seq (note 67 tn)   
            (note 65 tn))))

;;; The sound effect for when the game ends.
(define game-over
  (mod (instrument 81)
       (seq (note 60 qn)   
            (note 57 qn)   
            (note 55 hn))))

;;; The sound effect for when the game levels up.
(define level-up
  (seq (note 79 en)   
       (note 81 en)   
       (note 84 qn)   
       (note 86 qn)))

;;; The music theme of the game.
(define tetris-theme
  (seq (note 76 qn)   
       (note 71 en)   
       (note 72 en)   
       (note 74 qn)   
       (note 72 en)   
       (note 71 en)   
       (note 69 qn)   
       (note 69 en)   
       (note 72 en)   
       (note 76 qn)   
       (note 74 en)   
       (note 72 en)   
       (note 71 qn)   
       (rest qn)
       (note 72 en)  
       (note 74 en)  
       (note 76 qn)   
       (note 72 qn)   
       (note 69 qn)   
       (note 69 qn)))

;;; Set of the tempo of the music theme.
(define tetris-performance
  (mod (tempo qn 120)  
       (mod (dynamics 100)
            tetris-theme)))

;;; Loop of the music theme.
(define tetris-real (repeat 100 tetris-performance))

;;; Call of the the music theme.
(ignore
  (play-composition (mod (dynamics 20) tetris-real)))
        
;=========================================================================
;                               [ Tetris Grid ]
;=========================================================================

;;; (grid width height contents) -> struct-grid?
;;;   width : nonnegative-integer?
;;;   height : nonnegative-integer?
;;;   contents : vector?
;;; Creates a struct-grid consisting of width, height and contents.
(struct grid
  (width height contents))

;;; (create-grid width height) -> struct-grid?
;;;   width : nonnegative-integer?
;;;   height : nonnegative-integer?
;;; Creates a struct-grid with the contents being a vector of (* width height) occurences of 0.
(define create-grid
  (lambda (width height)
    (grid width height (make-vector (* width height) 0))))

;;; (grid-ref grid x y) -> any?
;;;   grid : struct-grid?
;;;   x : nonnegative-integer?
;;;   y : nonnegative-integer?
;;; Returns the element found in the coordinates (x,y) within the contents of the given grid.
(define grid-ref 
  (lambda (grid x y)
    (vector-ref (grid-contents grid)
                (+ x (* y (grid-width grid))))))

;;; (grid-set! grid x y value) -> void?
;;;   grid : struct-grid?
;;;   x : nonnegative-integer?
;;;   y : nonnegative-integer?
;;;   value : any?
;;; Sets the element found in the coordinates (x,y) within the contents of the given grid to value.
(define grid-set!
  (lambda (grid x y value)
    (vector-set! (grid-contents grid)
                 (+ x (* y (grid-width grid)))
                 value)))

;;; (grid-fill! grid value) -> void?
;;;   grid : struct-grid?
;;;   value : any?
;;; Sets all the elements within the contents of the given grid to value.
(define grid-fill!
  (lambda (grid value)
    (vector-fill! (grid-contents grid)
                  value)))

;;; (grid-in-bounds grid x y) -> boolean?
;;;   grid : struct-grid?
;;;   x : number?
;;;   y : number?
;;; Determines if the given grid can be placed within the area of (* x y).
(define grid-in-bounds?
  (lambda (grid x y)
    (and (>= x 0)
         (>= y 0)
         (< x (grid-width grid)) 
         (< y (grid-height grid)))))

;;; (get-row/helper grid y i) -> list?
;;;   grid : struct-grid?
;;;   y : nonnegative-integer?
;;;   i :zzzz nonnegative-integer?
;;; Returns the elements of the yth row of the given grid starting from the x-coordinate i.
(define get-row/helper 
  (lambda (grid y i)
    (if (>= i (grid-width grid))
        null
        (cons (grid-ref grid i y)
              (get-row/helper grid y (+ i 1))))))

;;; (get-row grid y) -> list?
;;;   gzzrid : struct-grid?
;;;   y : nonnegative-integer?
;;; Returns the elements of the yth row of the given grid.
(define get-row
  (lambda (grid y)
    (get-row/helper grid y 0)))

;;; (grid-rotate/helper grid x y new-grid) -> void? 
;;;   grid : struct-grid?
;;;   x : nonnegative-integer?
;;;   y : nonnegative-integer?
;;z; z  new-grid : struct-grid?
;;; Sets the contents of new-grid to the same as that of the given grid but rotated to the left.
(define grid-rotate/helper
  (lambda (grid x y new-grid)
    (cond
      [(>= y (grid-height grid)) void]
      [(>= x (grid-width grid)) (grid-rotate/helper grid 0 (+ y 1) new-grid)]
      [else 
       (begin 
         (grid-set! new-grid
                    y
                    (- (grid-height grid) x 1)
                    (grid-ref grid x y))
         (grid-rotate/helper grid (+ x 1) y new-grid))])))

;;; (grid-rotate grid) -> stuct-grid?
;;;   grid : struct-grid?
;;; Returns a new grid of which the contents's elements are rotated to the left.
(define grid-rotate
  (lambda (grid)
    (let* ([new-grid (create-grid (grid-width grid) (grid-height grid))])
          (begin
            (grid-rotate/helper grid 0 0 new-grid)
            new-grid))))

;=========================================================================
;                           [ Tetris Squares ]
;=========================================================================

;;; colors -> list?
;;; Returns a list of 8 elements: the rgb value for black and all 7 rgb values of Tetris squares.
(define colors
  (list (rgb 0 0 0)
        (rgb 254 17 60)
        (rgb 255 115 9)
        (rgb 255 222 2)
        (rgb 102 253 3)
        (rgb 1 230 254)
        (rgb 24 1 255)
        (rgb 184 3 253)))

;;; (shadow color) -> hsv?
;;;   color : rgb?zz
;;; Returns the hsv equivalent of the color given but with its value decreased, making it appear
;;; as a shadow version of the given color.
(define shadow 
  (lambda (color)
    (let* ([c (rgb->hsv color)]
           [h (hsv-hue c)]
           [s (hsv-saturation c)]
           [v (hsv-value c)]) 
          (hsv h s (* v 0.7)))))

;;; (highlight color) -> hsv?
;;;   color : rgb?
;;; Returnsz the hsv equivalent of the color given but with its saturation decreased, making it appear
;;; as a highlighted version of the given color. 
(define highlight 
  (lambda (color)
    (let* ([c (rgb->hsv color)]
           [h (hsv-hue c)]
           [s (hsv-saturation c)]
           [v (hsv-value c)]) 
          (hsv h (* s 0.5) v))))

;;; (tetris-square size color) -> image?
;;;   size : nonnegative-number?
;;;   color : rgb?
;;; Returns a Tetris square of the given size and color.
(define tetris-square 
  (lambda (size color)
    (let* ([shadow (shadow color)]
           [highlight (highlight color)])
          (overlay (solid-square (* size 0.75) color)
                   (path size size (list (pair 0 0)
                                         (pair 0 size)
                                         (pair size 0))
                         "solid" highlight)
                   (solid-square size shadow)))))

;=========================================================================
;                  [ Grid/Block Collision Detection ]
;=========================================================================

;;; (shape-collision?/helper game-grid shape-grid shape-x shape-y x y) -> boolean?
;;;   game-grid : struct-grid?
;;;   shape-grid : struct-grid?
;;;   shape-x : nonnegative-integer?
;;;   shape-y : nonnegative-integer?
;;;   x : nonnegative-integer?
;;;   y : nonnegative-integer?
;;; Returns #f the height of shape-grid is lower than y or if the coordinates
;;; of the contents of game-grid are within the area of the coordinates of the shape (shape-grid) added to x and y respectively
;;; and if there is no Tetris square in the coordinates (x,y) of shape-grid at the same time.
;;; Returns #t otherwise.
(define shape-collision?/helper
  (lambda (game-grid shape-grid shape-x shape-y x y)
    (cond [(>= y (grid-height shape-grid)) #f]
          [(>= x (grid-width shape-grid))
           (shape-collision?/helper game-grid shape-grid shape-x shape-y 0 (+ y 1))] 
          [(not (or (grid-in-bounds? game-grid (+ shape-x x) (+ shape-y y))
                    (equal? (grid-ref shape-grid x y) 0)))
           #t]
          [(or (equal? (grid-ref game-grid (+ shape-x x) (+ shape-y y)) 0)
               (equal? (grid-ref shape-grid x y) 0))
           (shape-collision?/helper game-grid shape-grid shape-x shape-y (+ x 1) y)]
          [else #t])))

;;; (shape-collision? game-grid shape-grid shape-x shape-y) -> boolean?
;;;   game-grid : struct-grid?
;;;   shape-grid : struct-grid?
;;;   shape-x : nonnegative-integer?
;;;   shape-y : nonnegative-integer?
;;; Returns #t if the next movement or rotation of the shape (shape-grid) towards the right, the left,
;;; or downwards, according to its coordinates (shape-x, shape-y), would place it outside the boundaries
;;; of the game-grid or on top of another shape. Returns #f otherwise.
(define shape-collision?
  (lambda (game-grid shape-grid shape-x shape-y)
    (shape-collision?/helper game-grid shape-grid shape-x shape-y 0 0)))

;;; (mouse-rectangle-collision? mx my rx ry rw rh) -> boolean?
;;;   mx : nonnegative-number?
;;;   my : nonnegative-number?
;;;   rx : nonnegative-number?
;;;   ry : nonnegative-number?
;;;   rw : nonnegative-number?
;;;   rh : nonnegative-number?
;;; Returns #t if the values of mx and my are found within the area of a rectangle (* rw rh) as well as
;;; within its coordinates with a starting point of (rx,ry).
(define mouse-rectangle-collision?
  (lambda (mx my rx ry rw rh)
    (and (> mx rx)
         (> my ry)
         (< mx (+ rx rw))
         (< my (+ ry rh)))))

;=========================================================================
;                  [ Freezing Blocks into the Grid ]
;=========================================================================

;;; (block-freeze/helper game-grid block-grid block-x block-y x y) -> boolean?
;;;   game-grid : grid?
;;;   block-grid : grid?
;;;   block-x : nonnegative-integer?
;;;   block-y : nonnegative-integer?
;;;   x : integer?
;;;   y : integer?
;;; Copies all of the solid blocks in the block-grid into the game-grid at the position (block-x, block-y).
(define block-freeze/helper
  (lambda (game-grid block-grid block-x block-y x y)
    (cond [(>= y (grid-height block-grid)) #f]
          [(>= x (grid-width block-grid))
           (block-freeze/helper game-grid block-grid block-x block-y 0 (+ y 1))]   
          [else
              (let* ([cell-x (+ block-x x)]
                     [cell-y (+ block-y y)]
                     [grid-value (grid-ref block-grid x y)])
                    (if (= cell-y 0)
                      #t
                      (begin
                        (if (and (> grid-value 0)
                                 (grid-in-bounds? game-grid cell-x cell-y))
                            (grid-set! game-grid cell-x cell-y grid-value)
                            void)
                        (block-freeze/helper game-grid block-grid block-x block-y (+ x 1) y))))])))

;;; (block-freeze game-grid block-grid block-x block-y x y) -> boolean?
;;;   game-grid : grid?
;;;   block-grid : grid?
;;;   block-x : nonnegative-integer?
;;;   block-y : nonnegative-integer?
;;; Copies all of the solid blocks in the block-grid into the game-grid at the position (block-x, block-y).
(define block-freeze!
  (lambda (game-grid block-grid block-x block-y)
    (block-freeze/helper game-grid block-grid block-x block-y 0 0)))

;=========================================================================
;                 [ Detecting and Clearing Filled Rows ]
;=========================================================================

;;; (detect-full-row grid x y) -> boolean?
;;;   grid : struct-grid?
;;;   y : nonnegative-integer?
;;;   x : nonnegative-integer?
;;; Returns #t if there are Tetris squares present in the yth row of the contents of the given grid up to
;;; the xth element of that line.
(define detect-full-row
  (lambda (grid x y)
    (cond
      [(>= y (grid-width grid))
       #t]             
      [(equal? (grid-ref grid y x) 0)
       #f]          
      [else
       (detect-full-row grid x (+ y 1))]))) 

;;; (clear-row!/helper grid i y) -> void?
;;;   grid : struct-grid?
;;;   i : nonnegative-integer?
;;;   y : nonnegative-integer?
;;; Sets the elements of the yth row in the contents of the given grid to the elements of the line above them
;;; from the first element of the yth row up to the ith element, and fills the 0th (top) row with empty squares.
(define clear-row!/helper
  (lambda (grid i y)
    (cond
      [(< i 0) void]                 
      [else
       (begin
         (if (>= i (grid-width grid))
             (vector-set! (grid-contents grid) i 
                          (vector-ref (grid-contents grid) (- i (grid-width grid)))) 
             (vector-set! (grid-contents grid) i 0))
         (clear-row!/helper grid (- i 1) y))])))  

;;; (clear-row! grid y) -> void?
;;;   grid : struct-grid?
;;;   y : nonnegative-integer?
;;; Clears the yth row in the contents of the given grid and moves the contents of the row before
;;; to the yth row.
(define clear-row!
  (lambda (grid y)
    (clear-row!/helper grid (- (* (+ y 1) (grid-width grid)) 1) y)))

;;; (clear-full-rows! grid y number-cleared) -> void!
;;;   grid : grid?
;;;   y : nonnegative-integer?
;;;   number-cleared : nonnegative-integer?
;;; Clears the appropriate rows.
(define clear-full-rows!
  (lambda (grid y number-cleared)
    (cond
      [(>= y (grid-height grid))
       number-cleared]
      [(detect-full-row grid y 0)
       (begin
         (clear-row! grid y)
         (clear-full-rows! grid (+ y 1) (+ number-cleared 1)))]
      [else
       (clear-full-rows! grid (+ y 1) number-cleared)])))

;=========================================================================
;             [ Displaing a Grid on a Canvas ]
;=========================================================================

;;; (grid-value->tetris-square value size) -> tetris-square?
;;;   value : integer?
;;;   size : non-negative-integer?
;;; Converts a grid value into a corresponding Tetris square. If the value is `0`, creates a
;;; transparent square; otherwise, creates a colored square based on the value.
(define grid-value->tetris-square
  (lambda (value size)
    (if (equal? value 0)
        (solid-square size (rgb 0 0 0 0))
        (tetris-square size (list-ref colors value)))))

;;; (row->drawing row size) -> drawing?
;;;   row : list-of-integers?
;;;   size : non-negative-integer?
;;; Converts a row of grid values into a drawing by mapping each value to a corresponding 
;;  Tetris square and arranging them horizontally.
(define row->drawing
  (lambda (row size)
    (apply beside
      (map (section grid-value->tetris-square _ size) row ))))

;;; (grid->drawing-helper grid size i so-far) -> drawing?
;;;   grid : struct-grid?
;;;   size : non-negative-integer?
;;;   i : integer? 
;;;   so-far : list?
;;; Recursively constructs a drawing of the grid by converting each row into a drawing and
;;; stacking the rows vertically.
(define grid->drawing-helper
  (lambda (grid size i so-far)
    (if (< i 0)
        (apply above so-far)
        (grid->drawing-helper grid size (- i 1) (cons (row->drawing (get-row grid i) size) so-far)))))

;;; (grid->drawing grid pixel-size) -> drawing?
;;;   grid : struct-grid?
;;;   pixel-size : non-negative-integer?
;;; Converts the entire grid into a drawing by stacking its rows vertically, where each square
;;; in the grid is represented as a Tetris square of the specified pixel size.
(define grid->drawing 
  (lambda (grid pixel-size)
    (grid->drawing-helper grid pixel-size (- (grid-height grid) 1) null)))

;;; (canvas-grid! canvas x y grid pixel-size) -> void?
;;;   canvas : canvas?
;;;   x : integer? 
;;;   y : integer? 
;;;   grid : struct-grid?
;;;   pixel-size : non-negative-integer?
;;; Draws the grid onto a canvas at the specified coordinates, with each grid cell represented
;;; as a Tetris square of the specified pixel size.
(define canvas-grid! 
  (lambda (canvas x y grid pixel-size)
    (canvas-drawing! canvas x y (grid->drawing grid pixel-size))))

;=========================================================================
;                           [ Game Items ]
;=========================================================================

;;; A reference cell for keeping track of the score.
(define score-cell
  (ref 0))

;;; (score) -> number?
;;; Returns the current value of the score.
(define score
  (lambda ()
    (deref score-cell)))

;;; (score-set! value) -> void?
;;;   value : number?
;;; Sets the score to value.
(define score-set!
  (lambda (value)
    (ref-set! score-cell value)))

;;; (score-increase value) -> void?
;;;   value : number?
;;; Adds value to the current score.
(define score-increase
  (lambda (value)
    (score-set! (+ (score) value))))

;;; A reference cell for keeping track of how many lines have been cleared.
(define line-counter
  (ref 0))

;;; (line-count) -> nonnegative-integer?
;;; Returns the number of total lines cleared.
(define line-count
  (lambda ()
    (deref line-counter)))

;;; (level) -> nonnegative-integer?
;;; Returns the current level of the running game.
(define level
  (lambda ()
    (+ (quotient (line-count) 10) 1)))

; The grid used for the running game.
(define tetris-grid
  (create-grid 10 20))

; The grid used for the title screen.
(define title-grid
  (grid 18 5 (vector  1 1 1 2 2 2 3 3 3 4 4 0 5 5 5 0 6 6
                      0 1 0 2 0 0 0 3 0 4 0 4 0 5 0 6 0 0
                      0 1 0 2 2 0 0 3 0 4 4 0 0 5 0 0 6 0
                      0 1 0 2 0 0 0 3 0 4 0 4 0 5 0 0 0 6
                      0 1 0 2 2 2 0 3 0 4 0 4 5 5 5 6 6 0)))

;;; The "GAME OVER" image.
(define game-over-image
  (overlay (text "GAME OVER" 20 "red")
           (solid-rectangle 150 40 (rgb 0 0 0 0))))

;;; The image for the "Play Again?" button.
(define play-again-image
  (overlay (text "Play Again?" 20 (rgb 191 0 255))
           (solid-rectangle 150 40 (rgb 39 39 39))))

;;; The image for the "Menu" button.
(define menu-image
  (overlay (text "Menu" 20 (rgb 191 0 255))
           (solid-rectangle 150 40 (rgb 39 39 39))))

;;; (play-button-image hue) -> image?
;;;   hue : hue?
;;; REturns the image of the "PLAY" button which changes colors.
(define play-button-image
  (lambda (hue)
    (overlay (text "PLAY" 20 (hsv->rgb (hsv hue 100 100)))
             (solid-rectangle 150 40 (rgb 39 39 39)))))

;=========================================================================
;                           [ Animations ]
;=========================================================================

;;; The struct used for animation.
(struct animation
  (image x y vx vy f lifespan c))

;;; The vector of all animations.
(define active-animations
  (make-vector 20 null))

;;; (find-empty-index i) -> integer?
;;;   i : non-negative-integer?
;;; Finds the first empty index (containing `null`) in the `active-animations` vector starting
;;; from index `i`. Returns the index if found; otherwise, returns `-1`.
(define find-empty-index
  (lambda (i)
    (if (>= i (vector-length active-animations))
        -1
        (if (null? (vector-ref active-animations i))
            i 
            (find-empty-index (+ i 1))))))

;;; (create-animation image x y vx vy f lifespan) -> void?
;;;   image : image?
;;;   x : number? 
;;;   y : number? 
;;;   vx : number? 
;;;   vy : number? 
;;;   lifespan : positive-integer? 
;;; Creates a new animation with the specified parameters and stores it in the first available
;;; index of `active-animations`. Does nothing if the vector is full.
(define create-animation
  (lambda (image x y vx vy f lifespan)
    (let* ([new (animation image x y vx vy f lifespan 0)]
           [index (find-empty-index 0)])
          (if (>= index 0)
              (vector-set! active-animations index new)
              void))))

;;; (image-with-opacity img opacity) -> image?
;;;   img : image?
;;;   opacity : integer? 
;;; Creates a copy of the given image with the specified opacity
(define image-with-opacity
  (lambda (img opacity)
    (let* ([color (image-color img)]
           [r (rgb-red color)]
           [g (rgb-green color)]
           [b (rgb-blue color)])
          (image-recolor img (rgb r g b opacity)))))

;;; (draw-animation a canvas) -> void?
;;;   a : animation? or null?
;;;   canvas : canvas?
;;; Draws the animation on the canvas at its current position. Adjusts the opacity of the
;;; image based on the fade factor and current time step.
(define draw-animation
  (lambda (a canvas)
    (match a
      [null null]
      [(animation img x y _ _ f lifespan c)
       (let* ([percent (/ c lifespan)]
              [opc (if (= f 1) 1 (min 1 (/ (- percent 1) (- f 1))))]
              [draw (image-with-opacity img (* 255 opc))])
             (begin
               (canvas-drawing! canvas x y draw)))])))

;;; (update-animation a) -> animation? or null?
;;;   a : animation? or null?
;;; Updates the state of the given animation by moving it based on its velocity and
;;; incrementing its current time step. Returns `null` if the animation has reached the end of
;;; its lifespan.
(define update-animation
  (lambda (a)
    (match a
      [null null]
      [(animation img px py vx vy f lifespan c)
       (if (>= c lifespan)
           null
           (let* ([x (+ px vx)]
                  [y (+ py vy)]
                  [c (+ 1 c)])
                 (animation img x y vx vy f lifespan c)))])))

;;; (update-all-animations) -> void?
;;; Updates all animations in the `active-animations` vector by applying `update-animation` to
;;; each entry.
(define update-all-animations
  (lambda () 
    (vector-map! update-animation active-animations)))

;; (draw-all-animations canvas) -> void?
;;;   canvas : canvas?
;;; Draws all active animations from the `active-animations` vector onto the specified canvas.
(define draw-all-animations
  (lambda (canvas)
    (vector-for-each (section draw-animation _ canvas)
                     active-animations)))

;;; (spawn-test-animation) -> void?
;;; Spawns a test animation: a red circle with a downward velocity, a fade factor of `0.5`, 
;;; and a lifespan of 30 frames.
(define spawn-test-animation
  (lambda ()
    (create-animation (solid-circle 5 "red") 200 200 0 -3 0.5 30)))

;;; (spawn-score-animation t) -> void?
;;;   t : string?
;;; Spawns a score animation: a white text animation with the given string `t`, a fade factor
;;; of `0.5`, and a lifespan of 60 frames. The animation moves upward with a vertical velocity of `-2`.
(define spawn-score-animation
  (lambda (t)
    (create-animation (text t 20 "white") 200 200 0 -2 0.5 60)))

;============================================================================
;                               [ Tetrominos ]
;============================================================================

;;; (tetromino rotations) -> tetromino?
;;;   rotations :  vector?
;;; Creates a tetromino (shape struct) with a vector containing all possible
;;; orientations as `rotations`
(struct tetromino
  (rotations))

;;; (make-rotations! grid) -> vector
;;;   grid : grid?
;;; Takes a grid as input and returns a vector containing all possible 
;;; orientations of the grid
(define make-rotations!
  (lambda (grid)
    (vector grid 
            (grid-rotate grid) 
            ((o grid-rotate grid-rotate) grid)
            ((o grid-rotate grid-rotate grid-rotate) grid))))

;;; (shape->drawing shape index) -> image?
;;;    shape : tetromino?
;;;    index : non-negative integer?
;;; Converts the grid at (tetromino-rotations shape index) to an image.
;;; Returns the image produced.
(define shape->drawing
  (lambda (shape index)
    (grid->drawing (vector-ref (tetromino-rotations shape) index) 20)))

;;; The I Tetromino's versions.
(define I
  (let* ([shape-grid (create-grid 4 4)]
         [base-shape
          (ignore
            (begin
              (grid-set! shape-grid 0 1 5)
              (grid-set! shape-grid 1 1 5)
              (grid-set! shape-grid 2 1 5)
              (grid-set! shape-grid 3 1 5)))])
        (tetromino (make-rotations! shape-grid))))

;;; The J Tetromino's versions.
(define J
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape
          (ignore
            (begin
              (grid-set! shape-grid 0 0 6)
              (grid-set! shape-grid 0 1 6)
              (grid-set! shape-grid 1 1 6)
              (grid-set! shape-grid 2 1 6)))])
        (tetromino (make-rotations! shape-grid))))

;;; The L Tetromino's versions.
(define L
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape
          (ignore
            (begin
              (grid-set! shape-grid 3 0 2)
              (grid-set! shape-grid 3 1 2)
              (grid-set! shape-grid 2 1 2)
              (grid-set! shape-grid 1 1 2)))])
        (tetromino (make-rotations! shape-grid))))

;;; The O Tetromino's versions.
(define O
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape
          (ignore
            (begin
              (grid-set! shape-grid 1 1 3)
              (grid-set! shape-grid 2 1 3)
              (grid-set! shape-grid 1 2 3)
              (grid-set! shape-grid 2 2 3)))])
        (tetromino (make-rotations! shape-grid))))

;;; The S Tetromino's versions.
(define S
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape
          (ignore
            (begin
              (grid-set! shape-grid 1 0 4)
              (grid-set! shape-grid 2 0 4)
              (grid-set! shape-grid 0 1 4)
              (grid-set! shape-grid 1 1 4)))])
        (tetromino (make-rotations! shape-grid))))

;;; The Z Tetromino's versions.
(define Z
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape
          (ignore
            (begin
              (grid-set! shape-grid 0 0 1)
              (grid-set! shape-grid 1 0 1)
              (grid-set! shape-grid 1 1 1)
              (grid-set! shape-grid 2 1 1)))])
        (tetromino (make-rotations! shape-grid))))

;;; The T Tetromino's versions.
(define T
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape
          (ignore
            (begin
              (grid-set! shape-grid 1 0 7)
              (grid-set! shape-grid 0 1 7)
              (grid-set! shape-grid 1 1 7)
              (grid-set! shape-grid 2 1 7)))])
        (tetromino (make-rotations! shape-grid))))

;=========================================================================
;                         [ Spawing shapes ]
;=========================================================================
;;; A list of all spawnable shapes. Pair containing the shape name and 
;;; spawnable indeces. Indeces are repeated for all shapes except
;;; O to even out the probabilities of getting each shape.
(define spawnable-shapes
  (vector (pair I (list 0 2 0 2))
          (pair J (list 0 1 0 1))
          (pair L (list 1 2 1 2))
          (pair O (list 0 1 2 3))
          (pair S (list 0 2 0 2))
          (pair Z (list 0 2 0 2))
          (pair T (list 0 0 0 0))))

;;; A reference cell that keeps track of if the current Tetromino is rotated or not.
(define rotation
  (ref "none"))

;;; A reference cell that keeps track of the current Tetromino version's index in its vector of versions.
(define shape-index
  (ref 0))

;;; A reference cell that keeps track of the current Tetromino in grid form.
(define current-shape
  (ref void))

;;; A reference cell that keeps track of the x-coordinate of the current Tetromino.
(define x-position
  (ref 3))

;;; A reference cell that keeps track of the y-coordinate of the current Tetromino.
(define y-position
  (ref 0))

;;; A reference cell that keeps track of the time that the current Tetromino has been 
(define shape-spawn-time
  (ref 0))

;;; A reference cell for the queue of the next Tetrominos to be spawned from the queue as a grid.
(define queue
  (ref (vector-range 10)))

;;; A reference cell that stores the index of the next Tetrominos to be spawned in the queue.
(define queue-index
  (ref 0))

;;; A reference cell that stores the Tetromino to be spawned next.
(define next
  (ref void))

;;; A reference cell that stores the current Tetromino as a drawing.
(define current-shape-drawing
  (ref void))

;;; A reference cell that stores the next Tetromino to be spawned from the queue as a drawing.
(define next-shape-drawing
  (ref 0))

;;; (generate-queue!) -> void
;;; Makes a vector of shapes to spawn. Each item in the queue contains the shape and the
;;; index to spawn. Updates the reference cell `queue` with this vector and `next` with
;;; the first item in the queue.
(define generate-queue!
  (lambda ()
    (ignore 
      (let* ([q (deref queue)])
            (vector-for-each
              (lambda (i)
                (let* ([shape
                        (vector-ref spawnable-shapes 
                                    (random (vector-length spawnable-shapes)))]
                       [indeces (cdr shape)])
                      (begin
                        (vector-set! q i (pair (car shape) 
                                         (list-ref indeces (random (length indeces)))))
                        (ref-set! queue q)
                        (ref-set! next (vector-ref (deref queue) 0))
                        (ref-set! next-shape-drawing (shape->drawing (car (deref next))
                                                                     (cdr (deref next)))))))
            (vector-range 10))))))

;;; The initial call to generate the queue of Tetrominos.
(ignore
  (begin
    (generate-queue!)
    (deref next)))

;;; (spawn-shape!) -> void
;;; Using the generated queue stored in `queue`, the next item in the queue (`next`) and
;;; the `queue-index`, the next shape in the queue is selected and converted to a drawing
;;; stored in `current-shape-drawing`. The `x-position` and `y-position` are also updated.
;;; Checks the `queue-index` and either increments it or sets it to zero and calls 
;;; (generate-queue!) if it's at the end of the queue. Updates `next` and converts the 
;;; next shape into a drawing as well.
(define spawn-shape!
  (lambda ()
    (ignore 
      (let* ([q-index (deref queue-index)]
             [shape (car (deref next))]
             [index (cdr (deref next))]
             [q (deref queue)])
            (begin
              (ref-set! current-shape shape)
              (ref-set! shape-index index)
              (ref-set! current-shape-drawing
                        (shape->drawing shape index))
              (ref-set! x-position 3)
              (ref-set! y-position 0)
              (if (< q-index 9)
                  (ref-set! queue-index (+ 1 q-index))
                  (begin
                    (generate-queue!)
                    (ref-set! queue-index 0)))
              (ref-set! next
                        (vector-ref (deref queue) (deref queue-index)))
              (ref-set! next-shape-drawing
                        (shape->drawing (car (deref next))
                                        (cdr (deref next)))))))))

;;; (get-shape-grid) -> grid?
;;; Returns the shape grid of the curent shape
(define get-shape-grid
  (lambda ()
    (vector-ref (tetromino-rotations (deref current-shape))
                (deref shape-index))))

;;; (get-next-shape-grid) -> grid?
;;; Returns the shape grid of the current shape
(define get-next-shape-grid
  (lambda ()
    (vector-ref (tetromino-rotations (car (deref next)))
                (cdr (deref next)))))

;;; (get-shape-rotation index) -> grid?
;;;    index : non-negative integer?
;;; Returns the shape grid of the specifed index in (tetromino-rotations 
;;; (deref current-shape))
(define get-shape-rotation 
  (lambda (index)
    (vector-ref (tetromino-rotations (deref current-shape))
                index)))

;=========================================================================
;                      [ Update/Draw Functions ]
;=========================================================================

;;; (reset-game) -> void?
;;; CLears the entire game grid, resets the score and the line counter, and
;;; spawns a new shape.
(define reset-game
  (lambda ()
    (begin
      (grid-fill! tetris-grid 0)
      (score-set! 0)
      (ref-set! line-counter 0)
      (spawn-shape!))))

;;; (update-game) -> void?
;;; Detects the current position of the current Tetromino and allows movement and
;;; rotation if appropriate, allows the clearance of full rows, updates the score,
;;; updates the line-counter, and plays the animations and sound effects
;;; for their respective action.
(define update-game
  (lambda ()
    (begin
      (let* ([move-down? (= (remainder (frame-number) (max (- 42 (* 2 (level))) 1)) 0)])
            (if move-down?
                (move-down)
                void))
      (let* ([row-count (clear-full-rows! tetris-grid 0 0)])
            (begin
              (ref-set! line-counter
                        (+ (deref line-counter) row-count))
              (cond
                [(= row-count 1)
                 (begin 
                   (score-increase 100)
                   (spawn-score-animation "+100")
                   (play-composition line-clear-effect))]
                [(= row-count 2)
                 (begin
                   (score-increase 300)
                   (spawn-score-animation "+300")
                   (play-composition line-clear-effect))]
                [(= row-count 3)
                 (begin
                   (score-increase 500)
                   (spawn-score-animation "+500")
                   (play-composition line-clear-effect))]
                [(= row-count 4)
                 (begin
                   (score-increase 800)
                   (spawn-score-animation "Tetris! +800")
                   (play-composition line-clear-effect))]
                [else void])))
      (update-all-animations))))

;;; (draw-game) -> void?
;;; Draws all the appropriate drawings to the canvas for the running state of the game.
(define draw-game
  (lambda ()
    (begin 
      (canvas-rectangle! canv 0 0 100 400 "solid" (rgb 0 0 128))
      (canvas-rectangle! canv 300 0 100 400 "solid" (rgb 0 0 128))
      (canvas-text! canv 5 20  (string-append "Level " (number->string (level))) 20 "solid" (rgb 128 128 128))
      (canvas-text! canv 5 60  "Score:" 20 "solid" (rgb 128 128 128))
      (canvas-text! canv 5 80 (number->string (score)) 20 "solid" (rgb 128 128 128))
      (canvas-text! canv 5 120  "Lines:" 20 "solid" (rgb 128 128 128))
      (canvas-text! canv 5 140 (number->string (line-count)) 20 "solid" (rgb 128 128 128))
      (canvas-grid! canv 100 0 tetris-grid 20)
      (canvas-drawing! canv (+ (* (deref x-position) 20) 100) (* (deref y-position) 20) (deref current-shape-drawing))
      (canvas-grid! canv 320 100 (get-next-shape-grid) 20)
      (draw-all-animations canv))))

;;; (update-game-over-screen) -> void?
;;; There is nothing that needs updating on the game over screen so this function does nothing.
(define update-game-over-screen
  (lambda ()
    ()))

;;; (draw-game-over-screen) -> void?
;;; Calls the drawing function for the running state and draws the game over drawings on top of that.
(define draw-game-over-screen
  (lambda ()
    (begin (draw-game)
           (canvas-rectangle! canv 100 0 200 400 "solid" (rgb 0 0 0 (round (min 150 (/ (game-time) 3)))))
           (canvas-drawing! canv 125 (round (min 150 (/ (game-time) 3))) game-over-image)
           (canvas-drawing! canv 125 (- 400 (round (min 200 (/ (game-time) 2.25)))) play-again-image)
           (canvas-drawing! canv 125 (+ 50 (- 400 (round (min 200 (/ (game-time) 2.25))))) menu-image))))

;;; (update-menu-screen) -> void?
;;; There is nothing that needs updating on the menu screen so this function does nothing.
(define update-menu-screen
  (lambda ()
      ()))

;;; (draw-menu-screen) -> void?
;;; Calls the drawing function for the menu state and draws the game over drawings on top of that, along
;;; with the appropriate animations.
(define draw-menu-screen
  (lambda ()
    (begin (canvas-grid! canv 20 50 title-grid 20)
           (canvas-text! canv 200 170 "made in scamper™" 20 "solid" (rgb 94 17 158))
           (canvas-drawing! canv 125 290 (play-button-image (remainder (round (/ (game-time) 10)) 360)))
           (draw-all-animations canv))))

;;; (receive-mouse-hover x y) -> void?
;;;   x : nonnegative-number?
;;;   y : nonnegative-number?
;;; Detects the coordinates of the mouse.
(define recieve-mouse-hover
  (lambda (x y)
    ()))

;;; (receive-mouse-click x y) -> void?
;;;   x : nonnegative-number?
;;;   y : nonnegative-number?
;;; Makes the state-changing buttons on the menu and game over screen work if the mouse is clicked
;;; at the correct coordinates.
(define recieve-mouse-click
  (lambda (x y)
    (cond
      [(menu?) 
       (if (mouse-rectangle-collision? x y 125 290 150 40)
           (begin
              (state-set! state-running)
              (generate-queue!))
           void)]
      [(running?) 
       void]
      [(game-over?)
       (if (mouse-rectangle-collision? x y 125 200 150 40)
           (begin
              (state-set! state-running)
              (generate-queue!))
           (if (mouse-rectangle-collision? x y 125 250 150 40)
               (state-set! state-menu)
               void))])))

;;; (receive-key-press key) -> void?
;;;   key : key?
;;; Detects the keys pressed and allows rotation and movement for the respective key(s) of each action.
(define recieve-key-press
  (lambda (key)
    (if (running?)
        (cond
          [(and (or (equal? key "x")
                    (equal? key "z"))
                    (equal? (deref rotation)
                "none"))
           (begin 
             (ref-set! rotation "rotating")
             (rotate-shape key))]
          [(or (equal? key "ArrowRight")
               (equal? key "ArrowLeft")
               (equal? key "ArrowDown"))
           (move key)]
          [else void])
        void)))

;=========================================================================
;                     [ Main Game Loop Control ]
;=========================================================================

;;; A reference cell for the time since the window was first loaded.
(define clock
  (ref 0))

;;; A reference cell that counts the number of frames.
(define frame-counter
  (ref 0))

;;; A reference cell for the time since the state of the game was last changed.
(define state-change
  (ref 0))

;;; (time) -> nonnegative-number?
;;; Returns the time since the window was first loaded.
(define time
  (lambda ()
    (deref clock)))

;;; (frame-number) -> nonnegative-integer?
;;; Returns the number of frames.
(define frame-number
  (lambda ()
    (deref frame-counter)))

;;; (game-time) -> nonnegative-number?
;;; Returns the time the game has been running.
(define game-time
  (lambda ()
    (- (deref clock)
       (deref state-change))))

;;; A reference cell that keeps track of the state of the game.
(define game-state
  (ref 0))

;;; The bind of the menu state to value 1.
(define state-menu 1)

;;; The bind of the running state to value 2.
(define state-running 2)

;;; The bind of the game over state to value 3.
(define state-game-over 3)

;;; (menu?) -> boolean?
;;; Returns #t if the current game-state is the menu. Returns #f otherwise.
(define menu?
  (lambda ()
    (equal? (deref game-state)
            state-menu)))

;;; (running?) -> boolean?
;;; Returns #t if the current game-state is running. Returns #f otherwise.
(define running?
  (lambda ()
    (equal? (deref game-state)
            state-running)))

;;; (game-over?) -> boolean?
;;; Returns #t if the current game-state is game over. Returns #f otherwise.
(define game-over?
  (lambda ()
    (equal? (deref game-state)
            state-game-over)))

;;; (state-set! state) -> void?
;;;   state : any?
;;; Sets the game-state to the given state and updates the state-change reference cell
;;; to the current time since the window was loaded. If the current state is running,
;;; it resets the game.
(define state-set!
  (lambda (state)
    (begin
      (ref-set! game-state state) 
      (ref-set! state-change (time))
      (if (running?)
          (reset-game)
          void))))
             
;=========================================================================
;                     [ Reactive Canvas Functions ]
;=========================================================================

;;; (update-game-loop t) -> boolean?
;;;   t : nonnegative-number?
;;; Updates the reference cells clock and frame-counter and continuously
;;; calls the update function of the current game-state. Returns #t when it is called.
(define update-game-loop
  (lambda (t)
    (begin
      (ref-set! clock t)
      (ref-set! frame-counter (+ (deref frame-counter) 1))
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

;;; (draw-screen state canvas) -> boolean?
;;;   state : nonnegative-integer? (1-3)
;;;   canvas : canvas?
;;; Generates the canvas given and continuously calls the drawing function for the
;;; current game state.
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


;;; (handle-event event state) -> any-of boolean? void?
;;;   event : message?
;;; In the instance of the appropriate time passed, keys pressed or coordinates of mouse click,
;;; calls the functions that deal with them.
(define handle-event
  (lambda (event state)
    (match event
      [(event-timer t _) (update-game-loop t)]
      [(event-mouse-hover x y) (recieve-mouse-hover x y)]
      [(event-mouse-click b x y) (recieve-mouse-click x y)]
      [(event-key-down key) (recieve-key-press key)])))

;;; The reactive canvas of the game.
(define canv
  (reactive-canvas 400 400 #t draw-screen handle-event 
                   (on-timer (/ 1000 60)) 
                   (on-key-down) 
                   (on-mouse-click) 
                   (on-mouse-hover)))

;;; (attempt-movement shape-grid x-move y-move) -> boolean?
;;;    shape-grid : grid?
;;;    x-move : non-negative integer?
;;;    y-move : non-negative integer?
;;; Checks to see if a movement is valid and returns #t if it is
(define attempt-movement
  (lambda (shape-grid x-move y-move)
    (if (shape-collision? tetris-grid
                          shape-grid
                          (+ (deref x-position) x-move) 
                          (+ (deref y-position) y-move))
        #f
        (begin (ref-set! x-position (+ (deref x-position) x-move))
               (ref-set! y-position (+ (deref y-position) y-move))
               #t))))

;;; (rotate-shape key) -> void?
;;;    key : string?
;;; Sets the current shape to the one before or after it if the movement is valid
(define rotate-shape
  (lambda (key)
    (let* ([i (+ (deref shape-index) 4)]
           [next-rotation (remainder (+ i 1) 4)]
           [previous-rotation (remainder (- i 1) 4)])
          (begin
            (if (equal? key "x")
                (begin
                  (if (attempt-movement (get-shape-rotation next-rotation) 0 0)
                      (ref-set! shape-index next-rotation)
                      void))
                (begin
                  (if (attempt-movement (get-shape-rotation previous-rotation) 0 0)
                      (ref-set! shape-index previous-rotation)
                      void)))
          (ref-set! current-shape-drawing
                    (shape->drawing (deref current-shape) 
                                    (deref shape-index)))
          (ref-set! rotation "none")))))

;;; (move-down) -> void
;;; Moves the shape down when the down arrow is pressed if `attempt-movement` is #t
(define move-down
  (lambda ()
    (if (not (attempt-movement (get-shape-grid) 0 1))
        (begin
          (play-composition block-ground-effect)
          (if (block-freeze! tetris-grid
                             (get-shape-grid) 
                             (deref x-position) 
                             (deref y-position))
              (begin
                (play-composition game-over)
                (state-set! state-game-over))
              void)
          (spawn-shape!))
        void)))

;;; (move key) -> void
;;; Moves the shape in the direction specified by the key if `attempt-movement` is #t.
;;; For `ArrowDown`, calls (move-down)
(define move
  (lambda (key)
    (let* ([x-pos (deref x-position)]
           [y-pos (deref y-position)])
          (cond 
            [(equal? key "ArrowRight")
             (attempt-movement (get-shape-grid) 1 0)]
            [(equal? key "ArrowLeft")
             (attempt-movement (get-shape-grid) -1 0)]
            [(equal? key "ArrowDown")
             (begin
               (move-down)
               (score-increase 1))]
            [else void]))))

;;; The initial Tetromino spawned.
(ignore
  (spawn-shape!))

;;; The display of the game canvas.
canv