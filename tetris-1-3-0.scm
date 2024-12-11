; tetris.scm
; CSC-151-02 F24 Final Project
; v.1.3.0
; Made by The Tetromino Titans:
;  - Steph Kama-Kama
;  - Rayane Nossair
;  - Dimitra Giantsidi
;  - Leo Eakin
;  - Aarav Sharma

(import image)
(import canvas)
(import html)
(import reactive)
(import music)


;========================================================================
;                         [ Music & Sound Effects ]
;========================================================================

;;; Import of instruments used:
;;; Square Lead:
(ignore (load-instrument 81))

;;; Saw Wave Lead:
(ignore (load-instrument 82))

;;; Bass Drum for the block-ground effect:
(ignore (load-percussion 35))

;;; Gunshot:
(ignore (load-instrument 128))

;;; Tubular Bells:
(ignore (load-instrument 14))

;;; Acoustic Grand Piano:
(ignore (load-instrument 0))

;;; Enablement of high quality instruments:
(ignore (use-high-quality-instruments #t))

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

;;; Call of the music theme.
(ignore (play-composition (mod (dynamics 20) tetris-real)))
        
;=========================================================================
;                               [ Tetris Grid ]
;=========================================================================

;;; (grid width height contents) -> struct-grid?
;;;   width : nonnegative-integer?
;;;   height : nonnegative-integer?
;;;   contents : vector?
;;; Creates a struct-grid consisting of width, height and contents.
(struct grid (width height contents))

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

;;; (hzzzzizghlight color) -> hsv?
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
;;; Returns #f the height of shape-grid is lower than y or if the coordinates of the contents of
;;; game-grid are within the area of the coordinates of the shape (shape-grid) added to x and y respectively
;;; and if there is no Tetris square in the coordinates (x,y) of shape-grid at the same time.
;;; Returns #t otherwise.
(define shape-collision?/helper
  (lambda (game-grid shape-grid shape-x shape-y x y)
    (cond [(>= y (grid-height shape-grid)) #f]
          [(>= x (grid-width shape-grid))
           (shape-collision?/helper game-grid shape-grid shape-x shape-y 0 (+ y 1))] 
          [(not (or (grid-in-bounds? game-grid (+ shape-x x) (+ shape-y y))
                    (equal? (grid-ref shape-grid x y) 0))) #t]
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

(define block-freeze-helper
  (lambda (game-grid block-grid block-x block-y x y)
    (cond [(>= y (grid-height block-grid)) #f]
          [(>= x (grid-width block-grid))
           (block-freeze-helper game-grid block-grid block-x block-y 0 (+ y 1))]   
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
                        (block-freeze-helper game-grid block-grid block-x block-y (+ x 1) y))))])))

(define block-freeze!
  (lambda (game-grid block-grid block-x block-y)
    (block-freeze-helper game-grid block-grid block-x block-y 0 0)))

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
      [(>= y (grid-width grid)) #t]             
      [(equal? (grid-ref grid y x) 0) #f]          
      [else (detect-full-row grid x (+ y 1))]))) 

;;; (clear-row!/helper grid i y) -> void?
;;;   grid : struct-grid?
;;;   i : nonnegative-integer?
;;;   y : nonnegative-integer?
;;; Sets the elements of the yth row in the contents of the given grid to the elements of the line above them
;;; from the first element of the yth row up to the ith element.
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

(define clear-full-rows
  (lambda (grid y number-cleared)
    (cond
      [(>= y (grid-height grid)) number-cleared]
      [(detect-full-row grid y 0)
       (begin
         (clear-row! grid y)
         (clear-full-rows grid (+ y 1) (+ number-cleared 1)))]
      [else
       (clear-full-rows grid (+ y 1) number-cleared)])))

;=========================================================================
;             [ Functions to display a grid on a canvas ]
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


(define line-counter (ref 0))
(define line-count (lambda () (deref line-counter)))
(define level (lambda () (+ (quotient (line-count) 10) 1)))



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
;=========================================================================
;                           [ Animations ]
;=========================================================================
(struct animation (image x y vx vy f lifespan c))
(define active-animations (make-vector 20 null))

(define find-empty-index
  (lambda (i)
    (if (>= i (vector-length active-animations))
      -1
      (if (null? (vector-ref active-animations i)) i 
          (find-empty-index (+ i 1))))))

(define create-animation
  (lambda (image x y vx vy f lifespan)
    (let* ([new (animation image x y vx vy f lifespan 0)]
           [index (find-empty-index 0)])
          (if (>= index 0)
            (vector-set! active-animations index new)
            void))))
 
(define image-with-opacity
  (lambda (img opacity)
    (let* ([color (image-color img)]
           [r     (rgb-red color)]
           [g     (rgb-green color)]
           [b     (rgb-blue color)])
    (image-recolor img (rgb r g b opacity)))))

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

(define update-all-animations
  (lambda () 
    (vector-map! update-animation active-animations)))

(define draw-all-animations
  (lambda (canvas)
    (vector-for-each (section draw-animation _ canvas) active-animations)))

(define spawn-test-animation
  (lambda () (create-animation (solid-circle 5 "red") 200 200 0 -3 0.5 30)))

(define spawn-score-animation
  (lambda (t) (create-animation (text t 20 "white") 200 200 0 -2 0.5 60)))

;============================================================================
;                               [ Shapes ]
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

(define J
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 0 0 6)
                         (grid-set! shape-grid 0 1 6)
                         (grid-set! shape-grid 1 1 6)
                         (grid-set! shape-grid 2 1 6)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(define L
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 3 0 2)
                         (grid-set! shape-grid 3 1 2)
                         (grid-set! shape-grid 2 1 2)
                         (grid-set! shape-grid 1 1 2)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(define O
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 1 1 3)
                         (grid-set! shape-grid 2 1 3)
                         (grid-set! shape-grid 1 2 3)
                         (grid-set! shape-grid 2 2 3)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(define S
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 1 0 4)
                         (grid-set! shape-grid 2 0 4)
                         (grid-set! shape-grid 0 1 4)
                         (grid-set! shape-grid 1 1 4)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(define Z
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 0 0 1)
                         (grid-set! shape-grid 1 0 1)
                         (grid-set! shape-grid 1 1 1)
                         (grid-set! shape-grid 2 1 1)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

(define T
  (let* ([shape-grid (create-grid 3 3)]
         [base-shape (ignore
                       (begin
                         (grid-set! shape-grid 1 0 7)
                         (grid-set! shape-grid 0 1 7)
                         (grid-set! shape-grid 1 1 7)
                         (grid-set! shape-grid 2 1 7)))])
        (tetromino 0 0 (make-rotations! shape-grid))))

;=========================================================================
;                         [ Spawing shapes ]
;=========================================================================

(define spawnable-shapes
  (vector (pair I (list 0 2 0 2))
          (pair J (list 0 1 0 1))
          (pair L (list 1 2 1 2))
          (pair O (list 0 1 2 3))
          (pair S (list 0 2 0 2))
          (pair Z (list 0 2 0 2))
          (pair T (list 0 0 0 0))))

(define rotation
  (ref "none"))

(define shape-index
  (ref 0))

(define current-shape
  (ref void))

(define x-position
  (ref 3))

(define falling-y-position
  (ref 0))

(define y-position
  (ref 0))

(define shape-spawn-time
  (ref 0))

(define queue
  (ref (vector-range 10)))

(define queue-index
  (ref 0))

(define next
  (ref void))

(define current-shape-drawing
  (ref void))

(define next-shape-drawing
  (ref 0))

(define test-canvas
  (let ([canv (make-canvas 100 100)])
       (begin
         (ignore (canvas-rectangle! canv 0 0 100 100 "solid" "black"))
         canv)))

(define generate-queue!
  (lambda ()
    (ignore (let* ([q (deref queue)])
                  (vector-for-each
                    (lambda (i)
                      (let* ([shape (vector-ref spawnable-shapes 
                                               (random (vector-length spawnable-shapes)))]
                             [indeces (cdr shape)])
                            (begin
                              (vector-set! q i (pair (car shape) (list-ref indeces (random (length indeces)))))
                              (ref-set! queue q)
                              (ref-set! next (vector-ref (deref queue) 0))
                              (ref-set! next-shape-drawing (shape->drawing (car (deref next))
                                                                           (cdr (deref next)))))))
                      (vector-range 10))))))

(ignore (begin (generate-queue!)
               (deref next)))

(define spawn-shape!
  (lambda ()
    (ignore (let* ([q-index (deref queue-index)]
                   [shape (car (deref next))]
                   [index (cdr (deref next))]
                   [q (deref queue)])
                  (begin
                    (ref-set! current-shape shape)
                    (ref-set! shape-index index)
                    (ref-set! current-shape-drawing (shape->drawing shape index))
                    (ref-set! x-position 3)
                    (ref-set! y-position 0)
                    (if (< q-index 9)
                        (begin
                          (ref-set! queue-index (+ 1 q-index)))
                        (begin
                          (generate-queue!)
                          (ref-set! queue-index 0)))
                    (ref-set! next (vector-ref (deref queue) (deref queue-index)))
                    (ref-set! next-shape-drawing (shape->drawing (car (deref next))
                                                                      (cdr (deref next)))))))))

(define get-shape-grid
  (lambda ()
    (vector-ref (tetromino-rotations (deref current-shape)) (deref shape-index))))

(define get-next-shape-grid
  (lambda ()
    (vector-ref (tetromino-rotations (car (deref next))) (cdr (deref next)))))

(define get-shape-rotation 
  (lambda (index)
    (vector-ref (tetromino-rotations (deref current-shape)) index)))

;=========================================================================
;                      [ Update/Draw Functions ]
;=========================================================================

(define reset-game
  (lambda ()
    (begin
      (grid-fill! tetris-grid 0)
      (score-set 0)
      (ref-set! line-counter 0)
      (spawn-shape!))))

(define update-game
  (lambda ()
    (begin
      (let* ([move-down? (= (remainder (frame-number) (max (- 42 (* 2 (level))) 1)) 0)])
            (if move-down?
                (move-down)
                void))
       
      (let* ([row-count (clear-full-rows tetris-grid 0 0)])
            (begin
              (ref-set! line-counter (+ (deref line-counter) row-count))
              (cond
                [(= row-count 1) (begin 
                  (score-increase 100)
                  (spawn-score-animation "+100")
                  (play-composition line-clear-effect))]
                [(= row-count 2) (begin
                  (score-increase 300)
                  (spawn-score-animation "+300")
                  (play-composition line-clear-effect))]
                [(= row-count 3) (begin
                  (score-increase 500)
                  (spawn-score-animation "+500")
                  (play-composition line-clear-effect))]
                [(= row-count 4) (begin
                  (score-increase 800)
                  (spawn-score-animation "Tetris! +800")
                  (play-composition line-clear-effect))]
                [else void])))
      (update-all-animations))))

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
      

(define update-game-over-screen
  (lambda ()
    ()))

(define draw-game-over-screen
  (lambda ()
    (begin (draw-game)
           (canvas-rectangle! canv 100 0 200 400 "solid" (rgb 0 0 0 (round (min 150 (/ (game-time) 3)))))
           (canvas-drawing! canv 125 (round (min 150 (/ (game-time) 3))) game-over-image)
           (canvas-drawing! canv 125 (- 400 (round (min 200 (/ (game-time) 2.25)))) play-again-image))))

(define update-menu-screen
  (lambda ()
      ()))

(define draw-menu-screen
  (lambda ()
    (begin (canvas-grid! canv 20 50 title-grid 20)
           (canvas-text! canv 200 170 "made in scamper™" 20 "solid" (rgb 94 17 158))
           (canvas-drawing! canv 125 290 (play-button-image (remainder (round (/ (game-time) 10)) 360)))
           (draw-all-animations canv))))

(define recieve-mouse-hover
  (lambda (x y)
    ()))

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
           void)])))

(define recieve-key-press
  (lambda (key)
    (if (running?)
      (cond
        [(and (or (equal? key "x") (equal? key "z")) (equal? (deref rotation) "none"))
         (begin 
           (ref-set! rotation "rotating")
           (rotate-shape key canv))]
        [(or (equal? key "ArrowRight") (equal? key "ArrowLeft") (equal? key "ArrowDown"))
             (move key canv)]
        [(equal? key "a") 
          (spawn-test-animation)]
        [else void])
      void)))

;=========================================================================
;                     [ Main Game Loop Control ]
;=========================================================================

; clock cells allow current time to be accessed from anywhere
(define clock (ref 0))
(define frame-counter (ref 0))

(define state-change (ref 0))

; (time) returns the time since the window loaded
(define time (lambda () (deref clock)))

(define frame-number (lambda () (deref frame-counter)))

; (game-time) returns the time since the state most recently changed
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

; sets the game state to a specific value and resets the game clock
(define state-set! (lambda (state) (begin (ref-set! game-state state) 
                                          (ref-set! state-change (time))
                                          (if (running?) (reset-game) void))))
             
;=========================================================================
;                     [ Reactive Canvas Functions ]
;=========================================================================

; this is the main update function that updates the clock and draws the correct screen
; depending on what state the game is in
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

;returns #t if the movement succeeded
(define attempt-movement
  (lambda (shape-grid x-move y-move)
    (if (shape-collision? tetris-grid shape-grid (+ (deref x-position) x-move) (+ (deref y-position) y-move))
        #f
        (begin (ref-set! x-position (+ (deref x-position) x-move))
               (ref-set! y-position (+ (deref y-position) y-move))
               #t))))

(define rotate-shape
  (lambda (key canvas)
    (let* ([i (+ (deref shape-index) 4)]
           [next-rotation (remainder (+ i 1) 4)]
           [previous-rotation (remainder (- i 1) 4)])
         (begin
            (if (equal? key "x")
                (begin
                  (if (attempt-movement (get-shape-rotation next-rotation) 0 0)
                      (ref-set! shape-index next-rotation) void))
                (begin
                  (if (attempt-movement (get-shape-rotation previous-rotation) 0 0)
                      (ref-set! shape-index previous-rotation) void)))
            (ref-set! current-shape-drawing (shape->drawing (deref current-shape) (deref shape-index)))
            (ref-set! rotation "none")))))

(define move-down
  (lambda ()
    (if (not (attempt-movement (get-shape-grid) 0 1))
                (begin
                  (play-composition block-ground-effect)
                  (if (block-freeze! tetris-grid (get-shape-grid) (deref x-position) (deref y-position))
                      (begin (play-composition game-over)
                             (state-set! state-game-over))
                      void)
                  (spawn-shape!)
                  )
                void)))

(define move
  (lambda (key canvas)
    (let* ([x-pos (deref x-position)]
           [y-pos (deref y-position)])
         (cond 
           [(equal? key "ArrowRight")
            (attempt-movement (get-shape-grid) 1 0)]
           [(equal? key "ArrowLeft")
            (attempt-movement (get-shape-grid) -1 0)]
           [(equal? key "ArrowDown")
            (begin (move-down)
                   (score-increase 1))]
           [else
            void]))))

(ignore (spawn-shape!))

canv

