; tetrominoes (shapes)
(import image)
(import canvas)
(import html)
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
;===================================================================
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
; [ Tetromino Grid Definitions ]
(define all-rotations
  (lambda (grid)
    (let* ([r1 (grid-rotate grid)]
           [r2 (grid-rotate r1)]
           [r3 (grid-rotate r2)])
           (vector grid r1 r2 r3))))




(define I-grid
  (grid 5 5 (vector 0 0 0 0 0
                    0 0 0 0 0
                    0 5 5 5 5
                    0 0 0 0 0
                    0 0 0 0 0)))
(define L-grid
  (grid 3 3 (vector 0 0 2
                    2 2 2
                    0 0 0)))
(define J-grid
  (grid 3 3 (vector 6 0 0
                    6 6 6
                    0 0 0)))
(define O-grid
  (grid 2 2 (vector 3 3
                    3 3)))
(define S-grid
  (grid 3 3 (vector 0 4 4
                    4 4 0
                    0 0 0)))
(define T-grid
  (grid 3 3 (vector 0 7 0
                    7 7 7
                    0 0 0)))
(define Z-grid
  (grid 3 3 (vector 1 1 0
                    0 1 1
                    0 0 0))) 
(define all-grids (vector-map all-rotations (vector I-grid L-grid J-grid O-grid S-grid T-grid Z-grid)))                 

(define draw-rotations
  (lambda (grid-vec)
    (vector-map (section grid->drawing _ 20) grid-vec)))

(vector-map draw-rotations all-grids)





