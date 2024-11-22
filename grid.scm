; tetris grid
(import image)
(import canvas)
(import lab)
(define colors (list
                (rgb 0 0 0)
                 (rgb 255 0 0)
                 (rgb 255 127 0)
                 (rgb 255 255 0)
                 (rgb 0 255 0)
                 (rgb 0 255 255)
                 (rgb 0 0 255)
                 (rgb 255 0 255)))

;Grid Functions
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

(define get-row-helper 
  (lambda (grid y i)
    (if (>= i (grid-width grid))
        null
        (cons (grid-ref grid i y) (get-row-helper grid y (+ i 1))))))

(define get-row
  (lambda (grid y)
    (get-row-helper grid y 0)))

;Drawing functions
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


;functions to convert grid to a drawing
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
    (if (>= i (grid-height grid))
        (apply above so-far)
        (grid->drawing-helper grid size (+ i 1) (cons (row->drawing (get-row grid i) size) so-far)))))

(define grid->drawing 
  (lambda (grid pixel-size)
    (grid->drawing-helper grid pixel-size 0 null)))

;canvas
(define canv (make-canvas 280 100))

; 14 x 5 grid
(define test-grid (grid 14 5 (vector 7 7 7 7 0 4 4 4 4 0 0 2 2 0
                                     7 0 0 0 0 4 0 0 0 0 2 0 0 2
                                     7 0 0 0 0 4 4 4 0 0 2 0 0 2
                                     7 0 0 0 0 4 0 0 0 0 2 0 0 2
                                     7 0 0 0 0 4 4 4 4 0 0 2 2 0)))
; put the grid on the canvas
(begin (canvas-rectangle! canv 0 0 280 100 "solid" "black")
       (canvas-drawing! canv 0 0 (grid->drawing test-grid 20)))

;change pixels color and redraw the grid on click
(canvas-onclick! canv
  (lambda (x y)
    (let* ([grid-x (floor (/ x 20))]
           [grid-y (- (grid-height test-grid) (floor (/ y 20)) 1)]
           [current-value (grid-ref test-grid grid-x grid-y)]
           [new-value (remainder (+ current-value 1) 8)])
          (begin (grid-set! test-grid grid-x grid-y new-value)
                 (canvas-rectangle! canv 0 0 280 100 "solid" "black")
                 (canvas-drawing! canv 0 0 (grid->drawing test-grid 20))))))


;10 x 20 tetris grid
(define tetris-grid (create-grid 10 20))

;fill the grid with random colors
(ignore (vector-map! (section random 8) (grid-contents tetris-grid)))

;display the random color grid
(grid->drawing tetris-grid 20)

;display the canvas
(problem "click the canvas to edit the pixels \n    |\n    v")
canv





