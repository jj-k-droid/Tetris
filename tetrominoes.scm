; tetrominoes (shapes)

(import image)
(import canvas)
(import html)

(struct tetromino
  (x y rotations))

;;; (block color) -> image?
;;;    color : color?
;;; Returns an image of a block with the given color
(define block
  (lambda (color)
    (overlay (solid-square 18 color)
             (solid-square 20 (rgb-darker (rgb-darker color))))))

(define I
  (let* ([color (rgb 1 230 254)]
         [flat (beside (block color) (block color) (block color) (block color))]
         [i (above (block color) (block color) (block color) (block color))])
  (tetromino 0 0 (vector flat i flat i))))

(tetromino-rotations I)

(define J
  (let* ([color (rgb 24 1 255)]
         [flat (above/align "left" (block color)
                            (beside (block color) (block color) (block color)))]
         [filpped-j (beside/align "top" 
                                      (above (block color) (block color) (block color))
                                      (block color))]
         [filpped-flat (above/align "right" (beside (block color) (block color) (block color))
                                        (block color))]
         [j (beside/align "bottom" (block color)
                          (above (block color) (block color) (block color)))])
  (tetromino 0 0 (vector flat filpped-j filpped-flat j))))

(tetromino-rotations J)

(define L
  (let* ([color (rgb 255 115 9)]
         [flat (above/align "right" (block color)
                            (beside (block color) (block color) (block color)))]
         [l (beside/align "bottom" 
                          (above (block color) (block color) (block color))
                          (block color))]
         [filpped-flat (above/align "left" (beside (block color) (block color) (block color) )
                                        (block color))]
         [filpped-l (beside/align "top" (block color)
                                       (above (block color) (block color) (block color)))])
  (tetromino 0 0 (vector flat l filpped-flat filpped-l))))

(tetromino-rotations L)

(define O
  (let* ([color (rgb 255 222 2)]
         [o (above (beside (block color) (block color))
                   (beside (block color) (block color)))])
        (tetromino 0 0 (vector o o o o))))

(tetromino-rotations O)

(define S
  (let* ([color (rgb 102 253 3)]
         [s (above (beside (block (rgb 0 0 0 0)) (block color) (block color))
                   (beside (block color) (block color) (block (rgb 0 0 0 0))))]
         [vertical-s (beside (above (block color) (block color) (block (rgb 0 0 0 0)))
                             (above (block (rgb 0 0 0 0)) (block color) (block color)))])
        (tetromino 0 0 (vector s vertical-s s vertical-s))))

(tetromino-rotations S)

(define Z
  (let* ([color (rgb 254 17 60)]
         [z (above (beside (block color) (block color) (block (rgb 0 0 0 0)))
                   (beside (block (rgb 0 0 0 0)) (block color) (block color)))]
         [vertical-z (beside (above (block (rgb 0 0 0 0)) (block color) (block color))
                             (above (block color) (block color) (block (rgb 0 0 0 0))))])
        (tetromino 0 0 (vector z vertical-z z vertical-z))))

(tetromino-rotations Z)

(define T
  (let* ([color (rgb 184 3 253)]
         [flat (above/align "middle" (block color)
                            (beside (block color) (block color) (block color)))]
         [vertical-1 (beside/align "center" (above (block color) (block color) (block color))
                                   (block color))]
         [flipped-flat (above/align "middle" 
                                    (beside (block color) (block color) (block color))
                                    (block color))]
         [vertical-2 (beside/align "center" (block color)
                                   (above (block color) (block color) (block color)))])
         (tetromino 0 0 (vector flat vertical-1 flipped-flat vertical-2))))

(tetromino-rotations T)
