(define tetris
  (make-canvas 200 400))

(display tetris)

(ignore
  (begin
    (canvas-rectangle! tetris 0 0 200 400 "solid" "black")
    (canvas-drawing! tetris 70 190 (vector-ref (tetromino-rotations T) 0))))

(define rotation
  (ref "none"))

(define shape-index
  (ref 0))

(define current-shape
  (ref T))

(define rotate-shape
  (lambda (key)
    (let* ([i (deref shape-index)]
           [shape (deref current-shape)]
           [current (vector-ref (tetromino-rotations shape) i)]
           [next (vector-ref (tetromino-rotations shape) 
                             (remainder (+ i 1) 4))]
           [prev (vector-ref (tetromino-rotations shape) 
                             (remainder (- i 1) 4))])
         (begin
            (canvas-rectangle! tetris 0 0 200 400 "solid" "black")
            (if (equal? key "ArrowRight")
                (begin
                  (canvas-drawing! tetris 80 200 next)
                  (ref-set! shape-index (remainder (+ i 1) 4)))
                (begin
                  (canvas-drawing! tetris 80 200 prev)
                  (ref-set! shape-index (remainder (- i 1) 4))))
            (ref-set! rotation "none")))))

(on-keydown!
  (lambda (key)
    (cond
      [(equal? key "ArrowRight")
       (if (equal? (deref rotation) "none")
           (begin 
              (ref-set! rotation "rotating")
              (rotate-shape key))
           (rotate-shape key))]
      [(equal? key "ArrowLeft")
       (if (equal? (deref rotation) "none")
           (begin 
              (ref-set! rotation "rotating")
              (rotate-shape key))
           (rotate-shape key))])))


