(define (c img) (close-image img))

(define (turn-right img) (quarter-turn-right img))
(define (half-turn img) (turn-right (turn-right img)))
(define (turn-left img) (turn-right (half-turn img)))

(define (side-by-side a b)
  (turn-left (stack (turn-right a)
		    (turn-right b))))

(define (pinwheel img)
  (stack (side-by-side (turn-right img) (half-turn img))
	 (side-by-side img (turn-left img))))

(define (draw-segment x1 y1 . pts)
  (line x1 y1 (car pts) (cadr pts)))