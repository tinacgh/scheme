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

;;; (draw-segment 0.0 0.0 0.1 0.1 0.2 0.2)
;;; (draw-line

(define (draw-segment . pts)
  (if (< (length pts) 5)
      (line (car pts) (cadr pts) (caddr pts) (cadddr pts))
      (overlay (line (car pts) (cadr pts) (caddr pts) (cadddr pts)) (apply draw-segment (cddr pts)))))

(define (fill-to-center . pts)
  (if (< (length pts) 5)
      (filled-triangle (car pts) (cadr pts) (caddr pts) (cadddr pts) 0 0)
      (overlay (filled-triangle (car pts) (cadr pts) (caddr pts) (cadddr pts) 0 0) (apply fill-to-center (cddr pts)))))

(define circle
  (let ((param-t (map (lambda (x) (* 2 3.1415926 (/ x 35))) (range 36))))
    (apply draw-segment (interleave (map sin param-t) (map cos param-t)))))

(define (scaled-circle horiz vert)
  (let ((param-t (map (lambda (x) (* 2 3.1415926 (/ x 35))) (range 36))))
    (apply draw-segment (interleave (map (lambda (x) (* horiz (sin x))) param-t) (map (lambda (x) (* vert (cos x))) param-t)))))

;;; NEXT: filled circle
;;; for each segment, fill a triangle like a slice of pizza
(define filled-circle
  (let ((param-t (map (lambda (x) (* 2 3.1415926 (/ x 35))) (range 36))))
    (apply fill-to-center (interleave (map sin param-t) (map cos param-t)))))

(define (scaled-filled-circle horiz vert)
  (let ((param-t (map (lambda (x) (* 2 3.1415926 (/ x 35))) (range 36))))
    (apply fill-to-center (interleave (map (lambda (x) (* horiz (sin x))) param-t) (map (lambda (x) (* vert (cos x))) param-t)))))