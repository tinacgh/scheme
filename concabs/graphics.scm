;;; if images are blank try this
(define test-bb
  (resize-image
   (filled-triangle 0 1 0 -1 1 -1)
   25))

;;; to close images
(close-image #@12)

;;; custom size
(define default-image-size 200)

(define c-curve
  (lambda (x0 y0 x1 y1 level)
    (if (= level 0)
        (line x0 y0 x1 y1)
        (let ((xmid (/ (+ x0 x1) 2))
              (ymid (/ (+ y0 y1) 2))
              (dx (- x1 x0))
              (dy (- y1 y0)))
          (let ((xa (- xmid (/ dy 2)))
                (ya (+ ymid (/ dx 2))))
            (overlay (c-curve x0 y0 xa ya (- level 1))
                     (c-curve xa ya x1 y1 (- level 1))))))))

(c-curve 0 -1/2 0 1/2 10)