;; This file contains an MIT-Scheme  version of the functional graphics
;; system for use with Concrete Abstractions: An Introduction to Computer
;; Science Using Scheme, by Max Hailperin, Barbara Kaiser, and Karl Knight.

(define-structure
  (fungraph-image
   (print-procedure
    (lambda (unparser-state obj)
      (if (not (fungraph-image-gd obj))
          (let ((gd (cond ((graphics-type-available? 'win32)
                           (make-graphics-device 'win32
                                                 (fungraph-image-width obj)
                                                 (fungraph-image-height obj)
                                                 'system))
                          ((graphics-type-available? 'x)
                           (make-graphics-device 'x
                                                 #f
                                                 (x-geometry-string
                                                  #f #f
                                                  (fungraph-image-width obj)
                                                  (fungraph-image-height obj))))
                          (else #f))))
            (if gd
                (begin
                  ;; The below call-with-values is a workaround for a bug (?) in
                  ;; the win32 graphics -- there is a minimum width for windows,
                  ;; and if a window is requested that is narrower than that,
                  ;; it gets made into the minimum size, with the -1 to 1 virtual
                  ;; coordinate range stretched to the full width, rather than
                  ;; just covering the requested portion.
                  ;; Thus, we would wind up with distorted (horizontally
                  ;; stretched) images, when what we'd really prefer is just to
                  ;; not use the whole area.  The below arranges for this.
                  (call-with-values
                   (lambda () (graphics-device-coordinate-limits gd))
                   (lambda (xleft ybot xright ytop)
                     (if (> (+ (- xright xleft) 1) (fungraph-image-width obj))
                         (let ((xmax (- (* 2 (/ (+ (- xright xleft) 1)
                                                (fungraph-image-width obj)))
                                        1)))
                           (graphics-set-coordinate-limits gd -1 -1 xmax 1)
                           (graphics-set-clip-rectangle gd -1 -1 1 1)))))
                  (set-fungraph-image-gd! obj gd)
                  (graphics-enable-buffering gd)
                  (graphics-operation gd 'set-window-name
                                      (number->string (object-hash obj)))
                  ((fungraph-image-proc obj) gd
                                             (lambda (x y) x)
                                             (lambda (x y) y)
                                             "black" "white")
                  (graphics-flush gd)))))
      ((standard-unparser-method 'image
                                 (lambda (obj port)
                                   (write-char #\space port)
                                   (write (fungraph-image-width obj) port)
                                   (write-char #\x port)
                                   (write (fungraph-image-height obj) port)))
       unparser-state obj))))
  width height proc gd)

(define line
  (lambda (x0 y0 x1 y1 . wh)
    (if (not (real? x0))
        (error "x0 argument to line not a real" x0))
    (if (not (real? x1))
        (error "x1 argument to line not a real" x1))
    (if (not (real? y0))
        (error "y0 argument to line not a real" y0))
    (if (not (real? y1))
        (error "y1 argument to line not a real" y1))
    (let ((width default-image-size)
          (height default-image-size))
      (if (not (null? wh))
          (begin (set! width (car wh))
                 (if (not (null? (cdr wh)))
                     (begin (set! height (cadr wh))
                            (if (not (null? (cddr wh)))
                                (error "too many argument to line")))
                     (set! height width))))
      (if (not (and (integer? height)
                    (integer? width)
                    (exact? height)
                    (exact? width)
                    (> height 0)
                    (> width 0)))
          (error "illegal size specification in line" wh))
      (make-fungraph-image
       width height
       (lambda (gd xt yt color other-color)
         (graphics-operation gd 'set-foreground-color color)
         (graphics-draw-line gd
                             (exact->inexact (xt x0 y0))
                             (exact->inexact (yt x0 y0))
                             (exact->inexact (xt x1 y1))
                             (exact->inexact (yt x1 y1))))
       #f))))

(define filled-triangle
  (lambda (x0 y0 x1 y1 x2 y2 . wh)
    (if (not (real? x0))
        (error "x0 argument to filled-triangle not a real" x0))
    (if (not (<= -1 x0 1))
        (error "x0 argument to filled-triangle not in -1 to 1 range" x0))
    (if (not (real? x1))
        (error "x1 argument to filled-triangle not a real" x1))
    (if (not (<= -1 x1 1))
        (error "x1 argument to filled-triangle not in -1 to 1 range" x1))
    (if (not (real? x2))
        (error "x2 argument to filled-triangle not a real" x2))
    (if (not (<= -1 x2 1))
        (error "x2 argument to filled-triangle not in -1 to 1 range" x2))
    (if (not (real? y0))
        (error "y0 argument to filled-triangle not a real" y0))
    (if (not (<= -1 y0 1))
        (error "y0 argument to filled-triangle not in -1 to 1 range" y0))
    (if (not (real? y1))
        (error "y1 argument to filled-triangle not a real" y1))
    (if (not (<= -1 y1 1))
        (error "y1 argument to filled-triangle not in -1 to 1 range" y1))
    (if (not (real? y2))
        (error "y2 argument to filled-triangle not a real" y2))
    (if (not (<= -1 y2 1))
        (error "y2 argument to filled-triangle not in -1 to 1 range" y2))
    (let ((width default-image-size)
          (height default-image-size))
      (if (not (null? wh))
          (begin (set! width (car wh))
                 (if (not (null? (cdr wh)))
                     (begin (set! height (cadr wh))
                            (if (not (null? (cddr wh)))
                                (error "too many argument to filled-triangle")))
                     (set! height width))))
      (if (not (and (integer? height)
                    (integer? width)
                    (exact? height)
                    (exact? width)
                    (> height 0)
                    (> width 0)))
          (error "illegal size specification in filled-triangle" wh))
      (make-fungraph-image
       width height
       (lambda (gd xt yt color other-color)
         (graphics-operation gd 'set-foreground-color color)
         ;; Drawing the three boundary lines appears to be necessary
         ;; under win32, though not under X, but we might as well do
         ;; it unconditionally.  If the lines aren't drawn, and instead
         ;; just fill-polygon done, then under win32 an inverted image
         ;; with adjacent filled triangles winds up with a black line
         ;; across what should be a solid white area for the two triangles
         ;; together.
         (graphics-draw-line gd
                             (exact->inexact (xt x0 y0))
                             (exact->inexact (yt x0 y0))
                             (exact->inexact (xt x1 y1))
                             (exact->inexact (yt x1 y1)))
         (graphics-draw-line gd
                             (exact->inexact (xt x2 y2))
                             (exact->inexact (yt x2 y2))
                             (exact->inexact (xt x1 y1))
                             (exact->inexact (yt x1 y1)))
         (graphics-draw-line gd
                             (exact->inexact (xt x0 y0))
                             (exact->inexact (yt x0 y0))
                             (exact->inexact (xt x2 y2))
                             (exact->inexact (yt x2 y2)))
         (graphics-operation gd
                             'fill-polygon
                             (vector
                              (exact->inexact (xt x0 y0))
                              (exact->inexact (yt x0 y0))
                              (exact->inexact (xt x1 y1))
                              (exact->inexact (yt x1 y1))
                              (exact->inexact (xt x2 y2))
                              (exact->inexact (yt x2 y2)))))
       #f))))

(define default-image-size 100)

(define close-image
  (lambda (image)
    (if (fungraph-image? image)
        (let ((gd (fungraph-image-gd image)))
          (if gd
              (begin
                (graphics-close gd)
                (set-fungraph-image-gd! image #f)
                'closed)
              'not-open))
        (error "Illegal argument to close-image" image))))

(define overlay
  (lambda (first . rest)
    (let ((images (cons first rest)))
      (for-each (lambda (image)
                  (if (not (fungraph-image? image))
                      (error "Argument to overlay not an image" image)))
                images)
      (for-each (lambda (image)
                  (if (or (not (= (fungraph-image-width image)
                                  (fungraph-image-width first)))
                          (not (= (fungraph-image-height image)
                                  (fungraph-image-height first))))
                      (error "Can't overlay images of different sizes"
                             images)))
                rest)
      (make-fungraph-image
       (fungraph-image-width first) (fungraph-image-height first)
       (lambda (gd xt yt color other-color)
         (for-each (lambda (image)
                     ((fungraph-image-proc image) gd xt yt color other-color))
                   images))
       #f))))

(define invert
  (lambda (image)
    (if (not (fungraph-image? image))
        (error "Argument to invert not an image" image))
    (make-fungraph-image
     (fungraph-image-width image) (fungraph-image-height image)
     (lambda (gd xt yt color other-color)
       (graphics-operation gd 'set-foreground-color color)
       (graphics-operation gd 'fill-polygon
                           (vector (xt -1 -1)
                                   (yt -1 -1)
                                   (xt -1 1)
                                   (yt -1 1)
                                   (xt 1 1)
                                   (yt 1 1)
                                   (xt 1 -1)
                                   (yt 1 -1)))
       ((fungraph-image-proc image) gd xt yt other-color color))
     #f)))

(define (quarter-turn-right image)
  (if (not (fungraph-image? image))
      (error "argument to quarter-turn-right not an image" image))
  (make-fungraph-image
   (fungraph-image-height image)
   (fungraph-image-width image)
   (lambda (gd xt yt color other-color)
     ((fungraph-image-proc image) gd
                                  (lambda (x y) (xt y (- x)))
                                  (lambda (x y) (yt y (- x)))
                                  color other-color))
   #f))

(define (mirror-image image)
  (if (not (fungraph-image? image))
      (error "argument to mirror-image not an image" image))
  (make-fungraph-image
   (fungraph-image-width image)
   (fungraph-image-height image)
   (lambda (gd xt yt color other-color)
     ((fungraph-image-proc image) gd
                                  (lambda (x y) (xt (- x) y))
                                  (lambda (x y) (yt (- x) y))
                                  color other-color))
   #f))

(define (resize-image image . wh)
  (if (not (fungraph-image? image))
      (error "argument to resize-image not an image" image))
  (let ((width default-image-size)
        (height default-image-size))
    (if (not (null? wh))
        (begin (set! width (car wh))
               (if (not (null? (cdr wh)))
                   (begin (set! height (cadr wh))
                          (if (not (null? (cddr wh)))
                              (error "too many argument to resize-image")))
                   (set! height width))))
    (if (not (and (integer? height)
                  (integer? width)
                  (exact? height)
                  (exact? width)
                  (> height 0)
                  (> width 0)))
        (error "illegal size specification in resize-image" wh))
    (make-fungraph-image width height
                         (fungraph-image-proc image)
                         #f)))

(define (stack top . rest)
  (define (stack2 top bottom)
    (if (not (fungraph-image? top))
        (error "argument to stack not an image" top))
    (if (not (fungraph-image? bottom))
        (error "argument to stack not an image" bottom))
    (if (not (= (fungraph-image-width top) (fungraph-image-width bottom)))
        (error "Attempt to stack images of different widths" top bottom)
        (make-fungraph-image
         (fungraph-image-width top)
         (+ (fungraph-image-height top)
            (fungraph-image-height bottom))
         (let ((th (fungraph-image-height top))
               (bh (fungraph-image-height bottom)))
           (let* ((h (+ th bh))
                  (inexact-h (exact->inexact h)))
             (let ((tscale (/ th inexact-h))
                   (bscale (/ bh inexact-h)))
               (lambda (gd xt yt color other-color)
                 ((fungraph-image-proc top)
                  gd
                  (lambda (x y)
                    (xt x (+ (* tscale y) bscale)))
                  (lambda (x y)
                    (yt x (+ (* tscale y) bscale)))
                  color other-color)
                 ((fungraph-image-proc bottom)
                  gd
                  (lambda (x y)
                    (xt x (- (* bscale y) tscale)))
                  (lambda (x y)
                    (yt x (- (* bscale y) tscale)))
                  color other-color)))))
         #f)))
  (let loop ((image top)
             (images rest))
    (if (null? images)
        image
        (loop (stack2 image (car images)) (cdr images)))))