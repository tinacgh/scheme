;;; ex 4.9
(define triangle
  (lambda (x1 y1 x2 y2 x3 y3)
    (overlay
     (line x1 y1 x2 y2)
     (line x2 y2 x3 y3)
     (line x3 y3 x1 y1))))

(define (avg a b)
  (/ (+ a b) 2))

(define sierp
  (lambda (x1 y1 x2 y2 x3 y3 level)
    (if (= level 0)
        (triangle x1 y1 x2 y2 x3 y3)
        (let ((ax (avg x1 x2))
              (ay (avg y1 y2))
              (bx (avg x2 x3))
              (by (avg y2 y3))
              (cx (avg x3 x1))
              (cy (avg y3 y1)))
          (overlay 
           (sierp x1 y1 ax ay cx cy (- level 1))
           (sierp ax ay x2 y2 bx by (- level 1))
           (sierp cx cy bx by x3 y3 (- level 1)))))))