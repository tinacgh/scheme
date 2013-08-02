;;; from ch 2

(define get-exp-of-2
  (lambda (n)
    (if (odd? n)
        0
        (+ 1 (get-exp-of-2 (/ n 2))))))

(define get-exp-of-2-iter
  (lambda (n result)
    (if (odd? n)
        result
        (get-exp-of-2-iter (/ n 2) (+ 1 result)))))
(define get-exp-of-2-wrapper
  (lambda (n)
    (get-exp-of-2-iter n 0)))

(define factorial-product
  (lambda (a b)
    (if (= b 0)
        a
        (factorial-product (* a b) (- b 1)))))

(define fact-iter
  (lambda (n)
    (factorial-product 1 n)))

(define power-product
  (lambda (a b e)
    (if (= e 0)
        a
        (power-product (* a b) b (- e 1)))))

(define power-iter
  (lambda (b e)
    (power-product 1 b e)))

(define improve
  (lambda (phi-n)
    (+ 1 (/ 1 phi-n))))

(define approximate-golden-ratio
  (lambda (tolerance)
    (define find-approx-from
      (lambda (starting-point)
        (if (good-enough? starting-point)
            starting-point
            (find-approx-from (improve starting-point)))))
    (define good-enough?
      (lambda (approx)
        (< (/ 1 (square (denominator approx)))
           tolerance)))
    (find-approx-from 1)))