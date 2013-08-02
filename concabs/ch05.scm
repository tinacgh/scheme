(define together-copies-of
  (lambda (combine quantity thing)
    (if (= quantity 1)
        thing
        (combine (together-copies-of combine (- quantity 1) thing)
                 thing))))

(define stack-copies-of
  (lambda (quantity image)
    (together-copies-of stack quantity image)))

(define mystery
  (lambda (a b)
    (together-copies-of + a b)))

;;; 5.3
(define make-multiplier
  (lambda (scaling-factor)
    (lambda (x)
      (* scaling-factor x))))

(define make-exp
  (lambda (e)
    (lambda (n)
      (expt n e))))

(define (make-repeated-version-of f)
  (define (the-repeated-version b n)
    (if (= n 0)
        b
        (the-repeated-version (f b) (- n 1))))
  the-repeated-version)