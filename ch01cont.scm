(define (my-sqrt x)
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (cube-root x)
  (define (cube x) (* x x x))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.001))
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (define (cube-iter guess)
    (if (good-enough? guess)
        guess
        (cube-iter (improve guess))))
  (cube-iter 1.0))

;;; 1.2.4

(define (expt-linear-rec b n)
  (if (= n 0)
      1
      (* b (expt-linear-rec b (- n 1)))))

(define (expt-linear-iter b n)
  (expt-iter b n 1))

(define (expt-iter base n product)
  (if (= n 0)
      product
      (expt-iter base (- n 1) (* product base))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;;; 1.2.5

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (copy x n)
  (if (= n 0)
      ()
      (cons x (copy x (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;; 1.2.6
;;; ex. 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (fast-mult a b)
  (define (double n) (+ n n))
  (define (halve n) (/ n 2))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

;;; 1.3.1
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (recipr-sum a b)
  (define (recipr-term x)
    (/ 1.0 x))
  (define (recipr-next x)
    (+ x 1))
  (sum recipr-term a recipr-next b))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (pi-over-4-term i)
  (/ (+ (* 2 i) 2) (+ (* 2 i) 1.0)))

;;; ex 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (prod-accum term a next b)
  (accumulate * 1 term a next b))

;;; ex 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b) dx))

(define (nderiv f a dx)
  (/ (- (f (+ a dx)) (f a)) dx))

;;; 1.3.2
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define (some-func x)
  (let ((a 1)
        (b (+ 1 1)))
    (* x a b)))

;;; 1.3.3
(define (average a b)
  (/ (+ a b) 2.0))
(define (close-enough? x y)
  (< (abs (- x y)) 0.00001))
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
           (error "Values not of opposite sign" a b)))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fixed-pt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;; 1.3.4
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newtons x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;; ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

;;; ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))