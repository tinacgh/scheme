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