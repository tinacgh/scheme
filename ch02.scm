(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (< (/ n d) 0)
        (cons (- (/ (abs n) g)) (/ (abs d) g))
        (cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (myfun2 x) (* 2 x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (cond ((> (abs (denom x)) 1)
         (display "/") (display (denom x))))
  (/ (numer x) (denom x)))

;;; ex 2.2
(define (make-point x y)
  (cons x y))
(define (x-point pt)
  (car pt))
(define (y-point pt)
  (cdr pt))
(define (print-point p)
  (newline)
  (display "(") (display (x-point p)) (display ", ")
  (display (y-point p)) (display ")"))

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (midpoint-segment segment)
  (let ((mid-x (average (x-point (start-segment segment))
                        (x-point (end-segment segment))))
        (mid-y (average (y-point (start-segment segment))
                        (y-point (end-segment segment)))))
    (make-point mid-x mid-y)))

(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- My-cons" m))))
  dispatch)

(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

;;; ex 2.4
(define (lambda-cons x y)
  (lambda (m) (m x y)))

(define (lambda-car z)
  (z (lambda (p q) p)))

(define (lambda-cdr z)
  (z (lambda (p q) q)))

;;; 2.18
(define (my-reverse lst)
  (define (my-rev-iter lst result)
    (if (null? lst)
	result
	(my-rev-iter (cdr lst) (cons (car lst) result))))
  (my-rev-iter lst '()))

;;; 2.21
(define (square-list items)
  (map square items))

(define (square-list-2 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-2 (cdr items)))))

;;; 2.2.2
(define (scale-tree tree factor)
  (cond ((null? tree) '())
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree-map sub-tree factor)
	     (* sub-tree factor)))
       tree))

(define (my-filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;; 2.33
(define (my-length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 7)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (my-remove x s))))
               s)))

(define (my-remove item sequence)
  (filter (lambda (x) (not (equal? x item)))
          sequence))

;;; 2.3.1
(define (my-memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expr type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;; 2.3.3

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

;;; 2.4.1
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;;; Alyssa
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

;;; message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;;; put and get stackoverflow #5499005
(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
	  ((equal? (key (car array)) k) array)
	  (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
	  ((equal? (key (car array)) k) (value (car array)))
	  (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))
;;; 2.5
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum - Type-tag")))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum - Contents")))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(apply-generic op a1 a2))
	      (error "No method for these types"
		     (list op type-tags)))))))