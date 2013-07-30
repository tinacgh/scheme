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