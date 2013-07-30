(define (range n)
  (define (range-iter n acc)
    (if (= n 0)
	acc
	(range-iter (- n 1) (cons (- n 1) acc))))
  (range-iter n '()))

(define (flatten tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (flatten (car tree))
		      (flatten (cdr tree))))))