(define (interleave a b)
  (if (null? a) b
      (if (null? b) a
	  (cons (car a) (cons (car b) (interleave (cdr a) (cdr b)))))))

(define (rev-range n)
  (if (= n 0)
      '()
      (cons (- n 1) (rev-range (- n 1)))))

(define (range n)
  (reverse (rev-range n)))

(define (last lst)
  (car (reverse lst)))