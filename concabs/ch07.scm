(define integers-from-to
  (lambda (low high)
    (if (> low high)
        '()
        (cons low (integers-from-to (+ 1 low) high)))))

(define iter-ints-from-to
  (lambda (low high)
    (define iter
      (lambda (high lst)
        (if (< high low)
            lst
            (iter (- high 1)
                  (cons high lst)))))
    (iter high '())))

;;; ex 7.13
(define eights
  (lambda (n)
    (if (= n 0)
        '()
        (cons 8 (eights (- n 1))))))

(define list-of-lists
  (lambda (inputs)
    (if (null? inputs)
        '()
        (cons (integers-from-to 1 (car inputs)) (list-of-lists (cdr inputs))))))

(define my-map
  (lambda (fn lst)
    (if (null? lst)
        '()
        (cons (fn (car lst)) (my-map fn (cdr lst))))))

(define add-to-end
  (lambda (lst elt)
    (if (null? lst)
        (cons elt '())
        (cons (car lst)
              (add-to-end (cdr lst) elt)))))

(define iter-rev
  (lambda (lst)
    (define iter
      (lambda (lst result)
        (if (null? lst)
            result
            (iter (cdr lst) (cons (car lst) result)))))
    (iter lst '())))

(define merge-sort
  (lambda (lst)
    (cond ((null? lst)
           '())
          ((null? (cdr lst))  ; single element
           lst)
          (else
           (merge (merge-sort (one-part lst))
                  (merge-sort (the-other-part lst)))))))

(define merge
  (lambda (lst1 lst2)
    (cond ((null? lst1) lst2)
          ((null? lst2) lst1)
          ((< (car lst1) (car lst2))
           (cons (car lst1) (merge (cdr lst1) lst2)))
          (else
           (cons (car lst2) (merge lst1 (cdr lst2)))))))

(define odd-part
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst) (even-part (cdr lst))))))
(define even-part
  (lambda (lst)
    (if (null? lst)
        '()
        (odd-part (cdr lst)))))
(define one-part odd-part)
(define the-other-part even-part)