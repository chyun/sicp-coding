(define (map proc items)
   (if (null? items)
       '()
       (cons (proc (car items))
             (map proc (cdr items)))))

(define (filter predicate sequence)
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
(accumulate + 0 (list 1 2 3 4))

(define (enumerate-interval low high)
			   (if (> low high)
				   '()
				   (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond
    ((null? tree) '())
    ((not (pair? tree))
        (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4) 5)))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square (filter odd?
                                  (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              '()
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))

(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
    0
    coefficient-sequence
  ))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
    (cond
      ((null? x) 0)
      ((pair? x) (count-leaves x))
      (else 1))
  ) t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil
    (cons (accumulate op init (map (lambda (x) (car x)) seqs))
      (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))
  ))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-pairs n)
	(flatmap 
		(lambda (i)
			(map 
				(lambda (j) (list i j))
				(enumerate-interval 1 (- i 1))))
		 (enumerate-interval 1 n)))

(define (trible-sum n s)
    (filter (lambda (x) (= (+ (car x) (cadr x) (caddr x)) s))
        (accumulate append '() (unique-trible-pair n))))

(define (unique-trible-pair n)
			    (filter (lambda (x)  (pair? x))
          				(unique-trible n)))

(define (unique-trible n)
	(flatmap 
		(lambda (i)
			(map 
				(lambda (j) 
				    (map 
						(lambda (k) (list k j i))
						(enumerate-interval 1 (- j 1))))
				(enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))