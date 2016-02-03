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

(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)  ; 下方所有皇后检查完毕, 新皇后安全
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
            (if (or (= row-of-new-queen row-of-current-queen)           ; 行碰撞
                    (= row-of-new-queen (+ i row-of-current-queen))     ; 右下方碰撞
                    (= row-of-new-queen (- row-of-current-queen i)))    ; 左下方碰撞
                #f
                (iter-check row-of-new-queen 
                            (cdr rest-of-queens)    ; 继续检查剩余的皇后
                            (+ i 1))))))            ; 更新步进值

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (make-vect xcor ycor)
    (list xcor ycor))

(define (xcor-vect v)
    (car v))

(define (ycor-vect v)
    (cadr v))

(define (add-vect vect another-vect)
  (cond ((null? vect) another-vect)
        ((null? another-vect) vect)
        ((make-vect (+ (xcor-vect vect)
                        (xcor-vect another-vect))
                     (+ (ycor-vect vect)
                        (ycor-vect another-vect))))))

(define (sub-vect vect another-vect)
  (cond ((null? vect) '())
        ((null? another-vect) '())
        ((make-vect (- (xcor-vect vect)
                       (xcor-vect another-vect))
                    (- (ycor-vect vect)
                       (ycor-vect another-vect))))))

(define (scale-vect factor vect)
    (make-vect (* factor (xcor-vect vect))
               (* factor (ycor-vect vect))))

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame f)
    (car f))

(define (edge1-frame f)
    (cadr f))

(define (edge2-frame f)
    (caddr f))

(define (make-frame origin edge1 edge2)
    (cons origin
          (cons edge1 edge2)))

(define (origin-frame f)
    (car f))

(define (edge1-frame f)
    (cadr f))

(define (edge2-frame f)
    (cddr f))

(define (frame-coord-map frame)
    (lambda (v)
        (add-vect
            (origin-frame frame)
            (add-vect 
                (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each
            (lambda (segment)
                (draw-line
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))))
        segment-list)))

(define (make-segment start end)
    (list start end))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cadr s))

(define top-left (make-vect 0.0 1.0))

(define top-right (make-vect 1.0 1.0))

(define bottom-left (make-vect 0.0 0.0))

(define bottom-right (make-vect 1.0 0.0))

(define top (make-segment top-left top-right))

(define left (make-segment top-left bottom-left))

(define right (make-segment top-right bottom-right))

(define bottom (make-segment bottom-left bottom-right))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(define (below painter1 painter2)
    (lambda (frame)
        ((flip-horiz
            (rotate90
                (beside
                    (rotate270
                        (flip-horiz painter1))
                    (rotate270
                        (flip-horiz painter2)))))
         frame)))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))

(define (equal? x y)
    (cond ((and (symbol? x) (symbol? y))
            (symbol-equal? x y))
          ((and (list? x) (list? y))
            (list-equal? x y))
          (else
            (error "Wrong type input x and y -- EQUAL?" x y))))

(define (symbol-equal? x y)
    (eq? x y))

(define (list-equal? x y)
    (cond ((and (null? x) (null? y))    ; 空表
            #t)
          ((or (null? x) (null? y))     ; 长度不同的表
            #f)
          ((equal? (car x) (car y))     ; 对比 car 部分
            (equal? (cdr x) (cdr y)))   ; 递归对比 cdr 部分
          (else
            #f)))

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

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
         (error "unknown expression type - DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponention base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else (list '** base exponent))))

(define (exponention? x)
    (and (pair? x) (eq? (car x) '**)))

(define (base exponention)
    (cadr exponention))

(define (exponent exponention)
    (caddr exponention))

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
        ((exponention? exp)
            (make-product (make-product (exponent exp) (make-exponention (base exp) (- (exponent exp) 1))) (deriv (base exp) var)))
        (else
         (error "unknown expression type - DERIV" exp))))

(define (make-sum . s)
        (cons '+ s ))

(define (addend s) (car (cdr s)))

(define (augend s) 
    (cond ((> (length s) 3) (cons '+ (cdr (cdr s))))
          (else (caddr s))))

(define (make-product . s)
        (cons '* s ))

(define (multiplier s) (car (cdr s)))

(define (multiplicand s)
    (cond ((> (length s) 3) (cons '* (cdr (cdr s))))
          (else (caddr s))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
    ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set? x set)
    (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

