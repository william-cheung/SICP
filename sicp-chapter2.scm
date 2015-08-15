Ex 2.1
---------------------
"
(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b)))
)
"

(define (add-rat x y)
 	(make-rat (+ (* (numer x) (denom y)) 
 				 (* (numer y) (denom x)))
 			  (* (denom x) (denom y)))
)

(define (sub-rat x y)
 	(make-rat (- (* (numer x) (denom y))
 	             (* (numer y) (denom x)))
	          (* (denom x) (denom y)))
)

(define (mul-rat x y)
	(make-rat (* (numer x) (numer y))
	          (* (denom x) (denom y)))
)

(define (div-rat x y)
	(make-rat (* (numer x) (denom y))
	          (* (denom x) (numer y)))
)

(define (equal-rat? x y)
	(= (* (numer x) (denom y))
	   (* (numer y) (denom x)))
)

(define (print-rat x)
	(new-line)
 	(display (numer x))
 	(display "/")
 	(display (denom x))
 )

(define (make-rat n d) 
	(define (make-rat-aux n d)
		(cond ((< d 0) (cons (- n) (- d)))
	    	  ((> d 0) (cons n d))
	    	  (else (error "zero denominator"))
	    )
	)
	(let ((g (gcd n d)))
	     (make-rat-aux (/ n g) (/ d g))
	)
)

(define (numer x) (car x))
(define (denom x) (cdr x))

(add-rat (make-rat 1 2) (make-rat 1 3))
(mul-rat (make-rat 2 1) (make-rat 1 4))
(add-rat (make-rat 1 -2) (make-rat 1 3))
;; (mul-rat (make-rat 0 1) (make-rat 1 0))
(mul-rat (make-rat 0 1000) (make-rat 1 10))

Ex 2.2
---------------------

(define (make-segment s e) (cons s e))

(define (start-point seg) (car seg))

(define (end-point seg) (cdr seg))

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
	(newline)
	(display "(")
	(display (x-point p))
	(display ", ")
	(display (y-point p))
	(display ")")
)

(define (midpoint seg)
	(let ((p1 (start-point seg))
	      (p2 (end-point seg)))
		(define x1 (x-point p1))
		(define y1 (y-point p1))
		(define x2 (x-point p2))
		(define y2 (y-point p2))
		(make-point (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))
	)
)

(print-point (midpoint (make-segment (make-point 1 3) (make-point 2 4))))

Ex 2.4
----------------------

(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))

;; (car (cons x y)) == ((cons x y) (lambda (p q) p)) == ((lambda (p q) p) x y) == x

(define (cdr z) (z (lambda (p q) q)))


Ex 2.5
----------------------

(define (pow a n)
	(cond ((= n 0) 1)
	      ((even? n) (square (pow a (/ n 2))))
	      (else (* a (pow a (- n 1))))
	)
)

(define (exp a x)
	(define (iter x result)
		(if (not (= (remainder x a) 0))
			result
			(iter (/ x a) (+ result 1))
		)
	)
	(iter x 0)
)

(define (cons x y) (* (pow 2 x) (pow 3 y)))
(define (car z) (exp 2 z))
(define (cdr z) (exp 3 z))

(car (cons 3 4))
(cdr (cons 5 6))


Ex 2.6 
----------------------

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
 	(lambda (f) (lambda (x) (f ((n f) x))))
)


;; ((zero f) x) == x
;; (add-1 zero) == (lambda (f) (lambda (x) (f ((zero f) x))) == (lambda (f) (lambda (x) (f x)))
;; (((add-1 zero) f) x) == (f x)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;;(((add x y) f) z) == ffff...f(z)

(define (add x y) (lambda (f) (lambda (z) ((x f) ((y f) z)))))

(define (inc x) (+ x 1))

((zero inc) 0)
((one inc) 0)
((two inc) 0)
(((add one two) inc) 0)


Ex 2.7 - 2.10
----------------------

(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
		           (+ (upper-bound x) (upper-bound y)))
)

(define (mul-interval x y)
	(let ((p1 (* (lower-bound x) (lower-bound y)))
	      (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
		(make-interval (min p1 p2 p3 p4)
		               (max p1 p2 p3 p4))
	)
)

(define (div-interval x y)
	(mul-interval x 
	              (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))
	)
)

;; Ex 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound intv) (min (car intv) (cdr intv)))
(define (lower-bound intv) (max (car intv) (cdr intv)))

;; Ex 2.8
(define (sub-interval x y)
	(add-interval x
	              (make-interval (- (upper-bound y))
	                             (- (lower-bound y)))
	)
)

;; Ex 2.10
(define (div-interval x y)
	(define ub (upper-bound) y)
	(define lb (lower-bound) y)
	(if (>= (* ub lb) 0)
		(mul-interval x (make-interval (/ 1.0 ub) (/ 1.0 lb)))
		(error "divide by an interval that spans zero")
	)
)


Ex 2.17
---------------------

(define (last-pair lst)
	(if (null? (cdr lst)) lst (last-pair (cdr lst)))
)

(last-pair (list 23 72 149 34))


Ex 2.18
---------------------


"
(define (append list1 list2)
 	(if (null? list1)
 		list2
 		(cons (car list1) (append (cdr list1) list2))
 	)
)
"

"
(define (reverse lst) 
	(if (null? lst) 
		lst
		(append (reverse (cdr lst)) (list (car lst)))
	)
)
"

(reverse (list 1 4 9 16 25))


Ex 2.20
----------------------

"
(define (same-parity x . lst)
	(define (same-parity-aux lst)
		(if (null? lst) 
			lst
			(if (= (remainder (- x (car lst)) 2) 0)
				(cons (car lst) (same-parity-aux (cdr lst)))
				(same-parity-aux (cdr lst))
			)
		)
	)
	(cons x (same-parity-aux lst))
)
"

(define (same-parity x . lst)
	(define (same-parity-iter lst result)
		(if (null? lst) 
			result
			(if (= (remainder (- x (car lst)) 2) 0)
				(same-parity-iter (cdr lst) (append result (list (car lst))))
				(same-parity-iter (cdr lst) result)
			)
		)
	)
	(same-parity-iter lst (list x))
)

(same-parity 1)
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


Ex 2.21
---------------------

(define (square-list items)
 	(if (null? items)
 		items
 		(cons (square (car items)) (square-list (cdr items)))
 	)
)


"
(define (square-list items)
 	(map (lambda (x) (* x x)) items)
)
"

(square-list (list 1 2 3 4))	;;(1 4 9 16)


Ex 2.22
---------------------

(define (square-list items)
	(define (iter things answer)
 		(if (null? things)
			answer
 			(iter (cdr things) (cons (square (car things)) answer))
 		)
 	)
 	(iter items '())
)

(square-list (list 1 2 3 4)) ;; (16 9 4 1)

Ex 2.23
----------------------

"
(define (for-each proc items)
	(cond ((not (null? items)) (proc (car items)) (for-each proc (cdr items)))
	)
)
"

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

Ex 2.25
----------------------

(car (cdr (car (cdr (cdr    (list 1 3 (list 5 7) 9)    )))))
(car (car  (list (list 7))  ))
(define lst (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;;          cdr		cdr		cdr		cdr		cdr		cdr		car	
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst))))))))))))


Ex 2.26
----------------------
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) 	;; (1 2 3 4 5 6)
(cons x y)  	;; ((1 2 3) 4 5 6)
(list x y)		;; ((1 2 3) (4 5 6))


Ex 2.27
----------------------
(define x (list (list 1 2) (list 3 4)))

(reverse x)

(define (deep-reverse lst) 
	(if (null? lst) 
		lst
		(let ((first-elem (car lst)))
			(append (deep-reverse (cdr lst)) 
			        (list (if (pair? first-elem) (reverse first-elem) first-elem)))
		)
	)
)

(deep-reverse x)

Ex 2.28
-----------------------

(define (fringe tree)
	(if (null? tree)
		tree
		(cond ((not (pair? tree)) (list tree))
		      (else (append (fringe (car tree)) (fringe(cdr tree))))
		)
	)
)


(define x (list (list 1 2) (list 3 4)))
(fringe x)				;; (1 2 3 4)
(fringe (list x x))		;; (1 2 3 4 1 2 3 4)


Ex 2.29
-----------------------

(define (make-mobile left right)
	(list left right)
)

(define (left-branch mb)
	(car mb)
)

(define (right-branch mb)
	(car (cdr mb))
)

(define (make-branch length structure)
	(list length structure)
)

(define (branch-length brch)
	(car brch)
)

(define (branch-structure brch)
	(car (cdr brch))
)

(define (weight struct)
	(if (not (pair? struct)) struct (total-weight struct))
)

(define (total-weight mb)
	(let ((ls (branch-structure (left-branch mb)))
		  (rs (branch-structure (right-branch mb))))
		 (+ (weight ls) (weight rs))
	)
)

(define (balanced? mb)
	(define (balanced-s? struct)
		(if (not (pair? struct)) #t (balanced? struct))
	)
	(let ((llen (branch-length (left-branch mb)))
	      (rlen (branch-length (right-branch mb)))
	      (ls (branch-structure (left-branch mb)))
          (rs (branch-structure (right-branch mb))))
		 (if (not (= (* llen (weight ls)) (* rlen (weight rs))))
		 	#f
		 	(cond ((not (balanced-s? ls)) #f)
		 	      ((not (balanced-s? rs)) #f)
		 	      (else #t) 
		 	) 
		 )
	)
)

;; test

(define level-1-mobile (make-mobile (make-branch 2 1) 
                                    (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                    (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                    (make-branch 8 2))) 
  
(total-weight level-1-mobile) 	;; 3
(total-weight level-2-mobile) 	;; 4
(total-weight level-3-mobile)	;; 6

(balanced? (make-mobile (make-branch 2 3) 
                        (make-branch 3 2)))		;; #t 
  
(balanced? level-1-mobile) 						;; #t
(balanced? level-2-mobile) 						;; #t	
(balanced? level-3-mobile) 						;; #t
  
(balanced? (make-mobile (make-branch 10 1000) 
                        (make-branch 1 level-3-mobile))) 	;; #f 
                        
                        
                        
Ex 2.30
----------------------

(define (square-tree tree)
	(cond ((null? tree) tree)
	      ((not (pair? tree)) (square tree))
	      (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
	) 
)

(square-tree
 	(list 1
		(list 2 (list 3 4) 5)
	 	(list 6 7)))
;; (1 (4 (9 16) 25) (36 49))

Ex 2.32
----------------------

(define (subsets s)
	(if (null? s) 
		(list s)
 		(let ((rest (subsets (cdr s))))
 			(append rest (map (lambda (x) (cons (car s) x)) rest))
 		)
 	)
)
 
(subsets (list 1 2 3))


Ex 2.33
----------------------


(define (accumulate op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (accumulate op initial (cdr sequence)))
 	)
)


(define (map p sequence)
 	(accumulate (lambda (x y) (cons (p x) y)) '() sequence)
)

(define (append seq1 seq2)
	(accumulate cons seq2 seq1)
)

"
(define (length sequence)
 	(accumulate (lambda (x y) (+ 1 y)) 0 sequence)
)
"

(map square (list 1 2 3 4))
(append (list 1 2) (list 3 4))
(length (list 1 2 3 4))


Ex 2.34
----------------------

(define (accumulate op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (accumulate op initial (cdr sequence)))
 	)
)

(define (horner-eval x coefficient-sequence)
 	(accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
 	            0
                coefficient-sequence)
)

(horner-eval 2 (list 1 3 0 5 0 1))	;; 79

Ex 2.35
-----------------------

(define (accumulate op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (accumulate op initial (cdr sequence)))
 	)
)

(define (count-leaves t)
	(accumulate + 0 
		(map 
			(lambda (t) 
				(cond ((null? t) 0)
				      ((not (pair? t)) 1)
				      (else (count-leaves t))
				)) 
			t))
)

(define t (list (list 1 2) (list 3 4)))

(count-leaves t)


Ex 2.36
---------------------

(define (accumulate op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (accumulate op initial (cdr sequence)))
 	)
)

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
 		'()
	 	(cons (accumulate op init (map car seqs)) 
	 	      (accumulate-n op init (map cdr seqs)))
	)
)

(define seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 seqs) ;; (22 26 30)


Ex 2.37
----------------------

;; (map * (list 1 2) (list 3 4))

(define (accumulate op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (accumulate op initial (cdr sequence)))
 	)
)

(define (accumulate-n op init seqs)
	(if (null? (car seqs))
 		'()
	 	(cons (accumulate op init (map car seqs)) 
	 	      (accumulate-n op init (map cdr seqs)))
	)
)

(define (dot-product v w)
 	(accumulate + 0 (map * v w))
)
 	
(define (matrix-*-vector m v)
	(map (lambda (w) (dot-product v w)) m)
)

(define (transpose mat)
 	(accumulate-n cons '() mat)
)

(define (matrix-*-matrix m n)
 	(let ((cols (transpose n)))
 		(map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)
 	)
)

;; test
(define v1 (list 1 2 3))
(define v2 (list 4 5 6))
(define m1 (list (list 1 2 3) (list 4 5 6)))
(define m2 (list (list 1 2) (list 3 4) (list 5 6)))

(dot-product v1 v2)
(matrix-*-vector m1 v1)
(transpose m2)
(matrix-*-matrix m1 m2)


Ex 2.38
----------------------

(define (fold-left op initial sequence)
 	(define (iter result rest)
 		(if (null? rest)
 			result
 			(iter (op result (car rest)) (cdr rest)))
 	)
	(iter initial sequence)
)

(define (fold-right op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (fold-right op initial (cdr sequence)))
 	)
)

(define nil '())

(fold-right / 1 (list 1 2 3))			;; 1 / (2 / 3) == 3 / 2
(fold-left / 1 (list 1 2 3))			;; (1 / 2) / 3 == 1 / 6
(fold-right list nil (list 1 2 3))		;; (list 1 (list 2 (list 3 (list nil)))) == (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))		;; (list (list (list (list nil) 1) 2) 3) == (((() 1) 2) 3)

;; left: 	(((1 op 2) op 3) op 4)
;; right:	(1 op (2 op (3 op 4)))


Ex 2.39
---------------------

(define nil '())

(define (fold-left op initial sequence)
 	(define (iter result rest)
 		(if (null? rest)
 			result
 			(iter (op result (car rest)) (cdr rest)))
 	)
	(iter initial sequence)
)

(define (fold-right op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (fold-right op initial (cdr sequence)))
 	)
)

(define (reverse1 sequence)
	(fold-right (lambda (x y) (append y (list x))) nil sequence)
)
(define (reverse2 sequence)
 	(fold-left (lambda (x y) (cons y x)) nil sequence)
)


(reverse1 (list 1 2 3 4))
(reverse2 (list 1 2 3 4))


Ex 2.40 - 2.41
----------------------

"============================================================================="

(define (smallest-divisor n)
 	(find-divisor n 2)
)

(define (find-divisor n test-divisor)
 	(cond ((> (square test-divisor) n) n)
 		  ((divides? test-divisor n) test-divisor)
 		  (else (find-divisor n (+ test-divisor 1)))
 	)
)

(define (divides? a b)
 	(= (remainder b a) 0)
)

(define (prime? n)
	(= (smallest-divisor n) n)
)

"============================================================================="

(define nil '())

(define (accumulate op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (accumulate op initial (cdr sequence)))
 	)
)

(define (enumerate-interval low high)
	(if (> low high)
 		nil
 		(cons low (enumerate-interval (+ low 1) high))
	)
)

(define (filter predicate sequence)
	(cond ((null? sequence) nil)
		  ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
 	      (else (filter predicate (cdr sequence)))
	)
)

(define (flatmap proc seq)
	(accumulate append nil (map proc seq))
)

(define (prime-sum? pair)
 	(prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
 	(list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pairs n)
	(map make-pair-sum (filter prime-sum? (unique-pairs n)))
)

(define (unique-pairs n)
	(flatmap (lambda (i)
 	         	(map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
 	         (enumerate-interval 1 n))
)

;; (prime-sum-pairs 5)

;; Ex 2.41
(define (unique-triples n)
	(flatmap (lambda (i) 
	         	(map (lambda (pair) (cons i pair)) (unique-pairs (- i 1)))) 
	         (enumerate-interval 1 n))
)

(define (find-triples n s)
	(define (aux-pred? triple)
		(= (+ (car triple) (cadr triple) (caddr triple)) s)
	)
	(filter aux-pred? (unique-triples n))
)

;;(find-triples 10 20)


Ex 2.42
-----------------------

(define nil '())

(define (enumerate-interval low high)
	(if (> low high)
 		nil
 		(cons low (enumerate-interval (+ low 1) high))
	)
)

(define (accumulate op initial sequence)
	(if (null? sequence)
 		initial
 		(op (car sequence) (accumulate op initial (cdr sequence)))
 	)
)

(define (flatmap proc seq)
	(accumulate append nil (map proc seq))
)

(define empty-board nil)

(define (queens board-size)
	(define (queen-cols k) 
		(if (= k 0)
			(list empty-board)
			(filter	(lambda (positions) (safe? k positions))
			        (flatmap (lambda (rest-of-queens)
 			                 	(map (lambda (new-row)
 			        	     		(adjoin-position new-row k rest-of-queens))
                             		(enumerate-interval 1 board-size)))
           	                 (queen-cols (- k 1))))
		)
	)
	(queen-cols board-size)
)

(define (safe? k positions)
	(define (safe-aux? c1 r1 c2 r2)
		(if (or (= c1 c2) (= r1 r2) (= (- c1 r1) (- c2 r2)) (= (+ c1 r1) (+ c2 r2)))
			#f
			#t
		)
	)
	(define (kth-position k poss)
		(if (= k 1) 
			(car poss)
			(kth-position (- k 1) (cdr poss))
		)
	)
	(define pos-k (kth-position k positions))
	(define (test poss i)
		(cond ((= i k) #t)
		      ((not (safe-aux? i (car poss) k pos-k)) #f)
		      (else (test (cdr poss) (+ i 1)))
		)
	)
	(test positions 1)
)

;; unit test
(safe? 3 (list 3 7 2))	;; #t
(safe? 3 (list 1 2 3))	;; #f

(define (adjoin-position new-row k rest-of-queens)
	(append rest-of-queens (list new-row))
)

(queens 8) ;; (3 7 2 8 5 1 4 6)



Ex 2.43
---------------------- 
n * T



"
***************************************************************************

(define (memq item x)
 	(cond ((null? x) #f)
 	      ((eq? item (car x)) x)
 	      (else (memq item (cdr x)))
	)
)

(memq 'apple '(pear banana prune))				;; #f
(memq 'apple '(x (apple sauce) y apple pear))	;; (apple pear)

***************************************************************************
"

Ex 2.54
----------------------
"
(define (equal? a b)
	(cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
	      ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
	      (else #f)
	)
)
"

(eq? 1 1)							;; #t
(eq? 1 2)							;; #f
(eq? (list 1 2) (list 1 2))			;; #f

(eq? 'x 'x)							;; #t
(eq? 'x 'y)							;; #f
(eq? '(x y) '(x y))					;; #f

(equal? 1 2)						;; #f
(equal? 1 1)						;; #t
(equal? (list 1 2) (list 3 4))		;; #f
(equal? (list 1 2) (list 1 2))		;; #t

(equal? 'x 'y)						;; #f
(equal? 'x 'x)						;; #t
(equal? '(x y) '(x y))				;; #t
(equal? '(this is a list) '(this is a list))			;; #t
(equal? '(this is a list) '(this (is a) list))			;; #f
(equal? '(this is (is a) list) '(this (is a) list))		;; #f
(equal? '(this is (is b) list) '(this (is a) list))		;; #f


Ex 2.56 - 2.58
----------------------

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
;;	      ((exponentiation? exp)
;;	      	(make-product (make-product (exponent exp) 
;;	      	                            (make-exponentiation (base exp) 
;;	      	                                                 (make-sum (exponent exp) '-1)))
;;	      	              (deriv (base exp) var)))
 	      (else (error "unknown expression type -- DERIV" exp))
	)
)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (=number? exp num)
	(and (number? exp) (= exp num))
)

"
(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
	      ((=number? a2 0) a1)
	      ((and (number? a1) (number? a2)) (+ a1 a2))
	      (else (list '+ a1 a2))
	)
)

(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
	      ((=number? m1 1) m2)
	      ((=number? m2 1) m1)
	      ((and (number? m1) (number? m2)) (* m1 m2))
	      (else (list '* m1 m2))
	)
)

(define (sum? x)
	(and (pair? x) (eq? (car x) '+))
)

(define (addend s) (cadr s))
(define (augend s) 
	(if (null? (cdddr s))
		(caddr s)
		(cons '+ (cddr s))
	)
)

(define (product? x)
	(and (pair? x) (eq? (car x) '*))
)

(define (multiplier p) (cadr p))
(define (multiplicand p) 
	(if (null? (cdddr p))
		(caddr p)
		(cons '* (cddr p))
	)
)

(define (exponentiation? x)
	(and (pair? x) (eq? (car x) '**))
)

(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (make-exponentiation x n)
	(cond ((=number? n 0) '1)
	      ((=number? n 1) x)
	      (else (list '** x n))
	)
)

;; test

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(** (+ x -1) n) 'x)
(deriv '(* x y (+ x 3)) 'x)
"

(define (make-sum a1 a2)
	(cond ((=number? a1 0) a2)
	      ((=number? a2 0) a1)
	      ((and (number? a1) (number? a2)) (+ a1 a2))
	      (else (list a1 '+ a2))
	)
)

(define (make-product m1 m2)
	(cond ((or (=number? m1 0) (=number? m2 0)) 0)
	      ((=number? m1 1) m2)
	      ((=number? m2 1) m1)
	      ((and (number? m1) (number? m2)) (* m1 m2))
	      (else (list m1 '* m2))
	)
)
(define (sum? x)
	(if (pair? (memq '+ x))
		#t
		#f
	)
)

(define (addend s) 
	(define (iter ex result)
		(if (eq? (car ex) '+)
			(if (null? (cdr result)) (car result) result)
			(iter (cdr ex) (append result (list (car ex))))
		)
	)
	(iter s '())
)

(define (augend s) 
	(let ((tmp (cdr (memq '+ s))))
		(if (null? (cdr tmp))
			(car tmp)
			tmp
		)
	)
)

(define (product? x)
	(if (pair? (memq '* x))
		#t
		#f
	)
)

(define (multiplier p) 
	(define (iter ex result)
		(if (eq? (car ex) '*)
			(if (null? (cdr result)) (car result) result)
			(iter (cdr ex) (append result (list (car ex))))
		)
	)
	(iter p '())
)

(define (multiplicand p) 
	(let ((tmp (cdr (memq '* p))))
		(if (null? (cdr tmp))
			(car tmp)
			tmp
		)
	)
)

;; test

;; (memq '+ '(x + y))
(sum? '(x + 3 * y))
(addend '(x + 3 * y))
(augend '(x + 3 * y))

(deriv '(x + y) 'x)
(deriv '(x + 3 * x) 'x)
(deriv '(x + 3 * y) 'x)
(deriv '(x + x * y) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x * (x + y) + x * x * x) 'x)


Ex 2.59
-----------------------------

(define (element-of-set? x set)
	(cond ((null? set) #f)
	      ((equal? x (car set)) #t)
		  (else (element-of-set? x (cdr set)))
	)
)

(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)
	)
)

(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
	      ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
		  (else (intersection-set (cdr set1) set2))
	)
)

(define (union-set set1 set2)
	(cond ((null? set1) set2)	
	      ((null? set2) set1)
		  ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
		  (else (cons (car set1) (union-set (cdr set1) set2)))
	)
)

;; test1

(element-of-set? 1 (list 2 1 4 3))
(element-of-set? 5 (list 2 1 4 3))
(intersection-set (list 1 2) (list 3 4))
(intersection-set (list 1 2 3) (list 2 4 5))
(union-set (list 1 2) '())
(union-set (list 1 2) (list 2 3))
(union-set (list 1 2) (list 3))


Ex 2.61 - 2.62
-----------------------------

(define (element-of-set? x set)
	(cond ((null? set) #f)
	      ((= x (car set)) #t)
		  ((< x (car set)) #f)
		  (else (element-of-set? x (cdr set)))
	)
)

(define (adjoin-set x set)
	(cond ((null? set) (list x))
	      ((= x (car set)) set)
		  ((< x (car set)) (cons x set))
		  (else (cons (car set) (adjoin-set x (cdr set))))
	)
)

(define (intersection-set set1 set2)
	(cond ((or (null? set1) (null? set2)) '())
	 	  ((= (car set1) (car set2)) (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
		  ((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
	      (else (intersection-set set1 (cdr set2)))
	)
)	

(define (union-set set1 set2)
	(cond ((null? set1) set2)
	      ((null? set2) set1)
		  ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
		  ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
		  (else (cons (car set2) (union-set set1 (cdr set2))))
	)
)

;; test
(element-of-set? 1 '())
(element-of-set? 1 (list 0 1 2))
(element-of-set? 4 (list 1 2 3))
(adjoin-set 1 '())
(adjoin-set 2 (list 1 2 3))
(adjoin-set 4 (list 1 3 5))
(adjoin-set 4 (list 1 2 3))
(intersection-set (list 1 2 3) '())
(intersection-set (list 1 2) (list 3 4))
(intersection-set (list 1 2 3) (list 2 3 4))
(union-set (list 1 2) '())
(union-set '() (list 1 2))
(union-set (list 1 2) (list 3 4))
(union-set (list 1 2) (list 2 3))


Ex 2.63 - 2.64
-----------------------------

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right) (list entry left right))

;; inorder traversal : LNR 
(define (tree->list-1 tree)
	(if (null? tree)
    	'()
	 	(append (tree->list-1 (left-branch tree))
		        (cons (entry tree)
				      (tree->list-1 (right-branch tree))))
	)
)

;; inorder traversal : 
(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list (left-branch tree)
			              (cons (entry tree)
			                    (copy-to-list (right-branch tree) result-list)))
		)
	)
	(copy-to-list tree '())
)

(define tree1 (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '()))))
"
				    7
				   / \
				  3   9
				 / \   \
				1   5  11
"
(define tree2 (list 3 (list 1 '() '()) (list 7 (list 5 '() '()) (list 9 '() (list 11 '() '())))))
"
				    3
				   / \
				  1   7
				     / \
				    5   9
				         \
				         11				 
"
(define tree3 (list 5 (list 3 (list 1 '() '()) '()) (list 9 (list 7 '() '()) (list 11 '() '()))))
"
				    5
				   / \
				  3   9
				 /   / \
				1   7  11
"

"
(tree->list-1 tree1)
(tree->list-1 tree2)
(tree->list-1 tree3)

(tree->list-2 tree1)
(tree->list-2 tree2)
(tree->list-2 tree3)  ;; all are same
"

(define (list->tree elements)
	(car (partial-tree elements (length elements)))
)

(define (partial-tree elts n)
	(if (= n 0)
		(cons '() elts)
		(let ((left-size (quotient (- n 1) 2)))
		     (let ((left-result (partial-tree elts left-size)))
			      (let ((left-tree (car left-result))
		          	(non-left-elts (cdr left-result))
					(right-size (- n (+ left-size 1))))
				  	(let ((this-entry (car non-left-elts))
						 (right-result (partial-tree (cdr non-left-elts) right-size)))
					     (let ((right-tree (car right-result))
							  (remaining-elts (cdr right-result)))
						 	(cons (make-tree this-entry left-tree right-tree)
							      remaining-elts))))))
	)
)

Ex 2.73
-----------------------------

(define (variable? exp) (symbol? exp))
(define (same-variable? v1 v2)
	(and (variable? v1) (variable? v2) (eq? v1 v2))
)
	
(define (deriv exp var)
	(cond ((number? exp) 0)
	      ((variable? exp) (if (same-variable? exp var) 1 0))
		  (else ((get 'deriv (operator exp)) (operands exp) var))
	)
)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
 	(define (make-sum x y) (list '+ x y))
 	(define (deriv-sum opnds var)
		(make-sum (deriv (car opnds) var) (deriv (cadr opnds) var))
	)
	(define (make-prod x y) (list '* x y))
	(define (deriv-prod opnds var)
	 	(define opnd1 (car opnds))
		(define opnd2 (cadr opnds))
		(make-sum (make-prod opnd1 (deriv opnd2 var))
		          (make-prod opnd2 (deriv opnd1 var)))
	)
	(put 'deriv '+ deriv-sum)
	(put 'deriv '* deriv-prod)
	'done
)














