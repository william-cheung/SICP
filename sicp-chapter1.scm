Ex 1.20
--------------
(define (gcd a b)
	(if (= b 0) 
		a 
		(gcd b (remainder a b)))
)

Ex 1.21
--------------
(define (smallest-divisor n)
	(find-divisor n 2))
(define (find-divisor n i)
 	(cond ((> (* i i) n) n)
		  ((divides? i n) i)
		  (else (find-divisor n (+ i 1)))
	)
)
(define (divides? a b)
	(= (remainder b a) 0)
)
;; test
(smallest-divisor 199) 		;; 199
(smallest-divisor 1999) 	;; 1999
(smallest-divisor 19999)	;; 7

Ex 1.30
--------------
(define (sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))
	)
	(iter a 0)
)


Ex 1.31
--------------
(define (prod term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)))
		)
	)	
	(iter a 1)
)
(define (factorial n)
	(prod (lambda (x) x) 1 (lambda (x) (+ x 1)) n)
)

(factorial 0)
(factorial 1)
(factorial 6)

(define (pi-calc n) 
	(define (term k)
		(if (even? k) 
			(/ (+ k 2.0) (+ k 1.0))
			(/ (+ k 1.0) (+ k 2.0))
		)
	)
	(* 4.0 (prod term 1 (lambda (x) (+ x 1)) n))
)

(pi-calc 10)
(pi-calc 100)
(pi-calc 1000)

Ex 1.32
---------------
(define (accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result (term a)))
		)
	)
	(iter a null-value)	
)

(accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)
(accumulate (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)

Ex 1.33
---------------
(define (accumulate combiner null-value term a next b filter)
	(define (filtered-term x) 
		(if (filter x) (term x) null-value)
	)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result (filtered-term a)))
		)
	)
	(iter a null-value)	
)

(accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10 even?)

Ex 1.34
----------------
(define (f g)
	(g 2)
)
(f f)

Ex 1.35
----------------
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
 		(< (abs (- v1 v2)) tolerance)
 	)
 	(define (try guess)
 		(let ((next (f guess)))
 			(if (close-enough? guess next)
 				next
 				(try next)
 			)
 		)
 	)
 	(try first-guess)
)

(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0) ;; 1.6180327868852458

Ex 1.36
----------------
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
 		(< (abs (- v1 v2)) tolerance)
 	)
 	(define (try guess)
 		(newline)
 		(display guess)
 		(let ((next (f guess)))
 			(if (close-enough? guess next)
 				next
 				(try next)
 			)
 		)
 	)
 	(try first-guess)
)

(define (average x y) (/ (+ x y) 2.0))
(fixed-point (lambda (x) (average x (/ (log 1000.0) (log x)))) 2.0) 

Ex 1.37
----------------
(define (cont-frac n d k)
	(define (clac i)
		(if (= i k)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (clac (+ i 1))))
		)
	)
	(clac 1)
)

(define tolerance 0.0001)
(define (golden-ratio)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)
	(define (iter i pre)
		(newline)
		(display i)
		(display " ")
		(display pre)
		(let ((next  (cont-frac (lambda (i) 1.0) 
								(lambda (i) 1.0)
								(+ i 1))
			 ))
			 (if (close-enough? pre next)
			 	next
			 	(iter (+ i 1) next)
			 )
		)
	)
	(iter 0 0)
)

(golden-ratio) ;; 11

Ex 1.38
----------------
(define (cont-frac n d k)
	(define (clac i)
		(if (= i k)
			(/ (n i) (d i))
			(/ (n i) (+ (d i) (clac (+ i 1))))
		)
	)
	(clac 1)
)
(define (d i)
	(define r (remainder i 3))
	(cond ((= r 0) 1)
		  ((= r 1) 1)
		  ((= r 2) (* 2 (/ (+ i 1) 3)))
	)
)
(define tolerance 0.0001)
(define (natrual-log-base)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)
	(define (iter i pre)
		(newline)
		(display i)
		(display " ")
		(display pre)
		(let ((next  (cont-frac (lambda (i) 1.0) 
								d
								(+ i 1))
			 ))
			 (if (close-enough? pre next)
			 	next
			 	(iter (+ i 1) next)
			 )
		)
	)
	(+ 2.0 (iter 0 0))
)
(natrual-log-base) ;; 8, 2.718279569892473

Ex 1.40
----------------
(define (cube x) (* x x x))
(define (cubic a b c)
	(lambda (x) (+ (cube x) (* a (square x)) (* b x) c))
)

((cubic 1 2 3) 2)

Ex 1.41
----------------
(define (double op)
	(lambda (x) (op (op x)))
)
(define (inc x) (+ x 1))
((double inc) 3) 						;; 5
(((double (double double)) inc)  5)		;; 21

Ex 1.42
----------------
(define (compose f g)
	(lambda (x) (f (g x)))
)
(define (inc x) (+ x 1))
((compose square inc) 6) ;; 49

Ex 1.43
----------------
(define (compose f g)
	(lambda (x) (f (g x)))
)

(define (repeated f n)
	(if (= n 0)
		(lambda (x) x)
		(lambda (x) ((compose f (repeated f (- n 1))) x))
	)
)

(define (repeated2 f n)
	(define (iter i fn)
		(if (= i n) fn
			(iter (+ i 1) (compose f fn))
		) 
	)	
	(iter 0 (lambda (x) x))
)

((repeated square 2) 5)		;; 625
((repeated2 square 2) 5)	;; 625

Ex 1.44
----------------
(define (average x y z) (/ (+ x y z) 3.0))
(define dx 0.001)
(define (smooth f)
	(lambda (x) (average (f (- x dx)) (f x) (f (+ x dx))))
)

Ex 1.46
----------------
(define (iterative-improve good-enough? improve)
	(define (iter guess)
		(if (good-enough? guess)
			guess
			(iter (improve guess))
		) 
	)
	(lambda (guess) (iter guess))
)

(define (sqrt x)
	(define tolerance 0.0001)
	(define (improve guess)
		(/ (+ guess (/ x guess)) 2)
	)
	(define (good-enough? guess)
		(let ((next (improve guess))) 
			(< (abs (- guess next)) tolerance)	
		)
	)
	((iterative-improve good-enough? improve) 1.0)
)

(sqrt 1)
(sqrt 2)
(sqrt 3)
(sqrt 16)
(sqrt 17)
