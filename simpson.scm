(define (simpson f a b n)
 	(define h (/ (- b a) n))
	(define (c k) 
	 	(cond 	((= k 0)   1.0)
		 		((even? k) 2.0)
				(else      4.0)
		)
	)
	(define (simpson-aux x k tmp)
		(if (= k n) (/ (* (+ tmp (f (+ x h))) h) 3.0)
			(simpson-aux (+ x h) (+ k 1) (+ tmp (* (c k) (f x))))	
		)
	)
	(simpson-aux a 0 0.0)
)

;; test
(define (cube x) (* x x x))
(simpson cube 0.0 1.0 2)
(simpson cube 0.0 1.0 100)
(simpson cube 0.0 1.0 1000)
