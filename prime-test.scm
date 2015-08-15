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

;; -------------------------------------------------------------------

(define (expmod base exp m)
 	(cond ((= exp 0) 1)
 		  ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
 		  (else (remainder (* base (expmod base (- exp 1) m)) m))
 	)
) 

(define (fermat-test n)
 	(define (try-it a)
 		(= (expmod a n n) a)
 	)
 	(try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
 	(cond ((= times 0) true)
 		  ((fermat-test n) (fast-prime? n (- times 1)))
 		  (else false)
 	)
)

;; -------------------------------------------------------------------

(define (timed-prime-test n)
	(newline)
	(display n)
 	(start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
	(if (fast-prime? n 5) (report-prime (- (runtime) start-time)))
)

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)	;; ms
)

;; -------------------------------------------------------------------

	
(define (test n count) 
	(cond 	((= count 0) (newline) (display "Finished"))
			((prime? n) (timed-prime-test n) (test (+ n 1) (- count 1)))
			(else (test (+ n 1) count))
	)
) 

(test 1000 3)
(test 10000 3)
(test 100000 3)
(test 1000000 3)
(test 10000000 3)
(test 100000000 3)
(test 1000000000 3)

(prime? 561)				;; #f
(fast-prime? 561 5)			;; #t
(fast-prime? 561 10)		;; #t
(fast-prime? 561 100)		;; #t

;; ------------------------------------------------------------------

(define (fermat-ratio n)
	(/ (fermat-count n 1 0) (* (- n 1) 1.0))
)

(define (fermat-count n a cnt)
	(cond 	((= a n) cnt)
			((= (expmod a n n) a) (fermat-count n (+ a 1) (+ cnt 1)))
			(else (fermat-count n (+ a 1) cnt))
	)
)

;; ------------------------------------------------------------------

(define (test2 n)
	(test-helper n 2)
)

(define (test-helper n i)
	(cond	((= i n) (newline) (display "Finished"))
			(else 	(newline) 
					(display i)
					(display (if (prime? i) " T: " " F: ")) 
					(display (fermat-ratio i)) 
					(test-helper n (+ i 1)))
	)
)

;;(test2 10000)

;; ------------------------------------------------------------------

(define (fast-prime2? n times) 
	(cond 	((= times 0) true)
			((miller-rabin-test n) (fast-prime2? n (- times 1)))
			(else false)
	)
)

(define (miller-rabin-test n)
	(define (try-it n a)
		(= (expmod2 a (- n 1) n) 1)
	)
	(try-it n (+ (random (- n 1)) 1))
)

(define (expmod2 a m n)
	(cond 	((= m 0) 1)
			((even? m) (trivial-root-test (expmod2 a (/ m 2) n) n))
 		  	(else (remainder (* a (expmod2 a (- m 1) n)) n))
	)
)

(define (trivial-root-test x n)
	(define tmp (remainder (square x) n))
	(if (= tmp 1)
		(cond 	((= x 1) tmp)
			  	((= x (- n 1)) tmp)
			  	(else 0)
		)
		tmp
	)
)


(fast-prime2? 561 5)		;; #f Carmichael number
(fast-prime2? 1001 5)		;; #f
(prime? 1001)				;; #f
(fast-prime2? 2 5)			;; #t

(fast-prime2? 1105 5)		;; #f Carmichael numbers, ...		
(fast-prime2? 1729 5)
(fast-prime2? 2469 5)
(fast-prime2? 2821 5)
(fast-prime2? 6601 5)
(fast-prime2? 1999 5)		;; #t
