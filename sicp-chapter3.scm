Ex 3.1
-----------------------------
(define (make-accumulator n)
	(lambda (i) (set! n (+ n i)) n)
)

(define A (make-accumulator 5))
(A 10)
(A 10)

Ex 3.2
-----------------------------
(define (make-monitored f)
	(let ((count 0))
		(lambda (arg)
			(if (symbol? arg)
				(cond ((eq? arg 'how-many-calls?) count)
				      ((eq? arg 'reset-count) (set! count 0) #t)
				      (else (error "invalid argument"))
				)
				(begin (set! count (+ count 1)) (f arg))
			)
		)
	)
)

(define s (make-monitored sqrt))
(s 100)
(s 9)
(s 'how-many-calls?)
(s 'reset-count)
(s 100)
(s 'how-many-calls?)

Ex 3.3
-----------------------------
(define (make-account balance password)
	(lambda (pw op) 
		(if (not (eq? pw password)) 
			(begin (display "Incorrect password") (lambda (x) #f))
			(cond ((eq? op 'withdraw) (lambda (x) 
										(if (>= balance x) 
											(begin (set! balance (- balance x)) balance)
											(display "Insufficient funds"))))
			      ((eq? op 'deposit) (lambda (x) (set! balance (+ balance x)) balance))
			      (else (display "Invalid operation") (lambda (x) #f))
			)
		)
	)
)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

Ex 3.4
-----------------------------
(define (make-account balance password)
	(define count 0)
	(define inval #f)
	(lambda (pw op) 
		(cond ((not (eq? pw password)) 
				(display "Incorrect password\n") 
				(cond ((not inval) (set! inval #t) (set! count (+ count 1)))
				      (inval (set! count (+ count 1)) 
				      	(cond ((> count 3) 
				      			(display "you have accessed the acount more than three\n")
				      			(display "consecutive times with an incorrect password\n")
				      			(display "the system is calling the cops ...\n")
				      			(set! inval #f) (set! count 0))	
				      	))
				)
				(lambda (x) #f))
			(else (set! count 0) (set! inval #f)
				(cond ((eq? op 'withdraw) (lambda (x) 
											(if (>= balance x) 
												(begin (set! balance (- balance x)) balance)
												(begin (display "Insufficient funds\n") #f))))
			          ((eq? op 'deposit) (lambda (x) (set! balance (+ balance x)) balance))
			          (else (display "Invalid operation\n") (lambda (x) #f))
				))
		)
	)
)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)



Ex 3.5
-----------------------------
(define (random-in-range a b)
	(+ a (random (- b a)))
)
	
(define (estimate-integral pred x1 y1 x2 y2 trials)
	(define (iter trials result)
	 	(if (= trials 0) result
			(let ((x (random-in-range x1 x2))
				  (y (random-in-range y1 y2)))
				(iter (- trials 1) (if (pred x y) (+ result 1) result))
			)
		)
	)
	(/ (iter trials 0.0) trials)
)

(define (estimate-pi trials)
	(define (pred x y)
	 	(< (+ (square x) (square y)) 1.0)
	)
	(* (estimate-integral pred -1.0 -1.0 1.0 1.0 trials) 4.0)
)


(estimate-pi 1000)
(estimate-pi 10000)
(estimate-pi 100000)
(estimate-pi 1000000)


Ex 3.7
-----------------------------

(define (make-account balance password)
	(lambda (pw op) 
		(if (not (eq? pw password)) 
			(begin (display "Incorrect password") (lambda (x) #f))
			(cond ((eq? op 'withdraw) (lambda (x) 
										(if (>= balance x) 
											(begin (set! balance (- balance x)) balance)
											(display "Insufficient funds"))))
			      ((eq? op 'deposit) (lambda (x) (set! balance (+ balance x)) balance))
			      ((eq? op 'reset) (lambda (x) (set! password x)))
			      (else (display "Invalid operation") (lambda (x) #f))
			)
		)
	)
)


Ex 3.8
-----------------------------

(define (make-f)
	(define first #t)
	(lambda (x) (if first (begin (set! first #f) x) 0))
)

(define f (make-f))
;; (+ (f 1) (f 0))		;; 0
;; (+ (f 0) (f 1))		;; 1






