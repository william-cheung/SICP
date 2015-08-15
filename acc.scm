(define (foo n) (lambda (i) (set! n (+ n i)) n))

(define acc (foo 0))

(acc 1)
(acc 2)
