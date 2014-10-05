
;; Project Euler Problem #3
;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143?
;;
;; Answer : 6857

;;v1
(begin

	; these blocked off functions were to implement a prime sieve, but I took a simpler
	;	aproach, leaving the functions here for later use, and for documentation
	; ---------------------------------------------------------------------------------------
	; returns a list containing every 'increment' integer between 
	;	start (inclusive) and end (exclusive) 
	(define range (lambda (start end increment)
		(if (not (or (and (< start end) (> increment 0)) (and (> start end) (< increment 0))))
			'()
			(begin
				(if ((if (< start end) > <=) start end) 
					'()
					(begin
						(cons start (range (+ start increment) end increment))))))))


	; returns true if heap contains item
	(define contains? (lambda (heap item) 
		(cond 
			((null? heap) #f)
			((eq? (car heap) item) #t)
			(else (contains (cdr heap) item)))))

	; returns a list containing all items in source that are not contained in sink
	(define subtraction (lambda (source sink)
		(cond
			((not (pair? source)) '())
			((contains? sink (car source)) (subtraction (cdr source) sink))
			(else (cons (car source) (subtraction (cdr source) sink))))))

	; "zips" two lists together, returns a new list with the first element from a
	; 	second element form b, third from a, and so forth, if one list is shorter
	;	than the other, the tail of the list will be the end of the longer list
	(define zip (lambda (a b)
		(cond
			((not (pair? a)) b)
			((not (pair? b)) a)
			(else (cons (car a) (zip b (cdr a)))))))

	; ---------------------------------------------------------------------------------------

	; factorial
	(define ! (lambda (n)
		(cond 
			((= n 0) 1)
			(else (* n (! (- n 1)))))))

	; uses Wilson's Theorem to test for primailty, slower than the below prime check
	(define wilsonPrime? (lambda (n)
		(let* (
			(subFact (! (- n 1)))
			(subFactModN (remainder subFact n)))
			(cond
				((= subFactModN 0) #f)
				(else (= (remainder subFact subFactModN) 0))))))

	; finds the greatest number in a list
	(define greatest (lambda (l) 
		(cond
			; if we're at the last pair (x . '()), return x
			((not (pair? (cdr l))) (car l))
			; otherwise, return the greater of the first and the lowest of the tail
			(else (let (
				(tailGreatest (greatest (cdr l))))
				(cond
					((>= tailGreatest (car l)) tailGreatest)
					(else (car l))))))))

	; uses trial division to test for primality
	(define prime? (lambda (n)
		(define try (lambda (n div)
			(cond 
				((= (remainder n div) 0) #f)
				((> div (sqrt n)) #t)
				(else (try n (+ 1 div))))))
		(try n 2)))
	
	; finds the lowest divisor, by first finding the lowest even divisor
	;	of the number n, and then forming a list of that number, and the
	;	factorization of the quotient (n / divisor), if n happens to be
	;	prime, than a terminated list is returned containing only n
	(define factorize (lambda (n)
		(define findLowestDivisor (lambda (start n)
			(cond
				((= (remainder n start) 0) start)
				(else (findLowestDivisor (+ start 1) n)))))
		(cond 
			((prime? n) (cons n '()))
			(else (let* (
				(lowestDivisor (findLowestDivisor 2 n))
				(quotient (quotient n lowestDivisor)))
				(cons lowestDivisor (factorize quotient)))))))

	; do the stuff!
	(display (greatest (factorize 600851475143)))
)