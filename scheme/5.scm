
;; Project Euler Problem #5
;; 2520 is the smallest number that can be divided by each of the numbers 
;; 		from 1 to 10 without any remainder.
;; What is the smallest positive number that is evenly divisible by all 
;;		of the numbers from 1 to 20?
;;
;; Answer : 232792560

;; v2
;; notes:
;; -- this solution uses the fact that this problem is essentially finding
;;		the LCD for a list of numbers
;; -- the function "findFirstDivisibleUpTo" should probably be refactored
;;		into a generic, LCD function that accepts a list of numbers, not
;;		an upper limit
;; -- the previous solution took ~25 seconds to find the first number 
;;		divisible by all numbers 1-20, v2 does this in 0.01 seconds
;;		or less (sometimes time returns 0), for perspective in the 
;;		same time it takes v1 to find the answer, v2 can find the
;;		LCM for all numbers between 1 and 8000, with an actual
;;		prime sieve, this could potentially be even faster
(begin

	; returns a list containing only elements in n for which pass? evaluates to #t
	(define filter (lambda (pass? n) 
		(cond
			((not (pair? n)) '()) 
			((pass? (car n)) (cons (car n) (filter pass? (cdr n))))
			(else (filter pass? (cdr n))))))

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

	; uses trial division to test for primality
	(define prime? (lambda (n)
		(define try (lambda (n div)
			(cond 
				((= n 2) #t)
				((= n 1) #f)
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
			((= n 1) (list 1)) 
			((prime? n) (cons n '()))
			(else (let* (
				(lowestDivisor (findLowestDivisor 2 n))
				(quotient (quotient n lowestDivisor)))
				(cons lowestDivisor (factorize quotient)))))))

	; takes a "histogram list" a list containing dotted pairs with the
	;	form (freq . item), finds the item n in the histogram and adds
	;	1 to it's frequency, if the item is not found, then an entry in
	;	the histogram is created for the item with an initial frequency
	;	of 1
	(define hist-add (lambda (n l)
		(cond 
			((null? l) (list (cons 1 n)))
			((= (cdr (car l)) n) (cons (cons (+ (car (car l)) 1) n) (cdr l)))
			(else (cons (car l) (hist-add n (cdr l)))))))

	; takes a list and a histogram and adds the frequencies of each
	; 	item in the list to the existing histogram
	;	(1 1 2 2 2 5) '((1 . 3)) -> ((1 . 3) (3 . 1) (2 . 2) (1 . 5))
	(define hist-supplement (lambda (hist l)
		(cond
			((null? l) hist)
			(else (hist-supplement (hist-add (car l) hist) (cdr l))))))

	; finds the count of a specific value in a histogram
	(define hist-get-value (lambda (hist n) 
		(cond
			((null? hist) 0)
			((= (cdr (car hist)) n) (car (car hist)))
			(else (hist-get-value (cdr hist) n)))))
	
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

	; finds the first number divisible by all numbers from 1 up to (and including u)
	; - it does this by finding the prime factorization of each number from [2 u]
	; - for each factorization, it finds the powers of each factor by forming a
	;	histogram of the factorization
	; - for every prime between 1 and u, form a list that has the power the prime
	;	is raised to for every number 1 through u
	; - for each one of these lists, we find the highest count, effectively finding
	; 	the highest power each prime is rasied to in all of the factorizations
	; - we raise each prime to it's highest power
	; - finally, we multiply the highest power of each prime together to find the LCD
	(define findFirstDivisibleUpTo (lambda (u)
		(let* (
			(factorizations (map (lambda (n) (hist-supplement '() n)) (map factorize (range 2 (+ u 1) 1))))
			(neededPrimes (filter prime? (range 2 (+ u 1) 1)))
			(counts (map (lambda (p) (map (lambda (n) (hist-get-value n p)) factorizations)) neededPrimes))
			(greatestCounts (map greatest counts)))
			(apply * (map (lambda (n) (expt (list-ref neededPrimes n) (list-ref greatestCounts n))) (range 0 (length neededPrimes) 1))))))

	(display (findFirstDivisibleUpTo 8000))(newline)
)

; v1
; -- this is a brute force solution
; (begin
; 	; returns true if n is evenly divisibile by every number from 1, up to and
; 	;	including to
; 	(define testDivisibilityUpTo (lambda (n to)
; 		(cond
; 			((= to 2) #t)
; 			((not (= (remainder n to) 0) ) #f)
; 			(else (testDivisibilityUpTo n (- to 1))))))

; 	(define findFirstDivisibleUpTo (lambda (to) 
; 		(define findN (lambda (n) 
; 			(cond 
; 				((testDivisibilityUpTo n to) n)
; 				(else (findN (+ n 20))))))
; 		(findN to)))

; 	(display (findFirstDivisibleUpTo 20))(newline)
; )