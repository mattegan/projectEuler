
;; Project Euler Problem #6
;; By listing the first six prime numbers: 2, 3, 5, 7, 11 and 13, we can see that the 6th prime is 13.
;;
;; What is the 10001st prime number?
;;
;; Answer : ??

(begin
	(load "lib.scm")

	; this sieve works kind of like the sieve of eratothesnes but in a strange way
	;	it first finds all primes from 2 to sqrt(n), multiplies them by two to get <- using trial division
	;	the first multiple of the prime that shouldn't be included, converts these
	;	to ranges from prime * 2 to n by prime, and then subtracts these from a list
	;	that ranges from 2 to n by 1, kind of like the sieve in reverse
	(define weird-sieve (lambda (n)
		(let* (
			(bounds (ceiling (sqrt n)))
			(primes (filter prime? (range 2 bounds 1)))
			(starts (map (lambda (k) (* 2 k)) primes))
			(ranges (apply append (map (lambda (k) (range (list-ref starts k) n (list-ref primes k))) (range 0 (length primes) 1))))
			(domain (range 2 n 1))
			(result (subtraction domain ranges)))
		(display result))))
	
)