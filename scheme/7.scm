
;; Project Euler Problem #6
;; By listing the first six prime numbers: 2, 3, 5, 7, 11 and 13, we can see that the 6th prime is 13.
;;
;; What is the 10001st prime number?
;;
;; Answer : 104759

;; notes:
;; -- this solution involved writing the sieve-of-eratosthenes function
;; 		found in the lib file
;; -- kind of a cop out though, we just kind of guess and regess as to 
;;		what number the nth prime number might be under, there might
;;		be some sort of approximation function to look into, like
;;		a fit of the nth prime to the number it's below?

(begin
	(load "lib.scm")

	; finds the n prime by guessing what number it will be under
	(define find-nth-prime (lambda (n mult)
		(let* (
			(bound (* n mult))
			(primes (sieve-of-eratosthenes bound)))
			(cond
				((>= (length primes) n) (list-ref primes n))
				(else (find-nth-prime n (+ mult 2)))))))

	(display (find-nth-prime 10001 11))

)