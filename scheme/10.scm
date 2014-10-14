
;; Project Euler Probelem #10
;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;; Find the sum of all the primes below two million.
;;
;; Answer : 142913828922

(begin
	(load "lib.scm")

	(let* (
		(primes (sieve-of-eratosthenes 2000000)))
		(display (reduce + primes)))
	
)