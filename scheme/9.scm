
;; Project Euler Problem #9
;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;; 		a^2 + b^2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;;
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.
;;
;; Answer :31875000

(begin
	(load "lib.scm")

	; finds pythagorean triplets such that c < c-bound
	; uses the fact that any pythagorean triplets can be found
	;	using three integers, m, n and k such that:
	; 		- m > n
	;		- (m - n) is odd
	; if this is true, then the triplet has components:
	;		a = k * (m^2 - n^2)
	;		b = k * 2 * m * n
	;		c = k * (m^2 + n^2)
	; using the fact that we want m^2 + n^2 to be less than the bound
	;	placed on c, and that n is less than m, we can generalize the
	;	statment for c to c = 2m^2, which means we only need to examine
	; 	values of m less than the ceiling of sqrt(bound / 2)
	; for each m, we then only need to look at values that are less than
	; 	m, also, we can look at multiples of 2 starting at 2 or 3, depending
	; 	on if m is odd or not, if m is odd, n needs to be even, and conversely
	; then, all that is left is to pick a k value, to find triples that are
	;	not "primitive" (that is, a, b, and c are all coprime)
	; we can simplify the expresison for c again, to get bound > 2 * k * m^2
	;	to k < bound / (2 * m^2)
	; after this, we just add the found triplet to a list, not before checking
	;	to see if a or b should be ordered first in the triplet, as they
	;	are exchangable in the equation a^2 + b^2 = c^2, though by definition
	; 	a < b < c
	; note: the bounds are kind of mathmatically flubbed, so before 
	;	confirming the triplet, I check that c is actually less than c-bound
	(define triplets (lambda (c-bound)
		(define found (list))
		(let* (
			(m-bound (ceiling (sqrt (/ c-bound 2))))
			(possible-m (range 2 (add1 m-bound) 1))
			(try-n-values (lambda (m)
				(let* (
					(n-start (+ (remainder m 2) 1))
					(possible-n (range n-start (add1 m) 2))
					(try-k-values (lambda (n)
						(let* (
							(k-bound (round (/ c-bound (* 2 (expt m 2)))))
							(possible-k (range 1 (add1 k-bound) 1))
							(find-triplet (lambda (k)
								(let* (
									(a (* k (- (expt m 2) (expt n 2))))
									(b (* k 2 m n))
									(c (* k (+ (expt m 2) (expt n 2)))))
									(if (< c c-bound)
										(cond 
											((> a b) (set! found (append found (list (list b a c)))))
											(else (set! found (append found (list (list a b c)))))))))))
							(map find-triplet possible-k)))))
						(map try-k-values possible-n)))))
					(map try-n-values possible-m))
		found))

	; meat of the solution
	(let (
		(triplet (car (filter (lambda (n) (= (apply + n) 1000)) (triplets 1000)))))
		(output "(a, b, c) = " triplet "- product: " (apply * triplet)))
)