
;; Project Euler Problem #6
;; The sum of the squares of the first ten natural numbers is,
;; 		1^2 + 2^2 + ... + 10^2 = 385
;; The square of the sum of the first ten natural numbers is,
;;		(1 + 2 + ... + 10)^2 = 55^2 = 3025
;; Hence the difference between the sum of the squares of the 
;;		first ten natural numbers and the square of the sum 
;;		is 3025 − 385 = 2640.
;; Find the difference between the sum of the squares of the first 
;;		one hundred natural numbers and the square of the sum.
;;
;; Answer : 25164150

(begin
	
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

	; finds the difference between the sun of the squares of the first
	;	n natural numbers, and the sqare of the sum of the first n
	;	natural numbers
	(define findDiff (lambda (n)
		(let* (
			(sumSquares (apply + (map (lambda (n) (expt n 2)) (range 1 (+ n 1) 1))))
			(squareSum (expt (apply + (range 1 (+ n 1) 1)) 2)))
			(- squareSum sumSquares))))
)