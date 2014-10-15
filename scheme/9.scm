
;; Project Euler Problem #9
;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;; 		a^2 + b^2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;;
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.
;;
;; Answer : ??

(begin
	(load "lib.scm")

	; for every number a from  1-1000
	; b = numbers from a to 1000
	; c must equal sqrt (a^2 + b^2)
	; a + b + sqrt(a^2 + b^2) must equal 1000, since only one, stop when found

	(define a (lambda (i)
		(define b (lambda (j)
			(let* (
				(sum (+ i j (sqrt (+ (expt i 2) (expt j 2))))))
			(cond
				((= sum 1000) 
					(output "{a, b, c} : {" i ", " j ", " (- sum i j) "}"))
				((< sum 1000) (b (add1 j)))
				(else #f)))))
		(define find (lambda (n)
			; (display n)(newline)
			(cond
				((> (+ i i n) 1000) #f) 
				((not (b (+ i n))) (find (add1 n))))))
		(output "a: " i)
		(find 1)
	))

	(define find (lambda (n)
		; (display n)(newline)
		(cond
			((> (+ n n n) 1000) #f) 
			((not (a n)) (find (add1 n))))))
	(find 1)
)