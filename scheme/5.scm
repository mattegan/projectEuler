
;; Project Euler Problem #5
;; 2520 is the smallest number that can be divided by each of the numbers 
;; 		from 1 to 10 without any remainder.
;; What is the smallest positive number that is evenly divisible by all 
;;		of the numbers from 1 to 20?
;;
;; Answer : 232792560

;; v1
;; -- this is a brute force solution
(begin

	; finds the element in a list that op returns true for against all other
	;	elements, could be used to find least or greatest 
	;	(doesn't really have the best name)
	(define against (lambda (op l) 
		(cond
			; if we're at the last pair (x . '()), return x
			((null? (cdr l)) (car l))
			; otherwise, return the greater of the first and the lowest of the tail
			(else (let (
				(tailAgainst (against op (cdr l))))
				(cond
					((op tailAgainst (car l)) tailAgainst)
					(else (car l))))))))

	; returns true if n is evenly divisibile by every number from 1, up to and
	;	including to
	(define testDivisibilityUpTo (lambda (n to)
		(cond
			((= to 2) #t)
			((not (= (remainder n to) 0) ) #f)
			(else (testDivisibilityUpTo n (- to 1))))))

	(define findFirstDivisibleUpTo (lambda (to) 
		(define findN (lambda (n) 
			(cond 
				((testDivisibilityUpTo n to) n)
				(else (findN (+ n 20))))))
		(findN to)))

	(display (findFirstDivisibleUpTo 20))(newline)
)