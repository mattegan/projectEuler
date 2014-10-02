
;;Project Euler Problem #1
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 
;; 		6 and 9. The sum of these multiples is 23.
;; Find the sum of all the multiples of 3 or 5 below 1000.
;;
;; Answer : 233168

;;v2
(begin
	(define sumDivisibleBy (lambda (n end) 
		(let ((count (quotient end n)))
			(* count (+ count 1) 0.5 n))))
	(display (- (+ (sumDivisibleBy 3 999) (sumDivisibleBy 5 999)) (sumDivisibleBy 15 999)))
	(newline)
)

;;v1
; (begin
; 	(define div3
; 		(lambda (n) (= (remainder n 3) 0)))
; 	(define div5
; 		(lambda (n) (= (remainder n 5) 0)))
; 	(define num 1)
; 	(define numsum 0)
; 	(define limit 1000)
; 	(define checknext
; 		(lambda () 
; 			(if (< num limit)
; 				(begin
; 					(if (or (div3 num) (div5 num))
; 						(begin
; 							(set! numsum (+ numsum num))
; 							(display num)(display ", ")
; 						)
; 					)
					
; 					(set! num (+ num 1))
; 					(checknext)
; 				)
; 			)
; 		)
; 	)
; 	(checknext)
; 	(display numsum)
; )