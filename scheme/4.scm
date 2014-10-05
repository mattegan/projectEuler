
;; Project Euler Problem #4
;; A palindromic number reads the same both ways. The largest palindrome made from 
;;		the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.
;;
;; Answer : 906609

;; v1
;; notes:
;;	-- this could be done faster with some other interesting speedups, but this way
;;		seems to be the most "funcitonal" (and it works)
;; -- I'm using gambit scheme, which doesn't include a lot of functions that MIT-scheme
;;		includes, I'm using this as a feature, not a bug, since it causes me to figure
;;		out how to re-implement a lot of included "fluffy" convienience functions
(begin

	; returns true if a string is a palindrome, such as -> "racecar"
	(define string-palindrome? (lambda (n)
		(cond
			((= (remainder (string-length n) 2) 0) 
				(equal? 
					(reverse 
						(string->list (substring n (/ (string-length n) 2) (string-length n))))
					(string->list (substring n 0 (/ (string-length n) 2)))))
			(else (string-palindrome? (string-append
				(substring n 0 (/ (- (string-length n) 1) 2))
				(substring n (/ (+ (string-length n) 1) 2) (string-length n))))))))

	; returns a list containing only elements in n for which pass? evaluates to #t
	(define filter (lambda (pass? n) 
		(cond
			((not (pair? n)) '()) 
			((pass? (car n)) (cons (car n) (filter pass? (cdr n))))
			(else (filter pass? (cdr n))))))

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

	; returns a list without nested items
	(define flatten (lambda (l)
		(cond
			((null? l) '())
			((not (pair? l)) (list l))
			(else (append (flatten (car l)) (flatten (cdr l)))))))


	; finds all possible products of n and k where n and k are [100, 1000]
	; filters these down to ones that are only palindromes
	; finds the largest of the resulting list
	(let* (
		(allProds 
			(flatten (map (lambda (n) (map (lambda (k) (* n k)) (range n 1000 1)))  (range 100 1000 1))))
		(palindromeProds (filter (lambda (s) (string-palindrome? (number->string s))) allProds))
		(greatestPalindromeProd (greatest palindromeProds)))
		(display greatestPalindromeProd)(newline))
)