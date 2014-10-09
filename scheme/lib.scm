
;; Handy Scheme Functions

; ---------------------------------------------------------------------------------------
; Math
; ---------------------------------------------------------------------------------------

; factorial
(define ! (lambda (n)
	(cond 
		((= n 0) 1)
		(else (* n (! (- n 1)))))))

; uses Wilson's Theorem to test for primailty, slower than the below prime check
(define wilsonPrime? (lambda (n)
	(let* (
		(subFact (! (- n 1)))
		(subFactModN (remainder subFact n)))
		(cond
			((= subFactModN 0) #f)
			(else (= (remainder subFact subFactModN) 0))))))

; uses trial division to test for primality
(define prime? (lambda (n)
	(define try (lambda (n div)
		(cond 
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
		((prime? n) (cons n '()))
		(else (let* (
			(lowestDivisor (findLowestDivisor 2 n))
			(quotient (quotient n lowestDivisor)))
			(cons lowestDivisor (factorize quotient)))))))

; ---------------------------------------------------------------------------------------
; List Comprehension / Operations
; ---------------------------------------------------------------------------------------

; returns a list containing all items in source that are not contained in sink
(define subtraction (lambda (source sink)
	(cond
		((not (pair? source)) '())
		((contains? sink (car source)) (subtraction (cdr source) sink))
		(else (cons (car source) (subtraction (cdr source) sink))))))

; "zips" two lists together, returns a new list with the first element from a
; 	second element form b, third from a, and so forth, if one list is shorter
;	than the other, the tail of the list will be the end of the longer list
(define zip (lambda (a b)
	(cond
		((not (pair? a)) b)
		((not (pair? b)) a)
		(else (cons (car a) (zip b (cdr a)))))))

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

; returns a list containing only elements in n for which pass? evaluates to #t
(define filter (lambda (pass? n) 
	(cond
		((not (pair? n)) '()) 
		((pass? (car n)) (cons (car n) (filter pass? (cdr n))))
		(else (filter pass? (cdr n))))))

; returns a list without nested items
(define flatten (lambda (l)
	(cond
		((null? l) '())
		((not (pair? l)) (list l))
		(else (append (flatten (car l)) (flatten (cdr l)))))))

; returns true if heap contains item
(define contains? (lambda (heap item) 
	(cond 
		((null? heap) #f)
		((eq? (car heap) item) #t)
		(else (contains (cdr heap) item)))))

; ---------------------------------------------------------------------------------------
; Searching / Reducing Lists
; ---------------------------------------------------------------------------------------

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

; ---------------------------------------------------------------------------------------
; Strings
; ---------------------------------------------------------------------------------------

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


; ---------------------------------------------------------------------------------------
; Histograms
; ---------------------------------------------------------------------------------------

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