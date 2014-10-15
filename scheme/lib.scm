
;; Handy Scheme Functions

; ---------------------------------------------------------------------------------------
; Math
; ---------------------------------------------------------------------------------------

(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))

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
			((= 1 n) #f)
			((= 2 n) #t)
			((= (remainder n div) 0) #f)
			((> div (sqrt n)) #t)
			(else (try n (+ 1 div))))))
	(try n 2)))

; generates primes up to n
(define sieve-of-eratosthenes (lambda (b)
	(define vector-range (lambda (n)
		(let* (
			(source (make-vector n)))
			(define fill (lambda (i)
				(cond
					((< i n) (vector-set! source i i) (fill (add1 i))))))
			(fill 0)
			source)))

	(define vector-sieve-remove (lambda (vector num add value)
		(define count (vector-length vector))
		(define fill (lambda (i) 
			(let (
				(index (+ add (* i num))))
				(cond 
				((< index count) 
					(vector-set! vector index value) 
					(fill (+ i 1)))))))
		(fill 1)))

	(define vector-find-nth-value (lambda (vector n value)
		(define found 0)
		(define pass (lambda (i) 
			(cond
				((= found n) (sub1 i))
				((>= i (vector-length vector)) #f)
				((eq? (vector-ref vector i) value) (set! found (add1 found)) (pass (add1 i)))
				(else (pass (add1 i))))))
		(pass 0)))
	(define marks (make-vector (- (floor (/ b 2)) 1) #t))
	(define iteration (lambda (n)
		(let* (
			(prime-index (vector-find-nth-value marks (add1 n) #t))
			(current-prime (+ 1 (* 2 (+ 1 prime-index)))))
			(cond
				((< current-prime (ceiling (sqrt b)))
					(vector-sieve-remove marks current-prime prime-index #f)
					(iteration (add1 n)))))))
	(define find-result (lambda (i)
		(cond
			((>= i (vector-length marks)) '())
			((vector-ref marks i) (cons (add1 (* 2 (add1 i))) (find-result (add1 i))))
			(else (find-result (add1 i))))))
	(iteration 0)
	(append (list 2) (find-result 0))))

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

; returns the greatest common divisor of a set of numbers passed as arguments
(define (gcd . args)
	(define gcd-pair (lambda (a b)
		(cond 
			((= 0 b) a)
			(else (gcd b (remainder a b))))))
	(reduce gcd-pair args))

; ---------------------------------------------------------------------------------------
; List Comprehension / Operations
; ---------------------------------------------------------------------------------------

; returns true if heap contains item
(define contains? (lambda (heap item) 
	(cond 
		((null? heap) #f)
		((eq? (car heap) item) #t)
		(else (contains? (cdr heap) item)))))

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

(define reduce (lambda (sieve l)
	(cond
		((null? (cdr l)) (car l))
		(else (sieve (car l) (reduce sieve (cdr l)))))))
; returns a list without nested items
(define flatten (lambda (l)
	(cond
		((null? l) '())
		((not (pair? l)) (list l))
		(else (append (flatten (car l)) (flatten (cdr l)))))))

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
; Strings/Chars
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

(define char->digit (lambda (c)
	(cond
		((not (char-numeric? c)) (error "expected numeric character"))
		(else (- (char->integer c) 48)))))

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

; ---------------------------------------------------------------------------------------
; Other
; ---------------------------------------------------------------------------------------

(define (output . args)
	(cond ((not (null? args))
			(display (car args))
			(apply output (cdr args)))))
	
