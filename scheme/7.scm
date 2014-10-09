
;; Project Euler Problem #6
;; By listing the first six prime numbers: 2, 3, 5, 7, 11 and 13, we can see that the 6th prime is 13.
;;
;; What is the 10001st prime number?
;;
;; Answer : ??

(begin
	(load "lib.scm")

	; alternative aproach to eratosthenes sieve...

	; construct a list of pairs that contain a number and a #t or #f value, initially all #t (to indicate prime)
	; on each iteration
	;	 find itr-th #t value
	;	 if > than sqrt(n) -- stop
	;	 else
	;		mark every #t-th value as false
	; iterate

	; in a list of dotted pairs, finds the n-th true pair 
	;	if the pair is defined as (n . #t), 0 indexed
	; returns false if there are not at least n true values
	;	contained in l
	(define find-true (lambda (l n)
		(cond
			((null? l) #f)
			((and (= n 0) (cdar l)) (caar l))
			((= n 0) (find-true (cdr l) n))
			((cdar l) (find-true (cdr l) (sub1 n)))
			(else (find-true (cdr l) n)))))

	; takes a dotted truth pair and marks it to the truth value v
	(define set-truth-value (lambda (n v)
		(cons (car n) v)))

	; returns a list with every ~div element marked as value
	;	starting at index start (0 indexed)
	(define mark-multiples (lambda (l div value start)
		(cond
			((null? l) '())
			((= start 0) (cons (set-truth-value (car l) value) (mark-multiples (cdr l) div value (sub1 div))))
			(else (cons (car l) (mark-multiples (cdr l) div value (sub1 start)))))))

	; returns a list containing the values in the truth-pair list
	;	l for which the truth-value was #value
	(define pick-values (lambda (l value)
		(cond
			((null? l) '())
			((and (cdar l) value) (cons (caar l) (pick-values (cdr l) value)))
			(else (pick-values (cdr l) value)))))

	; generates the first n primes
	(define sieve-test (lambda (n)
		(define remove-multiples (lambda (l iter bound)

			(define multiple (find-true l iter))
			(display "iterating")(newline)
			(cond
				((> multiple bound) l)
				(else (let (
					(refined (mark-multiples l multiple #f (- (* 2 multiple) 2))))
					(remove-multiples refined (add1 iter) bound))))))
		(let* (
			(multiples (map (lambda (k) (cons k #t)) (range 2 n 1)))
			(pairs (remove-multiples multiples 0 (ceiling (sqrt n)))))
			(pick-values pairs #t))))

) 