(begin
	(load "lib.scm")

	(define vector-range (lambda (n)
		(let* (
			(source (make-vector n)))
			(define fill (lambda (i)
				(cond
					((< i n) (vector-set! source i i) (fill (add1 i))))))
			(fill 0)
			source)))

	(define vector-remove-odds (lambda (vector start add value)
		(define length (vector-length vector))
		(define fill (lambda (i) 
			(cond 
				((< (+ add (* i start)) length) (vector-set! vector (+ add (* i start)) value) (fill (+ i 1))))))
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

	(define vector-sieve (lambda (b)
		(define marks (make-vector (- (/ b 2) 2) #t))
		(define iteration (lambda (n)
			(cond
				((< n (sqrt b))
					(let* (
						(prime-index (vector-find-nth-value marks (add1 n) #t)))
						(vector-remove-odds marks (add1 (* 2 (add1 prime-index))) n  #f)
						(iteration (add1 n)))))))
		(define find-result (lambda (i)
			(cond
				((>= i (vector-length marks)) '())
				((vector-ref marks i) (cons (add1 (* 2 (add1 i))) (find-result (add1 i))))
				(else (find-result (add1 i))))))
		(iteration 0)
		(append (list 2) (find-result 0))
		
		; 2 3 4 5 6 7 8 9 10 11 12 13 14 15
		; 3 5 7 9 11 13 15 17 19 21 23 25 27 29, 31, 33, 35
		; 0 1 2 3 4
		; 2 4 6 8 10
		; 3 5 7 9 11

		; add + (mult * index)
		; where add is... the iteration
		; mult is the prime
		; and index always starts at 1

		; for three - start at index 3 (0 + 3), remove 3 (0 + 3), 6 (2 * (0 + 3), 9 (3 * (0 + 3),  11
		; for five - start at index 6 (1 + 5), remove 6 (1 + 5), 11 (1 + (2 * 5), 16 1 + (3 * 5)
		; for 7, start at index 9, remove 9 (2 + 7), 16 2 + (7 * 2)

		; (display result)
		))


	; find nth iteration 0 index
	; take index, set every nth multiple of that index to 0 (if index is still < sqrt(n))
	; repeat, increase iteration
	

	; (define z (vector-range 1000000))

	; (define z (range 1 1000000 1))

	(define z (vector-sieve 100))
	(display z)
)