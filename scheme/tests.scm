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

	(define vector-set-every (lambda (vector start inc value)
		(define length (vector-length vector))
		(define fill (lambda (i) 
			(cond 
				((< i length) (vector-set! vector i value) (fill (+ i inc))))))
		(fill start)))

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
		(define marks (make-vector b #t))
		(define nums (range 2 b 1))
		(define result (list))
		(define iteration (lambda (n)
			(cond
				((< n (sqrt b))
					(let* (
						(prime (+ (vector-find-nth-value marks n #t) 2)))
						(vector-set-every marks (- (* 2 prime) 2) prime #f)
						(iteration (add1 n))))
				(else marks))))
		(define prune (lambda (i)
			(cond
				((>= i (vector-length marks)) #f)
				((vector-ref marks i) (set! result (append result (list (list-ref nums i)))))
				(else (prune (add1 i))))))
		(iteration 1)
		(display marks)
		(prune 0)
		result))


	; find nth iteration 0 index
	; take index, set every nth multiple of that index to 0 (if index is still < sqrt(n))
	; repeat, increase iteration
	

	; (define z (vector-range 1000000))

	; (define z (range 1 1000000 1))

	; (define z (vector-sieve 2000000))



)