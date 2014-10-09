
;; Project Euler Probelm #8
;; The four adjacent digits in the 1000-digit number that have the greatest
;; 		product are 9 × 9 × 8 × 9 = 5832.
;; Find the thirteen adjacent digits in the 1000-digit number that have the 
;;		greatest product. What is the value of this product?
;;
;; Suporting Files : ./input/8.txt -- the 1000 digit number
;;
;; Answer : ??

(begin
	
	(load "lib.scm")

	; creates a list containing every character in the file in order
	(define read-file (lambda (x) 
		(cond 
			((not (eof-object? (peek-char x))) (cons (read-char x) (read-file x))) 
			(else '()))))

	; takes a list and returns the product of count consecutive elements
	; 	from start
	(define consecutive-product (lambda (l start count)
		(cond 
			((<= (length l) start) 0)
			((= count 0) 1)
			(else (* (list-ref l start) (consecutive-product l (add1 start) (sub1 count)))))))

	; reads all characters, filters newlines, maps to digits
	;	finds all possible consecutive products, finds greatest
	(let* (
		(characters (filter (lambda (c) (not (eq? #\newline c))) (read-file (open-input-file "input/8.txt"))))
		(numbers (map char->digit characters))
		(products (map (lambda (p) (consecutive-product numbers p 13)) (range 0 (length numbers) 1))))
		(display (greatest products)))

)
