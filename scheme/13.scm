
;; Project Euler Problem #13
;; Work out the first ten digits of the sum of the following
;; 		one-hundred 50-digit numbers.
;;
;; Suporting Files : ./input/13.txt -- the 100 50-digit numbers
;;
;; Answer : 5537376230

(begin
	(load "lib.scm")
	
	; creates a list containing every character in the file in order
	(define parse-file-to-strings (lambda (f) 
		(let* (
			(current-number-string "")
			(lines (list)))
			(define parse-next-char (lambda ()
				(cond
					((eof-object? (peek-char f)))
					((char-numeric? (peek-char f)) 
						(set! current-number-string (string-append current-number-string (string (read-char f))))
						(parse-next-char))
					(else 
						(set! lines (append lines (list current-number-string)))
						(set! current-number-string "")
						(cond
							((not (eof-object? (peek-char f))) (read-char f) (parse-next-char)))))))
			(parse-next-char)
			lines)))

	(define numbers (map string->integer (parse-file-to-strings (open-input-file "input/13.txt"))))
	(define sum (apply + numbers))
	(output "sum: " sum)(newline)
	(output "first ten digits: " (substring (number->string sum) 0 10))(newline)

)
