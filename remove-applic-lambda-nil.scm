(define lambda-simple?
	(lambda (expr)
		(equal? (car e) 'lambda-simple)))

(define nil-vars?
	(lambda (expr)
		(null? (cadr e))))

(define body-nil-lambda
	(lambda (expr)
		(car (cddadr expr))))

(define lambda-nil?
	(lambda (expr)
		(let ((first (car expr))
			  (rest-first (cadr expr)))
		(if (and 
				(equal? 'applic first)
				(lambda-simple? rest-first) 
				(= 3 (length rest-first)) 
				(nil-vars? rest-first) 
				(pair? rest-first))
			#t
			#f)))


(define remove-applic-lambda-nil
	(lambda (expr)
		(cond ((not (pair? expr) expr)
			  ((null? expr) expr)
			  ((lambda-nil? expr) (remove-applic-lambda-nil (body-nil-lambda expr)))
			  (else '(,(remove-applic-lambda-nil (car expr) , @remove-applic-lambda-nil (cdr expr))))))))
