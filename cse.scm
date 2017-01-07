
(define dup?
	(lambda (arg expr)
		(cond ((null? expr) #f)
		      ((equal? arg (car expr)) #t)
			  ((list? (car expr))
			    (or (dup? arg (car expr)) (dup? arg (cdr expr))))
			  (else (dup? arg (cdr expr))))))

(define remove-expr
	(lambda (arg expr)
		(if (null? expr)
		    '()
		    (let ((first-of-removed 
				     (if (null? (remove arg expr))
						'()
						(car (remove arg expr)))))
			(list* arg (remove-expr first-of-removed (remove arg expr)))))))
		    
		    
(define cse-dup-list
	(lambda (expr)
		(if (null? expr) 
			'()
		    (let ((first-expr (car expr))
			  	  (rest-expr (cdr expr)))
			  (cond ((and (list? first-expr) (dup? first-expr rest-expr))
			  	(list* first-expr (cse-dup-list rest-expr)))
			  ((and (list? first-expr) (not (dup? first-expr rest-expr)))
			  	(append (cse-dup-list first-expr)(cse-dup-list rest-expr)))
			  ((not (list? first-expr))
			  	(cse-dup-list rest-expr)))
		))))

(define remove-dup
	(lambda(arg expr)
		(cond ((null? expr) (list arg))
		    ((and (= 1 (length expr)) (not (equal? arg (car expr))))
				(list* arg expr))
			((not (member arg expr))
			    (list* arg (remove-dup (car expr) (cdr expr))))
			(else (let ((updatedLst (remove-expr arg expr)))
				  (if (null? updatedLst)
				      '()
				      (remove-dup (car updatedLst) (cdr updatedLst))))))))

(define arg-first-in-expr-list?
	(lambda (arg expr)
		(equal? arg (car expr))))

(define replace-expr
	(lambda (replacee replacer expr)
		(if (null? expr)
		    '()
		    (if (arg-first-in-expr-list? replacee expr)
				(list* replacer (replace-expr replacee replacer (cdr expr)))
				(list* (car expr) (replace-expr replacee replacer (cdr expr)))))))

			
(define recursive-replace
	(lambda (arg val expr)
		(cond ((null? expr) '())
			  ((member arg expr)
			    (recursive-replace arg val (replace-expr arg val expr)))
			  ((list? (car expr))
				(list* (recursive-replace arg val (car expr)) (recursive-replace arg val (cdr expr))))
			  (else (list* (car expr) (recursive-replace arg val (cdr expr)))))
		))
		
(define replace-dups
	(lambda(args expr)
		(if (null? args)
		    expr
		    (let((arg (cadar args))
			 (value (caar args)))
			(if (and (= 1(length expr)) (not (equal? arg (car expr))))
			    (list* arg expr)
			    (replace-dups (cdr args)(recursive-replace arg value expr)))))))
				     
(define non-void-list
	(lambda (expr)
		(if (not (pair? expr))
			'()
		   (cons (car expr) 
		   	(non-void-list (cdr expr)))
		   )))

(define arg-in-expr?
	(lambda (arg expr)
			(cond ((null? expr) #f)
				  ((equal? arg (car expr)) #t)
				  ((list? (car expr)) 
				  	(or (arg-in-expr? arg (car expr)) 
				  		(arg-in-expr? arg (cdr expr))))
				  (else (arg-in-expr? arg (cdr expr))))
			  ))
		    
(define get-args
	(lambda (expr)
		(if (null? expr)
		    '()
		    (let ((arg (car expr))
			  (gen-sym 's))
			  (begin (set! gen-sym  (symbol->string (gensym)))
				  (if (arg-in-expr? arg expr)
				      (list* (list gen-sym arg) (get-args (cdr(recursive-replace arg gen-sym expr))))
				      (list* (list gen-sym arg) (get-args (cdr expr)))))))))
			   
(define create-cse-args
	(lambda (expr)
		(if (not (null? expr))
			  (non-void-list (cse-dup-list expr))))
		)
		
			
(define new-args
	(lambda (args)
		(if(null? args)
		   '()
			(append (create-cse-args (car args)) (new-args (cdr args)))  
		)))

(define flatten-args
	(lambda (expr)
		(if(null? expr)
		   '()
			(append (new-args expr) expr (flatten-args (new-args expr))))))
			  
(define cse-args
	(lambda (expr)
		(if (not (null? expr))
		    (let* ((dup-list (cse-dup-list expr))
			   (no-void-dup-list (non-void-list dup-list))
			   (removed-dup-list (if (null? no-void-dup-list) 
			   		'() 		
			   		(remove-dup 
			   			(car no-void-dup-list) 
			   			(cdr no-void-dup-list))))
			   (flattened-dup-list (flatten-args removed-dup-list))
			   (final-dup-list (if (null? flattened-dup-list)
			     '() 
			     (remove-dup 
			     	(car flattened-dup-list)
			     	(cdr flattened-dup-list)))))
			  (get-args final-dup-list)
		))))
		
(define cse-expr
	(lambda (expr)
		(let ((args (cse-args expr)))
			(cond ((null? args) expr)
				   ((= 1 (length args))
				   		 `(let ,args ,(replace-dups args expr)))
				   (else `(let* ,args ,(replace-dups args expr))))
			)))
		    
(define cse
	(lambda (expr) 
		(cse-expr expr)))

		
(define cse-2 cse)