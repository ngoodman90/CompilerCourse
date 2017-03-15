(load "pattern-matcher.scm")
(print-gensym #f) 


      
(define simple-const?
      (lambda (constant)
	  (or (vector? constant) (boolean? constant) (char? constant) (number? constant) (string? constant))
	  ))
(define Applic?
	(let ((run 
			(compose-patterns
			;constant
				(pattern-rule
					(? 'c simple-const?  )
					(lambda (c) #f))
				(pattern-rule
					`(quote ,(? 'c) )
					(lambda (c) #f))
				;;applic
				(pattern-rule
					`(,(? 'operator) ,@(? 'exprs list?) )
					  (lambda (var lst) 
					      #t))
					     )))
	(lambda (e)
				(run e
						(lambda ()
							#f)))))
															

  (define (map function list)
    ;; non-variadic map.  Returns a list whose elements are
    ;; the result of calling function with corresponding
    ;; elements of list
    (if (null? list)
        '()
        (cons (function (car list))
              (map function (cdr list)))))
   
			
(define df?
    (lambda (lstBinding)
	(andmap (lambda (ls) (= (length (filter (lambda (x) (eq? (car x) (car ls))) lstBinding)) 1)) lstBinding))) 
		 (set! a 0)

(define cse
    (lambda (exprs)
	(begin (set! Bindings '())
	(set! Gen '())
	(set! GenBind '())
	 (set! a 0)
	 (set! c 0)

	 (set! Lisst exprs)
	(AddBind Lisst)
	 (set! GenBind (MakeGensym Bindings))

	 (subGenBind)
	
	(SubsituteAllBindings GenBind)
(CorrectTheLisst GenBind)
	(set! Gen GenBind)
	

	    (if (null? Gen)
		exprs
		(if (= 1 (length  Gen ))
		 (append (append (list 'let) (list Gen) (list Lisst)))
	  (append (append (list 'let*) (list Gen) ) (list Lisst)))
	 ))))
	 
(define SubsituteAllBindings
    (lambda (lst)
	(map (lambda (x) 	
	    (set! Lisst (Subsitute x Lisst ) )
	)
     lst )
    ))
	


(define subGenBind (lambda ()  ;; To subs in Genbind nested list

(map (lambda (x) 

  (set! GenBind  (map (lambda (y)  
         (if (equal?  x y)
                      y
                    ;  (begin (display x)
                (Subsitute x y)
                      
                
                


)) GenBind)   )) GenBind))

)
	 
	 
(define (HowMuch? x lst)
     (if (null? lst)  a                                
         (if (and (Applic? x) (Applic? (car lst)) (equal? x (car lst) )) (begin (set! a (+ a 1))   (if  (Applic? (car lst))
       (begin (HowMuch? x (car lst)) (HowMuch? x (cdr lst)))   
              (HowMuch? x (cdr lst))
              ))
         
         (if  (Applic? (car lst))
       (begin (HowMuch? x (car lst)) (HowMuch? x (cdr lst)))   
              (HowMuch? x (cdr lst))
              )
                 )))
                 
(define (HowMuch1? x lst)

     (if (null? lst)  c                                
         (if  (equal? x (car lst) ) (begin (set! c (+ c 1))   (if  (Applic? (car lst))
       (begin (HowMuch1? x (car lst)) (HowMuch1? x (cdr lst)))   
              (HowMuch1? x (cdr lst))
              ))
         
         (if  (Applic? (car lst))
       (begin (HowMuch1? x (car lst)) (HowMuch1? x (cdr lst)))   
              (HowMuch1? x (cdr lst))
              )
                 )))				      

(define (Notmember? x list)
     (if (null? list) #t                                
         (if (equal? x (car list)) #f                  
              (Notmember? x (cdr list))))) 

				      
(define How?
      (lambda (x lst)
      (if (> (HowMuch? x lst) 1) (begin (set! a 0) #t)
	    (begin (set! a 0) #f ))))
	    
(define NotMore3?
      (lambda (x lst)

      (if (< (HowMuch1? x lst) 3) (begin (set! c 0) #t)
	    (begin (set! c 0) #f ))))
				      
(define AddToBindig
			     (lambda (lst)     

	  (if (null? lst) Bindings                                
          (if (and (How? (car lst) Lisst)  (Notmember? (car lst) Bindings) ) (begin (set! Bindings (append Bindings (list (car lst))))  
         (if  (Applic? (car lst))
       (begin (AddToBindig (car lst)) (AddToBindig (cdr lst)))   
              (AddToBindig (cdr lst))
              ))
         
         (if  (Applic? (car lst))
       (begin (AddToBindig  (car lst)) (AddToBindig (cdr lst)))   
              (AddToBindig (cdr lst))
              
                 )))  ))
(define AddBind
	 (lambda (lst)
	 	  
	(map (lambda (x) 
	(if  (Applic?  x) 
	       (begin (AddBind x) (if (and (How? x Lisst)  (Notmember? x Bindings) ) (set! Bindings (append Bindings (list x)))  
	       x
	       ))   
	;else 
              x
                 )        )lst)))
			
(define converts 
      (lambda (lst)
	  (map (lambda (x) (cdr x )) lst)
      ))
	  
			
(define SubInBind
    (lambda (bindG lst)     
	(map (lambda (x) 
	(if (equal? (car bindG) x) 
	       (cadr bindG)   
	;else
	(if (list? x)
	(SubInBind bindG  x )
	;else 
              x
                 )        )) lst)))
               

               
(define CorrectTheLisst
    (lambda (lst)
    
	(filter (lambda (x) (not (eq? x '())))  (map (lambda (pair1)
		  (let ((gen1 (car pair1))
		       (bind1 (cdr pair1)))
		(if (NotMore3? gen1 (append lst Lisst)) 
		(set! GenBind (SubInBind pair1 (remove pair1 lst))) ;(remove gen1 Lisst);then
		
		'()    )))lst))
		 
		   ))

						      


(define MakeGensym
    (lambda (Bindings)
	    (map (lambda (x) 
			(cons (gensym)  (list x))) Bindings)))
			
			
			

			
(define Subsitute
	  (lambda (bindG lst)     
	(map (lambda (x) 
	(if (and (Applic?  x) (equal? (cadr bindG) x)) 
	       (car bindG)   
	;else
	(if 
	(Applic? x)  
	(Subsitute bindG  x )
	;else 
              x
                 )        ))lst)))
	      




	      
	      
	      

	  
	
	

