; test 181
(let ((a 1))
  (let ((b 2) (c 3))
    (let ((d 4) (e 5) (f 6))
      (= 720 (* a b c d e f)))))

; test 182
(define sum (lambda (n) (/ (* n (+ n 1)) 2)))
(sum 300)

; test 183
(define with (lambda (s f) (apply f s)))
(define fact
  (letrec ((fact-1
	    (lambda (n r)
	      (if (zero? n)
		  r
		  (fact-2 (- n 1)
			  (* n r)
			  'moshe
			  'yosi))))
	   (fact-2
	    (lambda (n r _1 _2)
	      (if (zero? n)
		  r
		  (fact-3 (- n 1)
			  (* n r)
			  'dana
			  'michal
			  'olga
			  'sonia))))
	   (fact-3
	    (lambda (n r _1 _2 _3 _4)
	      (if (zero? n)
		  r
		  (fact-1 (- n 1)
			  (* n r))))))
    (lambda (n)
      (fact-1 n 1))))
(fact 10)

; test 184
(define with (lambda (s f) (apply f s)))
(define list (lambda args args))
(define fact-1
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-1)
	(with (fact-2 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-1 trail)))))))
(define fact-2
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-2)
	(with (fact-3 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-2 trail)))))))
(define fact-3
  (lambda (n)
    (if (zero? n)
	(list 1 'fact-3)
	(with (fact-1 (- n 1))
	  (lambda (r . trail)
	    (cons (* n r)
	      (cons 'fact-3 trail)))))))
(fact-1 10)

; test 185
(+ 1 1/2)

; test 186
(+ 1/2 1)

; test 187
(+ 1/3 2/3)

; test 188
(+)

; test 189
(= (+ (/ 1 3) 5/3 (/ 9 27)) 7/3)

; test 190
(*)
