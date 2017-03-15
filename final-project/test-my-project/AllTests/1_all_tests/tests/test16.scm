; test 151
((lambda (list) (begin
		(set-car! (car (cdr list)) (cons 1 2))
		 list)) (list 1 (cons 22 66) 3 4))

; test 152
((lambda (list) (begin
		(set-cdr! (cdr list) (cons 1 2))
		list)) (list 1 2 3 4))

; test 153
(let* ((x 1)
         (y 2)
         (z 3))
    (+ x y z)
    )

; test 154
((lambda (x y) (
                 let* ((a x)
                       (b y)
                       )
                 (* a a b))
    ) 44 55)

; test 155
(letrec ((loop (lambda (i a)
		 (set! a (+ (* 10 a) i))
		 (if (< i 10)
		     (loop (+ i 1) a)
		     a))))
  (loop 0 0))

; test 156
(define func (lambda (lst num) (
                                  letrec ((loop
                                             (lambda (i a)
                                               (cond ((null? i)
                                                      #f)
                                                 ((eq? (car i) a) #t)
                                                 (else
                                                   (loop (cdr i) a)))
                                               )))
                                    (loop lst num)))
                 )
(func (list 1 2 3) 5)

; test 157
(quasiquote (0 1 2))

; test 158
(quasiquote (0 (unquote (+ 1 2)) 4))

; test 159
(quote (1 a (* 4)))

; test 160
(define q (quote (bla (((s ) s )sd ))))
q
