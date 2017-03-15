; test 191
(or)

; test 192
(and)

; test 193
(+ 3 4 5/4 (* 1000 2/1000) 4/5 3 2 4 3/200)

; test 194
(define f (lambda (x) (if (zero? x) x (+ 1 (f (- x 1))))))
(eq? 50 (f 50))

; test 195
`(+ 1 ,(car '(1 2)) ,@'(2 3))

; test 196
`((unquote-splicing (quote (3 4 5))))

; test 197
`(+ ,'(+ 1 2 3) ,'(+ 2 3) (+ ,@'( 6 7)))

; test 198
`(+ ,(+ 1 2 3) ,(+ 2 3) (+ ,@'( 6 7)))

; test 199
(quasiquote (+ ,(+ 1 2 3) ,(+ 2 3) (+ (unquote-splicing '( 6 7)))))

; test 200
`(+ ,(cons 2 3) ,@'((cons 2 3)) ,'( 2 3))

