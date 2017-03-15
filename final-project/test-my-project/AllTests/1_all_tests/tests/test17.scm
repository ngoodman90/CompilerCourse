; test 161
(quasiquote (1 2 (unquote (+ 3 4))))

; test 162
(quasiquote ( a 3 4 (unquote (* 4 3 2 1))))

; test 164
`(unquote (quote (3 4 5)))

; test 166
(let* ((a 1) (b 1) (c (* a b)))
   c)

; test 167
(define (lst . x) x)
(lst 1 2 3 4 5 6)

; test 168
(define (func . numbers)
    (if (null? numbers)
        0
        (+ (car numbers) (apply func (cdr numbers)))))
(func 9 8 7 6 5 4)

; test 169
(define (f . x) (apply + x))
(f 5 4 8 6)
