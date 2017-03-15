; test 71
(define foo2 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (f))
                            (set! a (f))
                            (set! a (f))
                            a)
                 )
    )
(foo2 50 60)

; test 72
(define foo3 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (set! a (f a))
                            (set! a (f a))
                            (set! a (f a))
                            a)
                 )
    )
(foo3 43 3)

; test 73
(define foo4 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            a)
                 )
    )
(foo4 31 3)

; test 74
(define foo5 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            (g)
                            (f x))
                 )
    )
(foo5 11 4)

; test 75
(define foo6 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b a x y)))
                            (define g (lambda () (set! x 5)))
                            (define t (lambda () (set! a y)))
                            (g)
                            (t)
                            (f x))
                 )
    )
(foo6 101 3)

; test 76
(define foo7 (lambda (x y) (
                            begin
                            (set! y x)
                            (set! x y)
                            (+ y x))
                 )
    )
(foo7 1 3)

; test 77
(define foo8 (lambda (x y) (
                            begin
                            (define y x)
                            (+ y x))
                 )
    )
(foo8 2 3)

; test 78
(define foo9 (lambda (x y) (
                            begin
                            (define y x)
                            (eq? y x))
                 )
    )
(foo9 12 8)

; test 79
(define foo10 (lambda (x y) (
                            begin
                            (set! y x)
                            (eq? y x))
                 )
    )
(foo10 12 12)

; test 80
(define bar1 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define num b)
                  (cond ((eq? num 0) a)
                    (else
                      (bar1 (rec1 a) (- b 1)))
                    )
                  )
                )
    )
	
(bar1 4 3)
