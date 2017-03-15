; test 61
 (define fun2 (lambda (x)
                 (begin
                   (set! x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun2 45)

; test 62
(define fun3 (lambda ()
                 (begin
                   (define x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun3)

; test 63
(define fun4 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define x (+ (f) 3 4))
                   x
                   )
                 )
    )
(fun4)

; test 64
(define fun5 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define g (lambda () (+ (f) 3 4)))
                   g
                   )
                 )
    )
((fun5))

; test 65
(define fun6 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define g (lambda () (+ (f) 3 4)))
                   (g)
                   )
                 )
    )
(fun6)

; test 66
(define fun7 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ 2 1)))
                   (define g (lambda () (f 3 4)))
                   (g)
                   )
                 )
    )
(fun7)

; test 67
(define fun8 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (+ (g f) (g *) (g -) (g +))
                   )
                 )
    )
(fun8)

; test 68
(define fun9 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (define t (lambda (a) (
                                         if (eq? a *)
                                             *
                                             a)))
                   (+ (g f) (g (t *)) (g -) (g (t -)))
                   )
                 )
    )
(fun9)

; test 70
(define fool (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (+ (f) (f) (f)))
                            a)
                 )
    )
(fool 2 3)
