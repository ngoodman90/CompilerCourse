; test 91
((lambda () (+)))

; test 92
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 55) 66)

; test 93
((((lambda () (lambda (aa) (lambda (bb) (- aa bb))))) 55) 66)

; test 94
((((lambda () (lambda (aa) (lambda (bb) (+ aa bb))))) 30) 4)

; test 95
((lambda (a b c d) (a (b (c d)))) + - * 4)

; test 96
(define tar1 (lambda (a)
                (begin
                  (define r a)
                  (if (= r 1) 1 (+ 1 (tar1 (- r 1)))))))
				  
(tar1 50)

; test 97
(define tar2 (lambda (a)
                (begin
                  (define r a)
                  (cond ((= r 1) 1)
                   (else (* 2 (tar2 (- r 1))))))))
				  
(tar2 5)

; test 98
(define bin2dec (lambda (x y)
                    (begin
                      (define rem (remainder x 10))
                      (set! y (+ (* y 2) (* y rem)))
                      (if (= x 0)
                          y
                          (bin2dec (remainder x 10) y)
                          )
                      )
                    )
    )
(bin2dec 1000 2)

; test 99
(define rem (lambda (x)(remainder x 10)))
(rem 443)

; test 100
(define f (lambda (b) (/ 200 b)))
(f 4)
