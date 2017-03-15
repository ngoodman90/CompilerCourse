; test 81
(define bar2 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (set! b (- b 1))
                  (cond ((eq? b 0) a)
                    (else
                      (bar2 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar2 4 5)

; test 82
(define bar3 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (bar3 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar3 6 2)

; test 83
(define bar4 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (rec1 (bar4 a b)))
                    )
                  )
                )
    )
	
(bar4 5 2)

; test 84
(define bar5 (lambda (a b)
                  (begin
                    (define rec1 (lambda (b) (* b b)))
                    (define rec2 (lambda (b) (- b 1)))
                    (set! b (rec2 b))
                    (if (eq? b 0) a
                        (rec1 (bar5 a b)))

                    )
                  )
    )
	
(bar5 5 3)

; test 85
(define bar6 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (rac b)
                    )
                  )
    )
	
(bar6 1 2 3)

; test 86
(define bar7 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar7 (rac a) (- b 1) c))
                    )
                  )
    )
	
(bar7 5 2 6)

; test 87
(define bar8 (lambda (a b c d)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar8 (rac a) (- b 1) c d))
                    )
                  )
    )
	
(bar8 1 5 2 6)

; test 88
(define bar9 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar9 (rac a) (- b 1) c d e))
                    )
                  )
    )
(bar9 2 7 3 3 10)

; test 89
(define bar10 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar10 (rac (rac1 (rac2 a))) (- b 1) c d e))
                    )
                  )
    )
(bar10 1 5 4 6 1)

; test 90
(((lambda (x)  
    (lambda (z)
      (* x x))) 4) 5)
