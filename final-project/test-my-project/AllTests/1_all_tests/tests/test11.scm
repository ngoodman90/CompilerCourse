; test 101
((lambda (a b) (cons a b)) 5 4)

; test 103
(boolean? (procedure? (lambda () (make-string 5))))

; test 104
((lambda (a) (boolean? a)) #t)

; test 105
((lambda (a) (if (char? a) (char->integer a) (if (integer? a) (integer->char a) a))) #\x50)

; test 106
(pair? (cons 4 6))

; test 107
((lambda (a b) (cons a b)) 55 6)

; test 108
(pair? (lambda (a b) (cons a b)))

; test 109
((lambda (a b) (pair? (cons a b))) 1234 5678)

; test 110
(procedure? (lambda (a b) (cons a b)))
