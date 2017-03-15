; test 121
(string->symbol ((lambda (b) (symbol->string b)) 'a))

; test 128
(define f (lambda (p x) (begin
                            (set-car! p x)
                            p)))
(f (cons 4 5) 444)

; test 129
(define f (lambda (p x) (begin
                            (set-cdr! p x)
                            p)))
(f (cons 4 5) 444)

; test 130
(apply (lambda (a) (* a a)) '(2))
