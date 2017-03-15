; test 111
(zero? 5)

; test 112
(not (zero? 5))

; test 113
(define a (lambda (b) (rational? b)))
(a 56)

; test 114
(define a (lambda (b) (not (rational? b))))
(a 56)

; test 115
(denominator (/ 10 2))

; test 116
(numerator 100/50)

; test 117
(define a (lambda (x y) (if (not (zero? x)) (denominator (/ y x)) (numerator y))))
(a 0 5)

; test 119
(define x (lambda (a b) (if (> (string-length a) b) (string-ref a b) a)))
(char->integer (x "hello" 3))

; test 120
(define x (lambda (a b c) (if (> (string-length a) b) (string-set! a b c) a)))
(string->symbol (x "hello" 30 #\r))
