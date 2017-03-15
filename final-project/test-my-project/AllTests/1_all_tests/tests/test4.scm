; test 31
(define (equal? x y)
    (if (not (pair? x))
        (eq? x y)
        (and (equal? (car x) (car y))
             (equal? (cdr x) (cdr y)))))
(equal? (cons 1 2) (cons 1 3))

; test 32
(define (variable? x) (symbol? x))
(variable? #t)

; test 33
((lambda (x y)
      (cond ((= x y) #t)
            ((> x y) 'x>y)
            ((and (> (+ x y) 10) (> (* x y) 40)) 'string)
            )
      ) 111 11)

; test 34
((lambda (a) (if (string? a) (string->symbol a))) "a23")

; test 35
 (define (=number? exp num)
  (and (number? exp) (= exp num)))
(=number? 5 1)

; test 37
(define (a x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set)(a x (cdr set))))))
	
(a 3 (cons 5 4))

; test 38
(define (expmod a b m) 
  (cond ((= b 0) 1)
	((= (remainder b 2) 0) (remainder (expmod (remainder (* a a) m) (/ b 2) m) m))
	(else (remainder (* a (expmod a (- b 1) m)) m))))
   
(expmod 5 13 1)

; test 39
(define (a str)
    (define (b x sum)
      (cond
        ((= (string-length str) x) sum)
        (else (b (+ x 1) (+ (char->integer (string-ref str x)) (* 256 sum))))))
    (b 0 0))
(a "hello")

; test 40
(define (b set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let ((s1 (car set1))
           (s2 (car set2)))
       (cond
       ((= s1 s2) (cons s1 (b (cdr set1) (cdr set2))))
       ((> s1 s2) (cons s2 (b set1 (cdr set2))))
       ((< s1 s2) (cons s1 (b (cdr set1) set2))))))))
(b '(1 2 3) '(4 5 6))
