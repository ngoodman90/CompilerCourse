; ############################# not #############################
(define not
    (lambda (x)
        (if x #f #t)))

; ############################# list #############################
    
(define list
    (lambda x x))
    
; ############################# map #############################
    
(define map
    ((lambda (a) 
        ((lambda (m) 
            ((lambda (m-lst) 
                (lambda (f . b) 
                    (m-lst f b))) 
            (a (lambda (m-lst) 
                (lambda (f b) 
                    (if (null? (car b)) '() 
                        (cons (apply f (m car b)) 
                            (m-lst f (m cdr b))))))))) 
        (a (lambda (m) 
            (lambda (f b) 
                (if (null? b) '() 
                    (cons (f (car b)) 
                        (m f (cdr b)))))))))
    (lambda (f)
        ((lambda (x)
            (f (lambda (a y)
                ((x x) a y))))
        (lambda (x) 
            (f (lambda (a y)
                ((x x) a y))))))))

; ############################# foldr #############################

(define foldr
    (lambda (op last a)
        'a
        (letrec ((loop
                    (lambda (x)
                        (if (not (null? x))
                            (op (car x) (loop (cdr x)))
                            last))))
            (loop a))))

; ############################# append #############################

(define help-concat
    (lambda (a b)
        (if (not (null? a))
            (cons (car a) (help-concat (cdr a) b))
            b)))
            
(define concat
    (lambda (a lst)
        (if (not (null? lst))
            (help-concat a (concat (car lst) (cdr lst)))
            a)))

(define append
    (lambda lst
        (if (null? lst)
            '()
            (concat (car lst) (cdr lst)))))

; ############################# car cdr #############################

(define special-func1
    (lambda (f g)
        (lambda (x)
            (f (g x)))))

(define special-func
    (lambda a
        (foldr
            special-func1
            (lambda (x) x)
            a)))
      
(define caar (special-func car car))
(define cadr (special-func car cdr))

(define caaar (special-func car caar))
(define caadr (special-func car cadr))
(define cadar (special-func car cdar))
(define caddr (special-func car cddr))

(define caaaar (special-func car caaar))
(define caaadr (special-func car caadr))
(define caadar (special-func car cadar))
(define caaddr (special-func car caddr))
(define cadaar (special-func car cdaar))
(define cadadr (special-func car cdadr))
(define caddar (special-func car cddar))
(define cadddr (special-func car cdddr))

(define cdar (special-func cdr car))
(define cddr (special-func cdr cdr))

(define cdaar (special-func cdr caar))
(define cdadr (special-func cdr cadr))
(define cddar (special-func cdr cdar))
(define cdddr (special-func cdr cddr))

(define cdaaar (special-func cdr caaar))
(define cdaadr (special-func cdr caadr))
(define cdadar (special-func cdr cadar))
(define cdaddr (special-func cdr caddr))
(define cddaar (special-func cdr cdaar))
(define cddadr (special-func cdr cdadr))
(define cdddar (special-func cdr cddar))
(define cddddr (special-func cdr cdddr))