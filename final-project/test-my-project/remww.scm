(define get_read
  (lambda (i)
    (cadr i)))

(define get_write
  (lambda (i)
    (caddr i)))

(define is_member_of_list?
  (lambda (m lst)
    (cond
        ((null? lst) #f)
        ((equal? m (car lst)) #t)
        (else (is_member_of_list? m (cdr lst))))))

(define iterate_over_instructions
  (lambda (m instructions)
    (cond
        ((null? instructions) #f)
        ((is_member_of_list? m (get_read (car instructions))) #f)
        ((is_member_of_list? m (get_write (car instructions))) #t)
        (else (iterate_over_instructions m (cdr instructions))))))

(define remove_instruction?
  (lambda (instructions)
    (andmap
        (lambda (m)
            (iterate_over_instructions m (cdr instructions)))
        (get_write (car instructions)))))

(define perform_removal
  (lambda (instructions)
    (cond
        ((null? instructions) '())
	((remove_instruction? instructions) (perform_removal (cdr instructions)))
        (else (append (list (car instructions)) (perform_removal (cdr instructions)))))))

(define remww
  (lambda (instructions)
    (if (equal? instructions (perform_removal instructions))
        instructions
        (remww (perform_removal instructions)))))