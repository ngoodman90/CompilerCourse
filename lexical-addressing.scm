;----------------------------6 Annotating Variables with their Lexical address---------------------
(define do-lex-bound
  (lambda (body var major minor)
    (cond ((or (not (pair? body)) (null? body)) body)
          ((and (equal? 'var (car body)) (equal? var (cadr body))) `(bvar ,(cadr body) ,major ,minor))
          ((equal? 'lambda-simple (car body)) (if (member var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var (+ 1 major) minor))))
          ((equal? 'lambda-opt (car body)) (if (member var (cons (caddr body) (cadr body))) body
                                    `(,(car body) ,(cadr body) ,(caddr body) 
                                                  ,@(do-lex-bound (cdddr body) var (+ 1 major) minor))))
          ((equal? 'lambda-var (car body)) (if (equal? var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var (+ 1 major) minor))))
          (else `(,(do-lex-bound (car body) var major minor) ,@(do-lex-bound (cdr body) var major minor))))
    ))

(define do-lex-parameter
  (lambda (body var minor)
    (cond ((or (not (pair? body)) (null? body)) body)
          ((and (equal? 'var (car body)) (equal? var (cadr body))) `(pvar ,(cadr body) ,minor))
          ((equal? 'lambda-simple (car body)) (if (member var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var 0 minor))))
          ((equal? 'lambda-opt (car body)) (if (member var (cons (caddr body) (cadr body))) body
                                    `(,(car body) ,(cadr body) ,(caddr body)
                                                  ,@(do-lex-bound (cdddr body) var 0 minor))))
          ((equal? 'lambda-var (car body)) (if (equal? var (cadr body)) body
                                    `(,(car body) ,(cadr body) ,@(do-lex-bound (cddr body) var 0 minor))))
          (else `(,(do-lex-parameter (car body) var minor) ,@(do-lex-parameter (cdr body) var minor))))
    ))

(define do-lex
  (lambda (body vars minor)
    (if (null? vars) body
        (do-lex (do-lex-parameter body (car vars) minor) (cdr vars) (+ 1 minor)))
    ))

(define pe->lex-pe
  (lambda (e)
    (cond ((or (not (pair? e)) (null? e)) e)
          ((equal? 'var (car e)) `(fvar ,@(cdr e)))
          ((equal? 'lambda-simple (car e)) `(,(car e) ,(cadr e) ,(pe->lex-pe (do-lex (caddr e) (cadr e) 0))))
          ((equal? 'lambda-opt (car e)) 
           `(,(car e) ,(cadr e) ,(caddr e) ,(pe->lex-pe (do-lex (cadddr e) `(,@(cadr e) ,(caddr e)) 0))))
          ((equal? 'lambda-var (car e)) `(,(car e) ,(cadr e) ,(pe->lex-pe (do-lex (caddr e) (list (cadr e)) 0))))
          (else `(,(pe->lex-pe (car e)) ,@(pe->lex-pe (cdr e)))))
    ))



;-----------------------------------------------------------------------------------------------------------------
(define lambda-simple?
  (lambda (expr)
    (equal? 'lambda-simple (car expr))))

(define lambda-opt?
  (lambda (expr)
    (equal? 'lambda-opt (car expr))))

(define lambda-var?
  (lambda (expr)
    (equal? 'lambda-var (car expr))))

(define lambda-expr? 
  (lambda (expr)
    (or (lambda-simple? expr) (lambda-opt? expr) (lambda-var? expr))))

(define null-or-not-pair?
  (lambda (expr)
    (or (null? expr) (not (pair? expr)))))

(define lex-address-bvar-lambda
  (lambda (lambda-var lambda-body minor major)
    (cond ((lambda-simple? lambda-body) (if (member lambda-var (cadr lambda-body)) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var minor (+ 1 major)))))
          ((lambda-opt? lambda-body) (if (member lambda-var (cons (caddr lambda-body) (cadr lambda-body))) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,(caddr lambda-body) 
                                                  ,@(lex-address-bvar (cdddr lambda-body) lambda-var minor  (+ 1 major)))))
          ((lambda-var? lambda-body) (if (equal? lambda-var (cadr lambda-body)) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var minor  (+ 1 major))))))
  ));;;;;;;;;;;;;;;;;;change this!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(define lex-address-bvar
  (lambda (lambda-var lambda-body minor major)
    (cond ((null-or-not-pair? lambda-body) lambda-body)
          ((and (lambda-var? lambda-body) (equal? lambda-var (cadr lambda-body))) `(bvar ,(cadr lambda-body) ,major ,minor))
          ((lambda-expr? lambda-body) (lex-address-bvar-lambda lambda-var lambda-body minor major))
          (else `(,(lex-address-bvar (car lambda-body) lambda-var minor major) ,@(lex-address-bvar (cdr lambda-body) lambda-var minor major))))
    ))

(define lex-address-pvar-lambda 
  (lambda (lambda-var lambda-body minor)
    (cond ((lambda-simple? lambda-body) (if (member lambda-var (cadr lambda-body)) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var minor 0))))
          ((lambda-opt? lambda-body) (if (member lambda-var (cons (caddr lambda-body) (cadr lambda-body))) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,(caddr lambda-body)
                                                  ,@(lex-address-bvar (cdddr lambda-body) lambda-var minor 0))))
          ((lambda-var? lambda-body) (if (equal? lambda-var (cadr lambda-body)) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var minor 0)))))
    ));;;;;;;;;;;;;;;;;;change this!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


(define lex-address-pvar
  (lambda (lambda-var lambda-body minor)
    (cond ((null-or-not-pair? lambda-body) lambda-body)
          ((and (lambda-var? lambda-body) (equal? lambda-var (cadr lambda-body))) `(pvar ,(cadr lambda-body) ,minor))
          ((lambda-expr? lambda-body) (lex-address-pvar-lambda lambda-var lambda-body minor))
          (else `(,(lex-address-pvar (car lambda-body) lambda-var minor) ,@(lex-address-pvar (cdr lambda-body) lambda-var minor))))
    ))


(define lex-address
  (lambda (lambda-vars lambda-body minor)
    (if (null? lambda-vars) 
      lambda-body
      (lex-address 
        (lex-address-pvar lambda-body (car lambda-vars) minor) 
        (cdr lambda-vars) (+ 1 minor))
      )))

(define lex-pe-lambda
  (lambda (expr)
    (cond ((lambda-simple? expr) `(,(car expr) ,(cadr expr) ,(pe->lex-pe (lex-address (caddr expr) (cadr expr) 0))))
          ((lambda-opt? expr) `(,(car expr) ,(cadr expr) ,(caddr expr) ,(pe->lex-pe (lex-address (cadddr expr) `(,@(cadr expr) ,(caddr expr)) 0))))
          ((lambda-var? expr) `(,(car expr) ,(cadr expr) ,(pe->lex-pe (lex-address (caddr expr) (list (cadr expr)) 0)))))
    ))

(define pe->lex-pe
  (lambda (expr)
  (cond ((null-or-not-pair? expr) expr)
        ((lambda-expr? expr) (lex-pe-lambda expr))
        (else `(,(pe->lex-pe (car expr)) ,@(pe->lex-pe (cdr expr)))))
  ))