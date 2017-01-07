(define tag-var?
  (lambda (expr)
    (equal? 'var (car expr))))

(define this-var?
  (lambda (expr var)
    (and (tag-var? expr) (equal? var (cadr expr)))))

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


(define part-of-list
  (lambda (var lst part not-part)
    (if (member var lst) part not-part)))

(define lex-address-bvar-lambda
  (lambda (lambda-body lambda-var major minor)
    (cond ((lambda-simple? lambda-body) (part-of-list lambda-var (cadr lambda-body) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var (add1 major) minor))))
          ((lambda-opt? lambda-body) (part-of-list lambda-var (cons (caddr lambda-body) (cadr lambda-body)) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,(caddr lambda-body)
                                                  ,@(lex-address-bvar (cdddr lambda-body) lambda-var (add1 major) minor))))
          ((lambda-var? lambda-body) (part-of-list lambda-var (cadr lambda-body) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var (add1 major) minor)))))
    ))

(define lex-address-bvar
  (lambda (body var major minor)
    (cond ((null-or-not-pair? body) body)
          ((this-var? body var) `(bvar ,(cadr body) ,major ,minor))
          ((lambda-expr? body) (lex-address-bvar-lambda body var major minor))
          (else `(,(lex-address-bvar (car body) var major minor) ,@(lex-address-bvar (cdr body) var major minor))))
    ))

(define lex-address-pvar-lambda
  (lambda (lambda-body lambda-var minor)
    (cond ((lambda-simple? lambda-body) (part-of-list lambda-var (cadr lambda-body) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var 0 minor))))
          ((lambda-opt? lambda-body) (part-of-list lambda-var (cons (caddr lambda-body) (cadr lambda-body)) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,(caddr lambda-body)
                                                  ,@(lex-address-bvar (cdddr lambda-body) lambda-var 0 minor))))
          ((lambda-var? lambda-body) (part-of-list lambda-var (cadr lambda-body) lambda-body
                                    `(,(car lambda-body) ,(cadr lambda-body) ,@(lex-address-bvar (cddr lambda-body) lambda-var 0 minor)))))
    ))

(define lex-address-pvar
  (lambda (body var minor)
    (cond ((null-or-not-pair? body) body)
          ((this-var? body var) `(pvar ,(cadr body) ,minor))
          ((lambda-expr? body) (lex-address-pvar-lambda body var minor))
          (else `(,(lex-address-pvar (car body) var minor) ,@(lex-address-pvar (cdr body) var minor))))
    ))

(define lex-address
  (lambda (body vars minor)
    (if (null? vars) body
        (lex-address (lex-address-pvar body (car vars) minor) (cdr vars) (add1 minor)))
    ))

(define lex-pe-lambda
  (lambda (expr)
    (cond ((lambda-simple? expr) `(,(car expr) ,(cadr expr) ,(pe->lex-pe (lex-address (caddr expr) (cadr expr) 0))))
          ((lambda-opt? expr) 
           `(,(car expr) ,(cadr expr) ,(caddr expr) ,(pe->lex-pe (lex-address (cadddr expr) `(,@(cadr expr) ,(caddr expr)) 0))))
          ((lambda-var? expr) `(,(car expr) ,(cadr expr) ,(pe->lex-pe (lex-address (caddr expr) (list (cadr expr)) 0)))))
    ))

(define pe->lex-pe
  (lambda (expr)
    (cond ((null-or-not-pair? expr) expr)
          ((tag-var? expr) `(fvar ,@(cdr expr)))
          ((lambda-expr? expr) (lex-pe-lambda expr))
          (else `(,(pe->lex-pe (car expr)) ,@(pe->lex-pe (cdr expr)))))
    ))