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







;;;;;;;;;;;;;;;;;;;;;; Removing redundant applications ;;;;;;;;;;;;;;;;;


(define remove-applic-expr-helper?
	(lambda (expr)
		(and 
			(pair? expr) 
			(lambda-simple? expr) 
			(= 3 (length expr)) 
			(null? (cadr expr)))
		))

(define remove-applic-expr?
  (lambda (expr)
    (and 
    	(equal? 'applic (car expr)) 
    	(remove-applic-expr-helper? (cadr expr)))
    ))

(define remove-applic-lambda-nil
  (lambda (expr)
    (cond ((null-or-not-pair? expr) expr)
          ((remove-applic-expr? expr) (remove-applic-lambda-nil (caddr (cadr expr))))
          (else `(,(remove-applic-lambda-nil (car expr)) ,@(remove-applic-lambda-nil (cdr expr)))))
    ))




;;;;;;;; Annotating Variables with their Lexical address ;;;;;;;;;;;;;;





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








;;;;;;;;;;;;;;;;;;;;;;;;;; Annotating tail calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define tc-helper
  (lambda (expr rest first?)
    (cond ((and first? (= 1 (length expr))) `(,(list rest) ,expr))
          ((null? (cdr expr)) `(,rest ,expr))
          (else (tc-helper (cdr expr) (if first? `(,rest ,(car expr)) `(,@rest ,(car expr))) #f)))
    ))

(define lambda-tail-helper
  (lambda (pe)
    (cond ((lambda-simple? pe)`(,(car pe) ,(cadr pe) ,(lambda-tail (caddr pe))))
          ((lambda-opt? pe) `(,(car pe) ,(cadr pe) ,(caddr pe) ,(lambda-tail (cadddr pe))))
          ((lambda-var? pe) `(,(car pe) ,(cadr pe) ,(lambda-tail (caddr pe)))))
          ))

(define dont-care?
  (lambda (pe)
    (or (null-or-not-pair? pe)
        (member (car pe) (list 'const 'var 'fvar 'pvar 'bvar 'box-get)))))

(define definition?
  (lambda (pe)
    (member (car pe) (list 'def 'define))))

(define set-op?
  (lambda (pe)
    (member (car pe) (list 'set 'box-set))))

(define same-op?
	(lambda (pe)
		(or (dont-care? pe) (lambda-expr? pe) (definition? pe) (set-op? pe) (equal? 'box (car pe)))))

(define same-op
	(lambda (pe)
		(cond ((dont-care? pe) pe)
					((lambda-expr? pe) (lambda-tail-helper pe))
					((definition? pe) `(,(car pe) ,(cadr pe) ,(annotate-tc (caddr pe))))
          ((set-op? pe) `(,(car pe) ,(cadr pe) ,@(annotate-tc (cddr pe))))
          ((equal? 'box (car pe)) `(box ,@(annotate-tc (cdr pe)))))
		))

(define lambda-tail
  (lambda (pe)
    (cond ((same-op? pe) (same-op pe))
          ((equal? 'if3 (car pe)) `(if3 ,(annotate-tc (cadr pe)) ,(lambda-tail (caddr pe)) ,(lambda-tail (cadddr pe))))
          ((equal? 'or (car pe)) `(or (,@(car (tc-helper (cdadr pe) (caadr pe) #t)) 
                                       ,@(lambda-tail (cadr (tc-helper (cdadr pe) (caadr pe) #t))))))
          ((equal? 'applic (car pe)) `(tc-applic ,(annotate-tc (cadr pe)) ,(map annotate-tc (caddr pe))))
          ((equal? 'seq (car pe)) `(seq ,`(,@(map annotate-tc (car (tc-helper (cdadr pe) (caadr pe) #t)))
                                                 ,@(lambda-tail (cadr (tc-helper (cdadr pe) (caadr pe) #t))))))
          (else `(,(lambda-tail (car pe)))))
    ))


(define annotate-tc
  (lambda (pe)
    (cond ((same-op? pe) (same-op pe))
          ((equal? 'if3 (car pe)) `(if3 ,(annotate-tc (cadr pe)) ,(annotate-tc (caddr pe)) ,(annotate-tc (cadddr pe))))
          ((equal? 'or (car pe)) `(or (,@(car (tc-helper (cdadr pe) (caadr pe) #t)) 
                                       ,@(annotate-tc (cadr (tc-helper (cdadr pe) (caadr pe) #t))))))
          ((equal? 'applic (car pe)) `(applic ,(annotate-tc (cadr pe)) ,(map annotate-tc (caddr pe))))
          ((equal? 'seq (car pe)) `(seq ,(map annotate-tc (cadr pe))))
          (else `(,(annotate-tc (car pe)))))
    ))
