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













(define prepare-seq-to-tail
  (lambda (e not-tail is-first?)
    (cond ((and is-first? (= 1 (length e))) `(,(list not-tail) ,e))
          ((null? (cdr e)) `(,not-tail ,e))
          (else (prepare-seq-to-tail (cdr e) (if is-first? `(,not-tail ,(car e)) `(,@not-tail ,(car e))) #f)))
    ))


;(define annotate-tc 1)

(define annotate-tail-lambda
  (lambda (pe)
    (cond ((lambda-simple? pe)`(,(car pe) ,(cadr pe) ,(annotate-tail (caddr pe))))
          ((lambda-opt? pe) `(,(car pe) ,(cadr pe) ,(caddr pe) ,(annotate-tail (cadddr pe))))
          ((lambda-var? pe) `(,(car pe) ,(cadr pe) ,(annotate-tail (caddr pe)))))
          ))

(define dont-care?
  (lambda (pe)
    (or (null-or-not-pair? pe)
        (member (car pe) (list 'const 'var 'fvar 'pvar 'bvar 'box-get)))))

(define definition?
  (lambda (pe)
    (or (equal? 'def (car pe)) (equal? 'define (car pe)))))

(define set-op?
  (lambda (pe)
    (or (equal? 'box-set (car pe)) (equal? 'set (car pe)))))

(define annotate-tail
  (lambda (pe)
    (cond ((dont-care? pe) pe)
          ((lambda-expr? pe) (annotate-tail-lambda pe))
          ((equal? 'if3 (car pe)) `(if3 ,(annotate-tc (cadr pe)) ,(annotate-tail (caddr pe)) ,(annotate-tail (cadddr pe))))
          ((equal? 'or (car pe)) `(or (,@(car (prepare-seq-to-tail (cdadr pe) (caadr pe) #t)) 
                                       ,@(annotate-tail (cadr (prepare-seq-to-tail (cdadr pe) (caadr pe) #t))))));;;;;;
          ((definition? pe) `(,(car pe) ,(cadr pe) ,(annotate-tc (caddr pe))))
          ((set-op? pe) `(,(car pe) ,(cadr pe) ,@(annotate-tc (cddr pe))))
          ((equal? 'box (car pe)) `(box ,@(annotate-tc (cdr pe))))
          ((equal? 'applic (car pe)) `(tc-applic ,(annotate-tc (cadr pe)) ,(map annotate-tc (caddr pe))));;;;;;;;
          ((equal? 'seq (car pe)) `(seq ,`(,@(map annotate-tc (car (prepare-seq-to-tail (cdadr pe) (caadr pe) #t)))
                                                 ,@(annotate-tail (cadr (prepare-seq-to-tail (cdadr pe) (caadr pe) #t))))));;;;;
          (else `(,(annotate-tail (car pe)))));;;;;;
    ))


(define annotate-tc
  (lambda (pe)
    (cond ((dont-care? pe) pe)
          ((lambda-expr? pe) (annotate-tail-lambda pe))
          ((equal? 'if3 (car pe)) `(if3 ,(annotate-tc (cadr pe)) ,(annotate-tc (caddr pe)) ,(annotate-tc (cadddr pe))));;;;;
          ((equal? 'or (car pe)) `(or (,@(car (prepare-seq-to-tail (cdadr pe) (caadr pe) #t)) 
                                       ,@(annotate-tc (cadr (prepare-seq-to-tail (cdadr pe) (caadr pe) #t))))));;;;;;
          ((definition? pe) `(,(car pe) ,(cadr pe) ,(annotate-tc (caddr pe))))
          ((set-op? pe) `(,(car pe) ,(cadr pe) ,@(annotate-tc (cddr pe))))
          ((equal? 'box (car pe)) `(box ,@(annotate-tc (cdr pe))))
          ((equal? 'applic (car pe)) `(applic ,(annotate-tc (cadr pe)) ,(map annotate-tc (caddr pe))));;;;;;;;
          ((equal? 'seq (car pe)) `(seq ,(map annotate-tc (cadr pe))));;;
          (else `(,(annotate-tc (car pe)))));;;;;;;;;;;;
    ))   