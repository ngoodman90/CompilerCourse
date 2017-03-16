
; Yuval Har Zahav   201408143
; Noam Goodman      301364329

(load "pc.scm")
(load "pattern-matcher.scm") 
(load "qq.scm")

; ####################################### Useful Lambdas #######################################

(define flatten-list
    (lambda (head tail)
        (if (null? tail)
            head
            (flatten-list (list* head (car tail)) (cdr tail)))))

(define create-vector-ref
    (lambda (head tail)
        (if (null? tail)
            head
            (create-vector-ref (list 'vector-ref head (car tail)) (cdr tail)))))

(define create-add-sub-list
    (lambda (head tail)
        (if (null? tail)
            head
            (if (eq? (caar tail) #\+)
                (create-add-sub-list (list '+ head (cadar tail)) (cdr tail))
                (create-add-sub-list (list '- head (cadar tail)) (cdr tail))))))

(define create-mul-div-list
    (lambda (head tail)
        (if (null? tail)
            head
            (if (eq? (caar tail) #\*)
                (create-mul-div-list (list '* head (cadar tail)) (cdr tail))
                (create-mul-div-list (list '/ head (cadar tail)) (cdr tail))))))

(define create-power-list
    (lambda (head tail)
        (if (null? tail)
            head
            (list 'expt head (create-power-list (car tail) (cdr tail))))))

(define evaluateHexadecimalNumber
    (lambda (num)
        (if (null? num)
            0
            (if (eq? (cdr num) '())
                (car num)
                (+ (* (expt 16 (- (length num) 1)) (car num)) (evaluateHexadecimalNumber (cdr num)))))))

; ####################################### Useful Definitions #######################################

(define <digit-0-9> 
    (range #\0 #\9))

(define <char-a-z>
    (range #\a #\z))

(define <char-A-Z>
    (range #\A #\Z))

(define <Char-a-zA-Z>
    (new
        (*parser <char-a-z>)
        (*parser <char-A-Z>)
        (*pack (lambda (n) (integer->char (+ (char->integer n) 32))))
        (*disj 2)
    done))

(define maximalHexChar 1114112)

; ####################################### Handle Comments #######################################

(define <white-space>
    (new
        (*parser (const (lambda (ch) (char<=? ch #\space))))
    done))

(define <comment-ending>
    (new
        (*parser (char #\newline))
        (*parser <end-of-input>)
        (*disj 2)
    done))
   
(define <regular-comment>       ; until the ending of the line/file
    (new
        (*parser (char #\;))    
        
        (*parser <any-char>)
        (*parser <comment-ending>)
        *diff
        *star

        (*parser <comment-ending>)
        (*caten 3)
    done))

(define <hashtag-comment>       ; comments out Sexpr / InfixExpr
    (new
        (*parser (word "#;"))
        
        (*delayed (lambda () <InfixExpression>))
        (*delayed (lambda () <Sexpr>))
        (*disj 2)
        
        (*caten 2)
    done))

(define <ignore>
    (new
        (*parser <hashtag-comment>)
        (*parser <regular-comment>)
        (*disj 2)
        
        (*parser <white-space>)
        (*disj 2)
        
        *star
    done))

; ####################################### Useful Parsers #######################################

(define <RegularSymbolChar>
    (new
        (*parser (char #\!))
        (*parser (char #\$))
        (*parser (char #\_))
        (*parser (char #\=))
        (*parser (char #\<))
        (*parser (char #\>))
        (*parser (char #\?))
        (*disj 7)
    done))

(define <MathSymbolChar>
    (new
        (*parser (char #\^))
        (*parser (char #\*))
        (*parser (char #\/))
        (*parser (char #\+))
        (*parser (char #\-))
        (*disj 5)
    done))

(define <SpecialSymbolChar>
    (new
        (*parser <MathSymbolChar>)
        (*parser (char #\())
        (*parser (char #\)))
        (*parser (char #\[))
        (*parser (char #\]))
        (*disj 5)
    done))

; ####################################### Boolean #######################################

(define <Boolean>
    (new 
        (*parser <ignore>)
        
        (*parser (word-ci "#t"))
        (*parser (word-ci "#T"))
        (*disj 2)
        (*pack (lambda (_) #t))
        
        (*parser (word-ci "#f"))
        (*parser (word-ci "#F"))
        (*disj 2)
        (*pack (lambda (_) #f))
                
        (*disj 2)
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

; ####################################### Char #######################################

(define <CharPrefix>
    (new 
        (*parser <ignore>)

        (*parser (word-ci "#\\"))
        (*pack (lambda (_) #\\))
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <VisibleSimpleChar>
    (new
        (*parser <ignore>)
        
        (*parser <any-char>)
        (*parser (range (integer->char 0) (integer->char 32)))
        *diff
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <NamedChar>
    (new	
        (*parser <ignore>)
        
        (*parser (word "lambda"))
        (*pack (lambda (_) char (integer->char 955)))
        
        (*parser (word "newline"))
        (*pack (lambda (_) #\newline))
        
        (*parser (word "nul"))
        (*pack (lambda (_) #\nul))
        
        (*parser (word "page"))
        (*pack (lambda (_) #\page))
        
        (*parser (word "return"))
        (*pack (lambda (_) #\return))
        
        (*parser (word "space"))
        (*pack (lambda (_) #\space))

        (*parser (word "tab"))
        (*pack (lambda (_) #\tab))
        
        (*disj 7)
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <HexChar>
    (new
        (*parser <digit-0-9>)
        (*pack (lambda (c) (- (char->integer c) (char->integer #\0))))
        
        (*parser (range #\a #\f))
        (*pack (lambda (c) (+ 10 (- (char->integer c) (char->integer #\a)))))
        
        (*parser (range #\A #\F))
        (*pack (lambda (c) (+ 10 (- (char->integer c) (char->integer #\A)))))
                        
        (*disj 3)
    done))

(define <HexUnicodeChar>
    (new
        (*parser (char #\x))
        (*parser <HexChar>)
        (*parser <HexChar>) *star
        (*caten 2)
        (*pack-with
            (lambda (h t)
                (let ((num (evaluateHexadecimalNumber `(,h ,@t))))
                    (if (< num maximalHexChar)
                        num
                        '(hexadecimal number too large)))))
                                    
        (*guard (lambda (n) (number? n)))
        (*pack (lambda (n) (integer->char n)))
        
        (*caten 2)
        (*pack-with (lambda (pre c) `(,@c)))	
    done))

(define <Char>
    (new
        (*parser <CharPrefix>)
        
        (*parser <NamedChar>)  
        (*parser <HexUnicodeChar>)
        (*parser <VisibleSimpleChar>)
        (*disj 3)
        
        (*caten 2)
        (*pack-with (lambda (pre c) c))
    done))

; ####################################### Symbol #######################################

(define <SymbolChar>
    (new 
        (*parser <digit-0-9>)
        (*parser <Char-a-zA-Z>)
        (*parser <RegularSymbolChar>)
        (*parser <MathSymbolChar>)
        (*disj 4)
    done))

(define <Symbol>
    (new 
        (*parser <ignore>)
        
        (*parser <SymbolChar>) *plus
        (*pack (lambda (a) (string->symbol (list->string `(,@a)))))
                                        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

; ####################################### Number #######################################

(define <Natural>
    (new
        (*parser <digit-0-9>)
        (*parser <digit-0-9>) *star
        (*caten 2)
        (*pack-with (lambda (a s) (string->number (list->string `(,a ,@s)))))
    done))

(define <Integer>
    (new
        (*parser <ignore>)
        
        (*parser (char #\+))
        (*parser <Natural>)
        (*caten 2)
        (*pack-with
            (lambda (s n) n))               ; + Natural

        (*parser (char #\-))
        (*parser <Natural>)
        (*caten 2)
        (*pack-with
            (lambda (s n) (- n)))           ; - Natural

        (*parser <Natural>)                 ; Natural

        (*disj 3)
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <Fraction>
    (new
        (*parser <Integer>)
        (*parser (char #\/))
        (*parser <Natural>)
        (*guard (lambda (n) (not (zero? n))))
        (*caten 3)
        (*pack-with
                (lambda (num div den)
                        (/ num den)))
    done))

(define <Number>
    (new
        (*parser <Fraction>)
        (*parser <Integer>)
        (*disj 2)
    done))

(define <NumberAndSymbol>
    (new
        (*parser <digit-0-9>) *plus
        
        (*parser <Char-a-zA-Z>)
        (*parser <RegularSymbolChar>)
        (*disj 2)
        *plus
        
        (*parser <SymbolChar>) *star
        (*caten 3)
        
        (*pack-with (lambda (a b c) (string->symbol (list->string (append a b c)))))
    done))

(define <NumberAndSymbolWithoutMathSymbol>
    (new
        (*parser <digit-0-9>) *plus
        
        (*parser <Char-a-zA-Z>)
        (*parser <RegularSymbolChar>)
        (*disj 2)
        *plus
        
        (*parser <SymbolChar>)
        (*parser <MathSymbolChar>)
        *diff
        *star
        
        (*caten 3)
        (*pack-with (lambda (a b c) (string->symbol (list->string (append a b c)))))
    done))

; ####################################### String #######################################

(define <StringMetaChar>
    (new
        (*parser (word-ci "\\\\"))
        (*pack (lambda (_) #\\))
        (*parser (word "\\\""))
        (*pack (lambda (_) #\"))
        (*parser (word-ci "\\t"))
        (*pack (lambda (_) #\tab))
        (*parser (word-ci "\\f"))
        (*pack (lambda (_) #\page))
        (*parser (word-ci "\\n"))	
        (*pack (lambda (_) #\newline))
        (*parser (word-ci "\\r"))
        (*pack (lambda (_) #\return))
        (*disj 6)
    done))

(define <StringHexChar>
    (new
        (*parser (word-ci "\\x"))
        (*parser <HexChar>)
        (*parser <HexChar>) *star
        (*caten 2)
        (*caten 2)
        (*pack-with
            (lambda (a b)
                (if (eq? (cdr b) '())
                    (evaluateHexadecimalNumber b)
                    (evaluateHexadecimalNumber `(,(car b) ,@(cadr b))))))
        (*parser (char #\;))
        (*caten 2)
        (*pack-with (lambda (num s) `(,@num)))
        
        (*pack (lambda (a)
                    (if (< a maximalHexChar)
                        a
                        '(hexadecimal number too large))))
        (*guard (lambda (a) (number? a)))
        (*pack (lambda (a) (integer->char a)))
    done))

(define <StringLiteralChar>
    (new
        (*parser <any-char>)
        (*parser (char #\\))
        *diff
    done))

(define <StringChar>
    (new
        (*parser <StringHexChar>)
        (*parser <StringMetaChar>)
        (*parser <StringLiteralChar>)
        (*parser (char #\space))
        (*disj 4)
    done))

(define <String>
    (new
        (*parser (char #\"))
        
        (*parser <StringChar>) 
        (*parser (char #\"))
        *diff
        *star
        
        (*parser (char #\"))
        
        (*caten 3)
        (*pack-with (lambda (open str close) (list->string str)))
    done))

; ####################################### Math Operations & Infix Expressions #######################################

(define <PowerSymbol>
    (new		
        (*parser (char #\^))
        (*parser (word "**"))
        (*disj 2)
    done))

(define <MathOperationSymbol>
    (new		
        (*parser (char #\+))
        (*parser (char #\-))
        (*parser (char #\*))
        (*parser (char #\/))
        (*parser <PowerSymbol>)
        (*disj 5)
    done))
    
(define <MathOperationSymbolWithoutDivision>
    (new		
        (*parser <MathOperationSymbol>)
        (*parser (char #\/))
        *diff
    done))

(define <InfixPrefixExtensionPrefix>
    (new
        (*parser <ignore>)
        
        (*parser (word "##"))
        (*parser (word "#%"))
        (*disj 2)
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <InfixSymbol>
    (new 
        (*parser <ignore>)
        
        (*parser <SymbolChar>)
        (*delayed (lambda () <MathOperationSymbol>))
        *diff
        *plus
        (*pack (lambda (a) (string->symbol (list->string `(,@a)))))
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <InfixSexprEscape>
    (new		
        (*parser <InfixPrefixExtensionPrefix>)
        (*delayed (lambda () <Sexpr>))
        (*caten 2)
        (*pack-with(lambda (pre expr) expr))
    done))

(define <InfixParen>
    (new
        (*parser (char #\())
        (*delayed (lambda () <InfixExpression>))
        (*parser (char #\)))
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))                    ; ( InfixExpr )
    done))

(define <InfixParenOrNumberOrSymbol>
    (new
        (*parser <InfixParen>)
        (*parser <NumberAndSymbolWithoutMathSymbol>)
        (*parser <Number>)
        (*parser <InfixSymbol>)
        (*parser <InfixSexprEscape>)
        (*disj 5)
    done))

(define <InfixArgList>
    (new
        (*delayed (lambda () <InfixExpression>))          ; InfixExpr
        (*parser (char #\,))
        (*delayed (lambda () <InfixExpression>))
        (*caten 2)
        (*pack-with (lambda (s t) t)) *star                ; ( , InfixExpr )*
        
        (*caten 2)
        (*pack-with (lambda (h t) (list* h t)))            ; InfixExpr ( , InfixExpr )*
        
        (*parser <ignore>)
        (*pack (lambda (a) '()))
        (*disj 2)
    done))

(define <InfixFuncallBrackets>
    (new
        (*parser (char #\())
        (*delayed (lambda () <InfixArgList>))
        (*parser (char #\)))
        (*caten 3)
        (*pack-with (lambda (open content close) content))            ; ( InfixArgList )
    done))

(define <InfixFuncall>
    (new
        (*parser <InfixParenOrNumberOrSymbol>)
        (*parser <InfixFuncallBrackets>) *star
        (*caten 2)
        (*pack-with (lambda (h t) (flatten-list h t)))                ; InfixParenOrNumberOrSymbol ( (InfixFuncallBrackets) )*
    done))

(define <InfixArrayBrackets>
    (new
        (*parser (char #\[))
        (*delayed (lambda () <InfixExpression>))
        (*parser (char #\]))
        (*caten 3)
        (*pack-with (lambda (open content close) content))
    done))

(define <InfixExpressionWithoutMath>
    (new 
        (*parser <ignore>)
        
        (*parser <InfixFuncall>)
        (*parser <InfixArrayBrackets>)
        (*caten 2)
        (*pack-with (lambda (h t) (list 'vector-ref h t)))      ; InfixFuncall [ InfixExpr ]
                        
        (*parser <InfixArrayBrackets>) *star
        (*caten 2)                                              ; ( [ InfixExpr ] )*
        (*pack-with (lambda (h t) (create-vector-ref h t)))     ; InfixFuncall [ InfixExpr ] ( [ InfixExpr ] )*
        
        (*parser <InfixFuncallBrackets>) *star
        (*pack (lambda (lst) `(,@lst)))                         ; ( ( InfixArgList ) )*
        (*caten 2)
        (*pack-with (lambda (h t) (flatten-list h t)))          ; InfixFuncall [ InfixExpr ] ( [ InfixExpr ] )* ( ( InfixArgList ) )*
        
        (*parser <InfixFuncall>)                                ; InfixFuncall
        (*disj 2)                                               ; InfixFuncall [ InfixExpr ] ( [ InfixExpr ] )* ( ( InfixArgList ) )* / InfixFuncall
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <InfixPower>
    (new 		
        (*parser <InfixExpressionWithoutMath>)                ; InfixExprWithoutMath
        
        (*parser (word "**"))
        (*parser (char #\^))
        (*disj 2)
        (*parser <InfixExpressionWithoutMath>)
        (*caten 2)
        (*pack-with (lambda (s expr) expr)) *star                          ; ( ^ InfixExprWithoutMath )*
        
        (*caten 2)
        (*pack-with (lambda (h t) (create-power-list h t)))         ; InfixExprWithoutMath ( ^ InfixExprWithoutMath )*
    done))

(define <BeginingOfInfixMultiplicationOrDivision>
    (new
        (*parser <InfixPower>)
        (*parser (char #\-))
        (*parser <InfixPower>)
        (*caten 2)
        (*pack-with (lambda (s expr) (list '- expr)))
        (*disj 2)                                                 ; InfixPower / - InfixPower
    done))

(define <InfixMultiplicationOrDivision>
    (new 		
        (*parser <BeginingOfInfixMultiplicationOrDivision>)       ; InfixPower / - InfixPower

        (*parser (char #\*))
        (*parser (char #\/))
        (*disj 2)
        (*parser <BeginingOfInfixMultiplicationOrDivision>)
        (*caten 3)
        (*pack-with (lambda (h s t) (if (eq? s #\*) (list '* h t) (list '/ h t)))) ; InfixPower *// InfixPower 

        (*parser (char #\*))
        (*parser (char #\/))
        (*disj 2)
        (*parser <InfixPower>)
        (*caten 2) *star				                    ; ( *// InfixMulDiv )*

        (*caten 2)
        (*pack-with (lambda (h t) (create-mul-div-list h t)))

        (*parser <BeginingOfInfixMultiplicationOrDivision>)         ; InfixPower / - InfixPower
        (*disj 2)
    done))

(define <BeginingOfInfixAdditionOrSubstitution>
    (new
        (*parser <InfixMultiplicationOrDivision>)
        
        (*parser (char #\-))
        (*parser <InfixMultiplicationOrDivision>)
        (*caten 2)
        
        (*pack-with (lambda (s expr) (list '- expr)))
        (*disj 2)                                                         ; InfixMulDiv / - InfixMulDiv
    done))

(define  <InfixAdditionOrSubstitution>
    (new
        (*parser <BeginingOfInfixAdditionOrSubstitution>)                 ; InfixMulDiv / - InfixMulDiv
        
        (*parser (char #\+))
        (*parser (char #\-))
        (*disj 2)
        (*parser <InfixMultiplicationOrDivision>)
        (*caten 3)
        (*pack-with (lambda (h s t) (if (eq? s #\+) (list '+ h t) (list '- h t))))  ; InfixMulDiv +/- InfixMulDiv
        
        (*parser (char #\+))
        (*parser (char #\-))
        (*disj 2)
        (*parser <InfixMultiplicationOrDivision>)
        (*caten 2) *star                                                  ; ( +/- InfixMulDiv )*
        
        (*caten 2)
        (*pack-with (lambda (h t) (create-add-sub-list h t)))             ; InfixMulDiv +/- InfixMulDiv ( +/- InfixMulDiv )*
        
        (*parser <BeginingOfInfixAdditionOrSubstitution>)                 ; InfixMulDiv / - InfixMulDiv
        (*disj 2)
    done))

(define <InfixExpression> <InfixAdditionOrSubstitution>)

(define <InfixExtension>
    (new
        (*parser <ignore>)
        
        (*parser <InfixPrefixExtensionPrefix>)
        (*parser <InfixExpression>)
        (*caten 2)
        (*pack-with (lambda (pre expr) expr))
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

; ####################################### Sexpr #######################################

(define <Sexpr>
    (new
        (*parser <ignore>)
        
        (*parser <Boolean>)
        
        (*parser <Char>)
        (*parser <Symbol>) *not-followed-by
        
        (*parser <NumberAndSymbol>)
        
        (*parser <Number>)
        (*parser <MathOperationSymbolWithoutDivision>) *not-followed-by
        
        (*parser <String>)
        (*parser <Symbol>)
        (*parser <InfixExtension>)
        
        (*delayed (lambda () <ImproperList>))
        (*delayed (lambda () <ProperList>))
        (*delayed (lambda () <Vector>))
        (*delayed (lambda () <Quoted>))
        (*delayed (lambda () <QuasiQuoted>))
        (*delayed (lambda () <Unquated>))
        (*delayed (lambda () <UnquatedAndSpliced>))
        (*disj 14)
        
        (*parser <ignore>)
        (*caten 3)
        (*pack-with (lambda (ignr1 content ignr2) content))
    done))

(define <sexpr> <Sexpr>)

; ####################################### Lists & Vectors #######################################

(define <ImproperList>
    (new
        (*parser (char #\())
        (*parser <Sexpr>) *plus
        (*parser (char #\.))
        (*parser <Sexpr>)
        (*parser (char #\)))
        
        (*caten 5)
        (*pack-with (lambda (open expr1 dot expr2 close) `(,@expr1 ,@expr2)))
    done))

(define <ProperList>
    (new
        (*parser (char #\())
        (*parser <Sexpr>) *star
        (*parser (char #\)))
        
        (*caten 3)
        (*pack-with (lambda (open expr close) `(,@expr)))
    done))

(define <Vector>
    (new
        (*parser (char #\#))
        (*parser <ProperList>)
        
        (*caten 2)
        (*pack-with (lambda (s lst) (list->vector lst)))
    done))

(define <Quoted>
    (new
        (*parser (char #\'))
        (*parser <Sexpr>)
        
        (*caten 2)
        (*pack-with (lambda (s expr) (list 'quote expr)))
    done))

(define <QuasiQuoted>
    (new
        (*parser (char #\`))
        (*parser <Sexpr>)
        
        (*caten 2)
        (*pack-with (lambda (s expr) (list 'quasiquote expr)))
    done))

(define <Unquated>
    (new
        (*parser (char #\,))
        (*parser <Sexpr>)
        
        (*caten 2)
        (*pack-with (lambda (s expr) (list 'unquote expr)))
    done))

(define <UnquatedAndSpliced>
    (new
        (*parser (word ",@"))
        (*parser <Sexpr>)
        
        (*caten 2)
        (*pack-with (lambda (s expr) (list 'unquote-splicing expr)))
    done))

; ####################################### Assignment 2 #######################################

(define error "ERROR")

(define throw-error
    (lambda ()
        error))
        
; ####################################### Constants #######################################

(define constant?
    (lambda (c)
	(or (number? c) (char? c) (boolean? c) (string? c) (eq? c (void)))))

(define const-record
    (compose-patterns
	(pattern-rule 
            (? 'expr constant?)
            (lambda (expr) (list 'const expr)))
            
        (pattern-rule 
	    `(quote ,(? 'expr))
            (lambda (expr) (list 'const expr)))))

            
; ####################################### Variables #######################################

(define *reserved-words*
    '(begin define
    cond if else and or
    do lambda let let* letrec
    quote unquote quasiquote unquote-splicing
    set!))

(define unreserved-word?
    (lambda (w)
        (not (member w *reserved-words*))))

(define variable?
    (lambda (w)
	(and (symbol? w) (unreserved-word? w))))

(define variable-record
    (pattern-rule 
        (? 'expr variable?)
        (lambda (expr) (list 'var expr))))

; ####################################### Conditionals #######################################

(define do-parse
    (lambda (expr) (parse expr)))

(define condition-record
    (compose-patterns
        (pattern-rule
            '(if)
            (throw-error))
            
        (pattern-rule
            '(if ,(? 'test))
            (lambda (test) 
                error))
		   
	(pattern-rule
            `(if ,(? 'test) ,(? 'dit))
            (lambda (test dit) 
                `(if3 ,(do-parse test) ,(do-parse dit) ,(do-parse (void)))))
        
        (pattern-rule
            `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
            (lambda (test dit dif) 
                `(if3 ,(do-parse test) ,(do-parse dit) ,(do-parse dif))))))
            
; ####################################### Disjunctions #######################################
	
(define parse-list
    (lambda (lst)
        (if (null? lst)
            '()
            (if (null? (cdr lst))
                (list (do-parse (car lst)))
                `(,(do-parse (car lst)) ,@(parse-list (cdr lst)))))))
		
(define or-record
    (compose-patterns
        (pattern-rule 
            `(or)
            (lambda () 
                (do-parse #f)))
	
	(pattern-rule 
            `(or ,(? 'expr))
            (lambda (expr) 
                (do-parse expr)))
   
	(pattern-rule 
            `(or ,(? 'expr) . ,(? 'expr-lst))
            (lambda (expr expr-lst) 
                `(or (,(do-parse expr) ,@(parse-list expr-lst)))))))

; ####################################### Lambda Forms #######################################
                
(define is-member-of-list?
    (lambda (m lst)
        (if (not (pair? lst))
            (eq? m lst)
            (if (not (eq? m (car lst)))
                (is-member-of-list? m (cdr lst))
                #t))))

(define duplicate-variables?
    (lambda (lst)
        (if (not (pair? lst))
            #f
            (or (is-member-of-list? (car lst) (cdr lst))
                (duplicate-variables? (cdr lst))))))

(define regular-lambda?
    (lambda (lst)
        (and (list? lst) (not (duplicate-variables? lst)))))
        
(define lambda-with-optional-arguements?
    (lambda (pair)
        (and (pair? pair) (not (duplicate-variables? pair)))))
		
(define get-mandatory-variables
    (lambda (pair)
        (if (pair? pair)
            `(,(car pair) ,@(get-mandatory-variables (cdr pair)))
            '())))
		
(define get-v-rest
    (lambda (pair)
        (cdr (last-pair pair))))
		
(define variadic-lambda?
    (lambda (lst)
        (if (variable? lst)
            #t
            #f)))
    
(define lambda-record
    (compose-patterns
        (pattern-rule                                                                  ; error lambda
            '(lambda)
            (throw-error))
            
        (pattern-rule                                                                  ; error lambda
            '(lambda . ,(? 'expr-lst))
            (lambda (expr-lst) error))
            
	(pattern-rule                                                                  ; lambda-simple
            `(lambda ,(? 'vars regular-lambda?) ,(? 'expr) . ,(? 'expr-lst))
            (lambda (vars expr expr-lst)
                `(lambda-simple ,vars ,(do-parse `(begin ,expr ,@expr-lst)))))
		   
	(pattern-rule                                                                  ; lambda-opt
            `(lambda ,(? 'vars lambda-with-optional-arguements?) ,(? 'expr) . ,(? 'expr-lst))
            (lambda (vars expr expr-lst)
                `(lambda-opt ,(get-mandatory-variables vars) ,(get-v-rest vars) ,(do-parse `(begin ,expr ,@expr-lst)))))

	(pattern-rule                                                                  ; lambda-var
            `(lambda ,(? 'vars variadic-lambda?) ,(? 'expr) . ,(? 'expr-lst))
            (lambda (vars expr expr-lst)
                `(lambda-var ,vars ,(do-parse `(begin ,expr ,@expr-lst)))))))
		
; ####################################### Define #######################################
		
(define define-record
    (compose-patterns
	(pattern-rule                                                                  ; Regular Define
            `(define ,(? 'var variable?) . ,(? 'expr))
            (lambda (var expr)
                `(def ,(do-parse var) ,(do-parse (cons 'begin expr)))))
	
	(pattern-rule                                                                  ; MIT Define
            `(define ,(? 'var pair?) . ,(? 'expr))
            (lambda (var expr) 
                    `(def ,(do-parse (car var)) ,(do-parse `(lambda ,(cdr var) ,(cons 'begin expr))))))))
		   
; ####################################### Assignments #######################################

(define set-record
    (pattern-rule 
        `(set! ,(? 'var variable?) ,(? 'expr))
        (lambda (var expr)
            `(set ,(do-parse var) ,(do-parse expr)))))

; ####################################### Applications #######################################

(define applic-record
    (pattern-rule
        `(,(? 'expr unreserved-word?) . ,(? 'expr-lst))
	(lambda (expr expr-lst)
            `(applic ,(do-parse expr) ,(parse-list expr-lst)))))

; ####################################### Sequences #######################################

(define empty-or-single-member-or-no-list?
    (lambda (lst)
        (or (not (list? lst)) (null? lst) (and (not (list? (car lst))) (null? (cdr lst))))))

(define organize-begin-list
    (lambda (lst)
        (if (empty-or-single-member-or-no-list? lst)
            (if (not (list? lst))
                (list lst)                                                                       ; make list
                (if (or (null? lst) (eq? 'begin (car lst))) '() lst))                            ; empty list or single member list
            (if (not (list? (car lst)))
                (if (eq? (car lst) 'begin)
                    (organize-begin-list (cdr lst))                                              ; ( begin ... )
                    (append (list (car lst)) (organize-begin-list (cdr lst))))                   ; ( ... )
                (if (eq? (caar lst) 'begin)
                    (append (organize-begin-list (cdar lst)) (organize-begin-list (cdr lst)))     ; ( ( begin ... ) ... )
                    (cons (car lst) (organize-begin-list (cdr lst))))))))                         ; ( ( ... ) ... )
            
(define seq-record
    (compose-patterns  
	(pattern-rule 
            `(begin)
            (lambda ()
                (do-parse (void))))

	(pattern-rule 
            `(begin ,(? 'expr))
	     (lambda (expr)
                (do-parse expr)))
		  
	(pattern-rule
            `(begin ,(? 'expr) . ,(? 'expr-lst))
            (lambda (expr expr-lst)
                (let ((new-expr-lst (organize-begin-list expr-lst)))
                    (if (not (null? new-expr-lst))
                        `(seq (,(do-parse expr) ,@(parse-list new-expr-lst)))
                        (do-parse expr)))))))

; ####################################### Quasi Quote #######################################
                    
(define quasi-quote-record
    (compose-patterns
        (pattern-rule
            `(,(string->symbol "quasiquote") ,(? 'qq))
            (lambda (qq)
                (parse (expand-qq qq))))))
		    
; ####################################### And #######################################
		    
(define and-macro-expander
    (compose-patterns	
	(pattern-rule 
            `(and)
            (lambda () 
                (do-parse #t)))
                
        (pattern-rule 
            `(and ,(? 'expr))
            (lambda (expr) 
                (do-parse expr)))
                
        (pattern-rule 
	  `(and ,(? 'expr1) ,(? 'expr2))
	      (lambda (expr1 expr2) 
		 (do-parse `(if ,expr1 ,expr2 #f))))
        
        (pattern-rule 
	  `(and ,(? 'expr1) . ,(? 'expr2))
	      (lambda (expr1 expr2)
		(do-parse `(if ,expr1 (and ,(car expr2) ,@(cdr expr2)) #f))))))

; ####################################### Let #######################################

(define let-variables
    (lambda (lst)
        (if (null? lst)
            '()
            `(,(caar lst) ,@(let-variables(cdr lst))))))
	  
(define let-values
    (lambda (lst)
        (if (null? lst)
            '()
            `(,(cadar lst) ,@(let-values(cdr lst))))))
            
(define let-macro-expander
    (compose-patterns
        (pattern-rule
            '(let)
            (throw-error))
            
        (pattern-rule
            '(let ,(? 'arguements))
            (lambda (arguements)
                error))
            
        (pattern-rule
            `(let ,(? 'arguements) ,(? 'body) . ,(? 'next-body))
            (lambda (arguements body next-body)
                (let ((vars (let-variables arguements))
                    (vals (let-values arguements)))
		    (if (not (duplicate-variables? vars))
                        ((lambda () (parse `((lambda ,vars ,body ,@next-body) ,@vals))))
                        error))))))

; ####################################### Letrec #######################################
            
(define pair-up-vars-and-vals
    (lambda (vars vals)
        (if(null? vars)
            '()
            `((set! ,(car vars) ,(car vals)) ,@(pair-up-vars-and-vals (cdr vars) (cdr vals))))))

(define init-vals-to-false
    (lambda (vals)
	(if (null? vals)
            vals
            `(#f ,@(init-vals-to-false (cdr vals))))))
	
(define letrec-macro-expander
    (compose-patterns
        (pattern-rule
            '(letrec)
            (throw-error))
            
        (pattern-rule
            '(letrec ,(? 'arguements))
            (lambda (arguements)
                error))
            
        (pattern-rule
            `(letrec ,(? 'arguements) ,(? 'body) . ,(? 'next-body))
            (lambda (arguements body next-body)
                (let ((vars (let-variables arguements))
                    (vals (let-values arguements)))
		    (let ((vars-and-vals (pair-up-vars-and-vals vars vals))
                        (vals (init-vals-to-false vals)))
                        (if (not (duplicate-variables? vars))
                            ((lambda () (parse `((lambda ,vars (begin ,@vars-and-vals ((lambda () ,body ,@next-body)))) ,@vals))))
                            error)))))))

; ####################################### Let* #######################################
            
(define let-star-macro-expander
    (compose-patterns
        (pattern-rule
            '(let*)
            (throw-error))
		
        (pattern-rule
            '(let* ,(? 'arguements))
            (lambda (arguements)
                error))
		
        (pattern-rule
            `(let* ,(? 'arguements) ,(? 'body) . ,(? 'next-body))
            (lambda (arguements body next-body)
		(if(> (length arguements) 1)
                    (do-parse `(let ,(list (car arguements)) (let* ,(cdr arguements) ,body ,@next-body)))
                    (do-parse `(let ,arguements ,body ,@next-body)))))))
                
; ####################################### Cond #######################################

(define append-action
    (lambda (action)
        (append '(begin) action)))

(define cond-macro-expander
    (compose-patterns	
	(pattern-rule 
            `(cond)
	    (throw-error))
			
        (pattern-rule
            `(cond (,(? 'condition) . ,(? 'action)))
	    (lambda (condition action)
                (if (list? action)
                    (if (eq? 'else condition)
                        (parse `,(append-action action))
                        (parse `(if ,condition ,(append-action action))))
                    (if (eq? 'else condition)
                        (parse `(action))
                        (parse `(if ,condition ,action))))))

	(pattern-rule
            `(cond (,(? 'condition) . ,(? 'action)) . ,(? 'next-condition))
            (lambda (condition action next-condition)
                (if (list? action)
                    (do-parse `(if ,condition ,(append-action action) (cond ,@next-condition)))
                    (do-parse `(if ,condition ,action (cond ,@next-condition))))))))

; ####################################### Parse #######################################

(define parse
    (lambda (sexpr)
        (let ((expr (compose-patterns	
                     const-record
                     variable-record
                     condition-record
                     or-record
                     lambda-record
                     define-record
                     set-record
                     applic-record
                     seq-record
                     and-macro-expander
                     let-macro-expander
                     letrec-macro-expander
                     let-star-macro-expander
                     cond-macro-expander
                     quasi-quote-record)))
             (expr sexpr (throw-error)))))
            
(define tag-parser parse)
									
									
; ####################################### Assignment 3 #######################################
       
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

(define lambda-simple-body
  (lambda (expr)
    (caddr expr)))
	
(define lambda-opt-body
  (lambda (expr)
    (cadddr expr)))

(define lambda-var-body
  (lambda (expr)
    (caddr expr)))

(define lambda-simple-parameters
  (lambda (expr)
    (cadr expr)))
    
(define lambda-opt-parameters
  (lambda (expr)
   (append (cadr expr) (list (caddr expr)))))
   
(define lambda-var-parameters
  (lambda (expr)
    (list (cadr expr))))
    
; ####################################### Eliminate Nested Defines #######################################

(define create_set_expressions
  (lambda (vars vals lst)
    (if (null? vars)
        lst
        (create_set_expressions (cdr vars) (cdr vals) (append lst (list `(set (var ,(car vars)) ,@(car vals))))))))
               
(define init_to_false 
  (lambda (vars)
    (map (lambda (x) `(const #f)) vars)))

(define transform_letrec_to_lambda 
  (lambda (expr)
    (let* ((vars_and_vals (cadr expr))
            (body  (cddr expr))
            (vars (map car vars_and_vals))
            (vars_init (init_to_false vars))
            (vals (map cdr vars_and_vals))
            (set_seq `(seq (,@(create_set_expressions vars vals '()) ,@body)))
            (lambda_expss `(lambda-simple ,vars ,set_seq))
            (applic_exp `(applic ,lambda_expss ,vars_init)))
        applic_exp)))

(define e-n-d-1
  (lambda (pes ret)
      (if (null? pes)
        (ret '() '())
        (e-n-d-1 (cdr pes)
            (lambda (ds es)
                    (cond ((eq? (caar pes) 'def)
                            (ret (cons (car pes) ds ) es))
                            ((eq? (caar pes) 'seq)
                                (e-n-d-1 (cadar pes)
                                (lambda (ds1 es1 ) (ret
                                (append ds1 ds)
                                (append es1 es)))))
                            (else  (ret ds (cons (car pes) es)))))))))

(define specialmap 
  (lambda (func lst)
    (if (null? lst)
        '()
        (cons (func (car lst)) (specialmap func (cdr lst))))))

(define e-n-d-2
  (lambda (lst)
    (e-n-d-3 (e-n-d-1 lst
        (lambda (x y)
            (if (null? x) 
                lst
                `(,(transform_letrec_to_lambda `(letrec ,(specialmap (lambda (i)  `(,(cadr (cadr i)) ,(caddr i))) x) ,@y)))))))))

(define e-n-d-3
  (lambda (lst)     
    (specialmap
        (lambda (x)
            (cond ((or (null? x) (not (list? x))) x)
                ((or (lambda-simple? x) (lambda-var? x)) `(,(car x) ,(cadr x) ,@(e-n-d-2 (cddr x))))
                ((lambda-opt? x) `(,(car x) ,(cadr x) ,(caddr x) ,@(e-n-d-2 (cdddr x))))
                (else (e-n-d-3 x))))
            lst)))

(define eliminate-nested-defines
  (lambda (lst) 
    (car (e-n-d-3 `(,lst)))))

; ####################################### Boxing of Variables #######################################

(define set?
  (lambda (expr var)
    (equal? var (cadr expr))))
 
(define var_set?
  (lambda (body var)
    (ormap
        (lambda (x)
            (if (and (not (null? x)) (list? x))
                (if (and (eq? 'set (car x)) (set? x var))
                    #t
                    (var_set?  x var ))
                #f))
        (car (list body)))))

(define not_member?
  (lambda (x list)
     (if (null? list)
        #t                                
        (if (equal? x (car list))
            #f                  
            (not_member? x (cdr list))))))

(define variable_in_parameters?
  (lambda (lst-lambda var)
    (cond ((equal? (car lst-lambda) 'lambda-simple)  (not_member? (cadr var) (lambda-simple-parameters lst-lambda)))      
        ((equal? (car lst-lambda) 'lambda-var)  (not_member? (cadr var) (lambda-var-parameters lst-lambda)))
        ((equal? (car lst-lambda) 'lambda-opt)  (not_member? (cadr var) (lambda-opt-parameters lst-lambda))))))

(define transform_to_box-get
  (lambda (body var)
    (map
        (lambda (x)
                (if (and (not (null? x)) (list? x))
                    (if (and (lambda-expr? x) (not (variable_in_parameters? x var)))
                        x
                        (if (equal? (car x) 'box-set)
                            `(box-set ,(cadr x) ,@(transform_to_box-get `(,(caddr x)) var ))       ; 
                            (if (equal? var x)
                                `(box-get ,x)
                                (transform_to_box-get x var))))
                    x))
        body)))

(define transform_to_box-set
  (lambda (body var)
    (map
        (lambda (x)
            (if (and (not (null? x)) (list? x))
                (if (and (lambda-expr? x) (not (variable_in_parameters? x var)))
                    x
                    (if (and (equal? (car x) 'set) (equal? var (cadr x)))
                        `(box-set ,(cadr x) ,(transform_to_box-set (caddr x) var))  
                        (transform_to_box-set x var)))
                x))
        body)))
        
(define bound?
  (lambda (body var)       
    (ormap
        (lambda (x)
            (if  (or (null? x) (not (list? x)))
                #f
                (if (and (equal? x var))
                    #t 
                    (if  (lambda-expr? x)
                        (if (not (variable_in_parameters? x var))
                            #f
                            (bound? x var))
                        (bound? x var)))))
        body)))

(define variable_appears?
  (lambda (body var)
    (ormap
        (lambda (x)
            (if (and (not (null? x)) (list? x))
                (if (equal? var x)
                    #t
                    (if (eq? 'set (car x))   
                        (variable_appears? (cddr x) var)
                        (if (and (lambda-expr? x) (not (variable_in_parameters? x var)))
                            #f
                            (variable_appears? x var))))
                #f))
        (car (list body)))))

(define get_lambda 
  (lambda (lambdas params body)
    (cond ((or (not (list? params)) (null? params)) lambdas) 
        ((and (bound? body `(var ,(car params)))  (var_set? body `(var ,(car params))) (variable_appears? body `(var ,(car params))))
            (get_lambda (transform_to_box-get (transform_to_box-set lambdas `(var ,(car params))) `(var ,(car params)))  (cdr params) body))
        (else (get_lambda lambdas (cdr params) body)))))

(define get_lambda_parameters
  (lambda (lambdas)
    (cond ((eq? (car lambdas) 'lambda-simple)
            (lambda-simple-parameters lambdas))
        ((eq? (car lambdas) 'lambda-opt)
            (lambda-opt-parameters lambdas))
        ((eq? (car lambdas) 'lambda-var)
            (lambda-var-parameters lambdas)))))
            
(define get_lambda_body
  (lambda (lambdas)
    (cond ((eq? (car lambdas) 'lambda-simple)
            (lambda-simple-body lambdas))
        ((eq? (car lambdas) 'lambda-opt)
            (lambda-opt-body lambdas))
        ((eq? (car lambdas) 'lambda-var)
            (lambda-var-body lambdas)))))
        
(define b-s-1
  (lambda (lambdas)
    (let* ((params (get_lambda_parameters lambdas))
        (body (get_lambda_body lambdas))
        (lmbd (get_lambda lambdas params body))
        (lmbd_body (get_lambda_body lmbd))
        (sets_to_add_after_lambda
            (filter
                (lambda (t)
                    (not (null? t)))
                (specialmap 
                    (lambda (p)
                        (if (and  (bound? body `(var ,p))  (var_set? body `(var ,p)) (variable_appears? body `(var ,p))) 
                            `(set (var ,p) (box (var ,p)))
                            '()))
                params))))
        ((lambda (x) 
            (cond ((lambda-var? x) `(,(car x) ,@params ,(caddr x)))
                ((lambda-opt? x) `(,(car x) ,(reverse (cdr (reverse params))) ,(car (reverse params)) ,(caddr x)))
            ((lambda-simple? x) `( ,(car x) ,params ,(caddr x)))))
        (if (<= (length sets_to_add_after_lambda) 0)
            `(,(car lmbd) ,params ,lmbd_body)
            (if (equal? (car lmbd_body) 'seq)
                `( ,(car lmbd) ,params (seq (,@sets_to_add_after_lambda ,@(cadr lmbd_body))))
                `(,(car lmbd) ,params (seq (,@sets_to_add_after_lambda ,lmbd_body)))))))))

(define b-s-2
  (lambda (y)
    (map
        (lambda (x)
            (if (or (null? x) (not (list? x)))
                x
                (if (lambda-expr? x) 
                    (b-s-2 (b-s-1 x)) 
                    (b-s-2 x))))
        y)))
   
(define box-set
  (lambda (y)
    (car (b-s-2 `(,y)))))
	      
; ####################################### Removing Redundant Applications #######################################
    
(define null-or-not-pair?
  (lambda (expr)
    (or (null? expr) (not (pair? expr)))))
    
(define r-a-e-1
  (lambda (expr)
    (and (pair? expr) (lambda-simple? expr) (= 3 (length expr)) (null? (cadr expr)))))

(define r-a-e-2
  (lambda (expr)
    (and 
    	(equal? 'applic (car expr)) 
    	(r-a-e-1 (cadr expr)))
    ))

(define remove-applic-lambda-nil
  (lambda (expr)
    (cond ((null-or-not-pair? expr) expr)
          ((r-a-e-2 expr) (remove-applic-lambda-nil (caddr (cadr expr))))
          (else `(,(remove-applic-lambda-nil (car expr)) ,@(remove-applic-lambda-nil (cdr expr)))))))

; ####################################### Annotating Variables With Their Lexical Address #######################################

(define change_pvar
  (lambda (lambdas var lvl place)
    (specialmap
        (lambda (x)
            (if (and (not (null? x)) (list? x))
                (if (lambda-expr? x ) (change_pvar x var (+ 1 lvl) place)
                    (if (and (= 0 lvl) (equal? x var) )
                        `(pvar ,(cadr var) ,place)
                        (if (and (not (= 0 lvl)) (equal? x var) )
                            `(bvar ,(cadr var) ,(- lvl 1) ,place)
                            (change_pvar x var lvl place))))
                x))
        lambdas)))

(define p->l-p-1
  (lambda (lambdas parms number)
    (if (null? parms)
        lambdas
        (if (equal? void parms)
            (p->l-p-1 (change_pvar lambdas `(var ,(car parms)) 0 number) '() (+ 1 number))
            (p->l-p-1 (change_pvar lambdas `(var ,(car parms)) 0 number) (cdr parms) (+ 1 number))))))

(define p->l-p-2
      (lambda (lambdas)
	  (let* ((parama1 (cond ((eq? (car lambdas) 'lambda-simple) (lambda-simple-parameters lambdas))
                    ((eq? (car lambdas) 'lambda-opt) (lambda-opt-parameters lambdas))
                    ((eq? (car lambdas) 'lambda-var) (lambda-var-parameters lambdas)))))
                (p->l-p-1  lambdas parama1 0))))
      
(define null-or-not-list?
  (lambda (x)
    (or (null? x) (not (list? x)))))
      
(define transform_var_2_fvar
  (lambda (x)
     (specialmap (lambda (y)
        (if (null-or-not-list? y)
            y
            (if (equal? (car y) 'var) 
                `(fvar ,(cadr y))
                (transform_var_2_fvar y))))
        x)))
      
(define p->l-p-3
  (lambda (x)
    (specialmap
        (lambda (y)
            (if (null-or-not-list? y)
                y
                (if (lambda-expr? y) 
                    (p->l-p-2 (p->l-p-3 y))
                    (p->l-p-3 y))))
        x)))
	      
(define pe->lex-pe
  (lambda (x)
    (car (transform_var_2_fvar(p->l-p-3 `(,x))))))
    
; ####################################### Annotating Tail Calls #######################################

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

; ####################################### Assignment 3 parser #######################################

(define ass3-parser
    (lambda (sexpr)
        (annotate-tc
            (pe->lex-pe
                (box-set
                    (remove-applic-lambda-nil
                        (eliminate-nested-defines (parse sexpr))))))))

; ####################################### Final Project #######################################


;######################################## Helper Functions ####################################

(define new-line (list->string (list #\newline)))

(define add-line-to-code
  (lambda (line)
      (string-append line ";" (string #\newline))))

(define add-label-to-code
  (lambda (label)
    (string-append label ":" (string #\newline))))

(define add-to-code
	(lambda (line)
		(string-append line (string #\newline))))


; Arithmetic instruction


(define ADD
  (lambda (dest src)
    (string-append "ADD(" dest "," src ")")))

(define DECR
  (lambda (dest)
    (string-append "DECR(" dest ")")))

(define DIV
  (lambda (dest src)
    (string-append "DIV(" dest "," src ")")))

(define INCR
  (lambda (dest)
    (string-append "INCR(" dest ")")))

(define MUL
  (lambda (dest src)
    (string-append "MUL(" dest "," src ")")))

(define REM
  (lambda (dest src)
    (string-append "REM(" dest "," src ")")))

(define SUB
  (lambda (dest src)
    (string-append "SUB(" dest "," src ")")))


; Logical instructions


(define AND
  (lambda (dest src)
    (string-append "AND(" dest "," src ")")))

(define NEG
  (lambda (dest)
    (string-append "NEG(" dest ")")))

(define OR
  (lambda (dest src)
    (string-append "OR(" dest "," src ")")))

(define SHL
  (lambda (dest src)
    (string-append "SHL(" dest "," src ")")))

(define SHR
  (lambda (dest src)
    (string-append "SHR(" dest "," src ")")))

(define XOR
  (lambda (dest src)
    (string-append "XOR(" dest "," src ")")))


; Stack


(define DROP
  (lambda (count)
    (string-append "DROP(" count ")")))

(define POP
  (lambda (dest)
    (string-append "POP(" dest ")")))

(define PUSH
  (lambda (src)
    (string-append "PUSH(" src ")")))


; Comparison


(define CMP
  (lambda (op1 op2)
    (string-append "CMP(" op1 "," op2 ")")))


; Control

(define CALL
  (lambda (dest)
    (string-append "CALL(" dest ")")))

(define CALLA
  (lambda (addr)
    (string-append "CALLA(" addr ")")))

(define JUMP
  (lambda (dest)
    (string-append "JUMP(" dest ")")))

(define JUMPA
  (lambda (dest)
    (string-append "JUMPA(" dest ")")))

(define JUMP_EQ
  (lambda (dest)
    (string-append "JUMP_EQ(" dest ")")))

(define JUMP_GE
  (lambda (dest)
    (string-append "JUMP_GE(" dest ")")))

(define JUMP_GT
  (lambda (dest)
    (string-append "JUMP_GT(" dest ")")))

(define JUMP_LE
  (lambda (dest)
    (string-append "JUMP_LE(" dest ")")))

(define JUMP_LT
  (lambda (dest)
    (string-append "JUMP_LT(" dest ")")))

(define JUMP_NE
  (lambda (dest)
    (string-append "JUMP_NE(" dest ")")))


; IO


(define IN
  (lambda (dest port)
    (string-append "IN(" dest "," port ")")))

(define OUT
  (lambda (port src)
    (string-append "OUT(" port "," src ")")))


; Register operations


(define MOV
  (lambda (dest src)
    (string-append "MOV(" dest "," src ")")))


; Addressing modes


(define IMM
  (lambda (const)
    (string-append "IMM(" const ")")))

(define IND
  (lambda (src)
    (string-append "IND(" src ")")))

(define INDD
  (lambda (src const)
    (string-append "INDD(" src "," const ")")))

(define STACK
  (lambda (n)
    (string-append "STACK(" n ")")))

(define STARG
  (lambda (n)
    (string-append "STARG(" n ")")))

(define FPARG
  (lambda (n)
    (string-append "FPARG(" n ")")))





(define get-last-element
  (lambda (lst)
    (car (reverse lst))))

(define remove-last-element
  (lambda (lst)
    (reverse (cdr (reverse lst)))))
 
(define test-s
  (lambda (parser str) 
    (letrec
        ((test-str
            (lambda (parser str cont)
                (parser (string->list str)
                    (lambda (e s) 
                        (if (null? s) 
                            (append  cont (list e))
                            (test-str parser (list->string s) (append cont (list e)))))
                    (lambda (w) `(failed with report: ,@w))))))
        (test-str parser str '()))))
			      
(define file->string
    (lambda (in-file)
        (let ((in-port (open-input-file in-file)))
            (letrec ((run
                (lambda ()
                    (let ((ch (read-char in-port)))
                        (if (eof-object? ch)
                            (begin
                                (close-input-port in-port)
                                '())
                            (cons ch (run)))))))
                    (list->string (run))))))

; extracts the consts or fvars (depends on the label specified) from the list of parsed expressions 
(define get-consts-or-fvars
  (lambda (tag)
    (letrec ((foo
        (lambda (pe)
            (cond
                ((atom? pe) '())
                ((null? pe) '())
                ((eq? (car pe) tag) (list pe))
                (else (append (foo (car pe)) (foo (cdr pe))))))))
    foo)))
           

(define get-consts (get-consts-or-fvars 'const))

(define get-fvars (get-consts-or-fvars 'fvar))

(define remove-duplicates
  (lambda (lst)
    (letrec ((rem
              (lambda (lst)
                (cond
                 ((null? lst) '())
                 ((member (car lst) (cdr lst))
                  (rem (cdr lst)))
                 (else (cons (car lst) (rem (cdr lst)))))
                 )))
      (reverse (rem (reverse lst))))))
      
(define void?
  (lambda (expr)
    (eq? void-object expr)))

; in order for multiple consts to point on the same value
(define topo-sort-consts
  (lambda (expr)
    (cond
        ((or  (null? expr) (boolean? expr)) `(,expr))
        ((or (number? expr) (string? expr) (void? expr)) `(,expr))
        ((pair? expr) `(,@(topo-sort-consts (car expr)) ,@(topo-sort-consts (cdr expr)) ,expr))
        ((vector? expr) `(,@(apply append
            (map topo-sort-consts
                (vector->list expr))) ,expr))
        ((symbol? expr) `(,@(topo-sort-consts (symbol->string expr)) ,expr))
        ((char? expr) `(,expr))
        (else `(,expr)))))

(define organize-fvars
  (lambda (fvars)
    (remove-duplicates (apply append (map cdr fvars)))))

(define organize-consts 
  (lambda (consts)
    (remove-duplicates
        (apply append
            (map (lambda (const)
                (remove-duplicates (topo-sort-consts (cadr const))))
            consts)))))
        
(define Get-associate-i
  (lambda (key lst column)
    (get-n-element key lst (- column 1))))
                            
(define constants-table
  (lambda (constants_list accumalte_lst addr last_address)
    (cond
        ((null? constants_list) (reverse accumalte_lst))
        (else 
            (let ((current (car constants_list)))
                (cond
                    ((and (number? current) (integer? current))
                     (constants-table (cdr constants_list)
                                        (cons  `(,addr ,current (\T_INTEGER ,current)) accumalte_lst)
                                        (+ addr 2)
                                        last_address))
                    ((and (number? current) (not (integer? current)))
                     (let ((numerat (numerator current)) (denom (denominator current)))
                          (constants-table (cdr constants_list)
                                    (cons  `(,addr ,current (\T_FRACTION ,numerat ,denom  )) accumalte_lst)
                                    (+ addr 3)
                                    last_address)) )              
                    ((string? current)
                     (let ((asci_chr (map char->integer (string->list current))))
                          (constants-table (cdr constants_list)
                                    (cons `(,addr ,current (\T_STRING ,(string-length current) ,@asci_chr)) accumalte_lst)
                                    (+ addr (+ (string-length current) 2))
                                    last_address)))
                    ((pair? current)
                     (let ((addr-car (car (Get-associate-i (car current) accumalte_lst 2)))
                                    (addr-cdr (car (Get-associate-i (cdr current) accumalte_lst 2))))
                          (constants-table (cdr constants_list)
                                    (cons `(,addr ,current (\T_PAIR ,addr-car ,addr-cdr)) accumalte_lst)
                                    (+ addr 3)
                                    last_address)))
                    ((symbol? current)
                     (let ((addr-str (car (Get-associate-i (symbol->string current) accumalte_lst 2))))
                          (constants-table (cdr constants_list)
                                    (cons `(,addr ,current (\T_SYMBOL ,addr-str ,last_address)) accumalte_lst)
                                    (+ addr 3)
                                    addr)))
                    ((char? current)
                     (constants-table (cdr constants_list)
                                (cons `(,addr ,current (\T_CHAR ,(char->integer current))) accumalte_lst)
                                (+ addr 2)
                                last_address))
                    ((vector? current)
                     (let ((members (map
                                    (lambda (mem)
                                    (car (Get-associate-i mem accumalte_lst 2)))
                                    (vector->list current))))
                    (constants-table (cdr constants_list)
                                    (cons `(,addr ,current (\T_VECTOR ,(length members) ,@members)) accumalte_lst)
                                    (+ addr 2 (length members))
                                    last_address)))
                    (else (constants-table (cdr constants_list) accumalte_lst addr last_address))))))))

(define create-const-table
    (lambda (pes init-address)
	(let ((basic-consts
            `((,init-address  ,void-object (\T_VOID))
                (,(+ init-address 1) () (\T_NIL))
                (,(+ init-address 2) ,#t (\T_BOOL 1))
                (,(+ init-address 4) ,#f (\T_BOOL 0)))))
            (constants-table (organize-consts (get-consts pes)) (reverse basic-consts) (+ init-address 6) -1))))
      
(define search-fvar
	(lambda (fvar fvar-tab)
		(cond ((null? fvar-tab) 'error_fvar_not_found)
                        ((equal? (cadar fvar-tab) fvar) (caar fvar-tab))
                        (else (search-fvar fvar (cdr fvar-tab))))))

(define fvars->table
   (lambda (fvars accum-lst addr fvars-initss)
     (cond
      ((null? fvars) (reverse accum-lst))
      (else
       (let ((curr (car fvars)))
       (if (not_member? curr fvars-initss)
         (fvars->table (cdr fvars)
                      (cons `(,addr ,curr) accum-lst)
                      (+ addr 1) fvars-initss)
                      (fvars->table (cdr fvars) ;; curr is in inits so no need to add them
                      accum-lst
                      addr fvars-initss)))))))

(define Fvars-init-withoutadresses
  (lambda (fvars_inits)
  (map cadr fvars_inits)))

(define Fvars-init
  (lambda (addr)
    (list
        (list addr '*)
        (list (+ 1 addr) '/)
        (list (+ 2 addr) '+)
        (list (+ 3 addr) '-)
        (list (+ 4 addr) 'boolean?)
        (list (+ 5 addr) 'string-length)
        (list (+ 6 addr) 'vector-length)
        (list (+ 7 addr) 'zero?)
        (list (+ 8 addr) 'string-ref)
        (list (+ 9 addr) 'pair?)
        (list (+ 10 addr) 'procedure?)
        (list (+ 11 addr) 'cons)
        (list (+ 12 addr) 'car)
        (list (+ 13 addr) 'cdr)
        (list (+ 14 addr) 'string?)
        (list (+ 15 addr) 'symbol?)
        (list (+ 16 addr) 'vector?)
        (list (+ 17 addr) 'make-string)
        (list (+ 18 addr) 'string-set!)
        (list (+ 19 addr) 'make-vector)
        (list (+ 20 addr) 'vector-set!)
        (list (+ 21 addr) 'vector-ref)
        (list (+ 22 addr) 'set-cdr!)
        (list (+ 23 addr) 'set-car!)
        (list (+ 24 addr) 'char->integer)
        (list (+ 25 addr) 'integer->char)
        (list (+ 26 addr) 'remainder)
        (list (+ 27 addr) 'integer?)
        (list (+ 28 addr) 'char?)
        (list (+ 29 addr) 'symbol->string)
        (list (+ 30 addr) 'eq?)
        (list (+ 31 addr) 'null?)
        (list (+ 32 addr) 'number?)
        (list (+ 33 addr) '=)
        (list (+ 34 addr) '<)
        (list (+ 35 addr) '>)
        (list (+ 36 addr) 'string->symbol)
        (list (+ 37 addr) 'apply)
        (list (+ 38 addr) 'vector)
        (list (+ 39 addr) 'rational?)
        (list (+ 40 addr) 'numerator)
        (list (+ 41 addr) 'denominator))))
      
(define create-fvar-table
  (lambda (pes addr fvars-withOut)
    (fvars->table (organize-fvars (get-fvars pes)) '() addr fvars-withOut)))

 ; create list of strings from list of symbols and numbers.     
(define create-string-list-from-list
  (lambda (lst)
    (map (lambda (x)
           (cond ((symbol? x) (symbol->string x))
                 ((number? x) (number->string x))))
         lst)))

;take list of strings 
; return string sepereted by commas
(define Make-string-of-commas
  (lambda (L_st)
    (fold-left (lambda (expr ls) (string-append expr ", " ls))  `,(car L_st) (cdr L_st))))

;;;create a string of the memory image of the constant, given a constant table.
(define const-table->const-string
  (lambda (table)
    (Make-string-of-commas (create-string-list-from-list (apply append (map caddr table))))))

;;;create a string of the memory image of the constant, given a constant table.
;;;This is actually the same as the previous procedure and I could've just written (define x [previous procedure])
(define create-cs
  (lambda (table)
      (const-table->const-string table)))
         
(define get-length-of-const-table
    (lambda (const-table)
    (length (create-string-list-from-list (apply append (map caddr const-table))))))
    
(define get-length-of-fvar-table   
  (lambda (table)
    (length table)))


;****************************************************************************************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CODE_GENARATOR_;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(define get-n-element
  (lambda (elem lst n)
    (cond ((null? lst) #f)
          ((equal? (list-ref (car lst) n) elem) (car lst))
          (else (get-n-element elem (cdr lst) n)))))
  
(define pe-identify
  (lambda (tag)
    (lambda (pe)
        (and (list? pe)  (eq? (car pe) tag)))))

(define parsed-const? (pe-identify 'const))      
(define parsed-define? (pe-identify 'def))
(define parsed-lambda-simple? (pe-identify 'lambda-simple))
(define parsed-lambda-opt? (pe-identify 'lambda-opt))
(define parsed-lambda-variadic? (pe-identify 'lambda-var))
(define parsed-if3? (pe-identify 'if3))
(define parsed-or? (pe-identify 'or))
(define parsed-seq? (pe-identify 'seq)) 
(define parsed-fvar? (pe-identify 'fvar))
(define parsed-pvar? (pe-identify 'pvar))
(define parsed-bvar? (pe-identify 'bvar))
(define parsed-applic? (pe-identify 'applic))
(define parsed-tc-applic? (pe-identify 'tc-applic))
(define parsed-set? (pe-identify 'set))
(define parsed-box-set? (pe-identify 'box-set))
(define parsed-box-get? (pe-identify 'box-get))
(define parsed-box? (pe-identify 'box))

(define code-gen-if3
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
    (let ((cgen (lambda (if3 predicate do-if-true do-if-false)
            (let* ((cgen-predicate (code-gen predicate size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                  (cgen-do-if-true (code-gen do-if-true size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                  (cgen-do-if-false (code-gen do-if-false size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                  (lbl-else (index-label "L_IF3_else_"))
                  (lbl-exit (index-label "L_IF3_exit_")))
              (string-append
               cgen-predicate
               new-line 
               (add-line-to-code (CMP "R0" "SOB_BOOLEAN_FALSE")) 
               (add-line-to-code (JUMP_EQ lbl-else))
               cgen-do-if-true 
               new-line
               (add-line-to-code (JUMP lbl-exit))
               (add-label-to-code lbl-else)
               cgen-do-if-false
               new-line
               (add-label-to-code lbl-exit)
               ))))) 
             (apply cgen pe_))))
             
(define code-gen-seq
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
 (let ((cgen (lambda (seq pes)
            (apply string-append
                   (map (lambda (pe)
                          (code-gen pe size-env_ num-params_ const-tab_ fvar-tab_ endlabel_))
                        pes))))) 
            (apply cgen pe_))))

(define code-gen-or
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
  (let ((cgen (lambda (or pes)
            (let* (
                  (last-pe (get-last-element pes))
                  (all-but-last-pe (remove-last-element pes))
                  (lbl-exit (index-label "L_or_exit_"))
                  (all-cgen-with-out-last-pe
                   (apply string-append
                          (map (lambda (expr)
                                 (string-append
                                  (code-gen expr size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
                                  (add-line-to-code (CMP "R0" "SOB_BOOLEAN_FALSE"))
                                  (add-line-to-code (JUMP_NE lbl-exit))
                                  ))
                               all-but-last-pe)))
                  (last-cgen-pe (code-gen last-pe size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)))
              (string-append
               all-cgen-with-out-last-pe
               last-cgen-pe
               (add-label-to-code lbl-exit)
               ))))) 
            (apply cgen pe_))))                      

(define code-gen-applic
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
    (let ((cgen (lambda (applic proc args)
            (let* ( 
                  (lbl-proc (index-label "L_proc_"))
                  (lbl-error (index-label "L_proc_err_"))
                  (lbl-no-error (index-label "L_no_proc_err_"))
                  (num-of-args-str (number->string (+ (length args) 1))))     
              (string-append
               (add-line-to-code (PUSH "SOB_NIL"))
               (apply string-append (map
                                     (lambda (arg)
                                       (string-append
                                        (code-gen arg size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
                                        (add-line-to-code (PUSH "R0"))
                                        ))
                                     (reverse args)))
               (add-line-to-code (PUSH num-of-args-str))
               (code-gen proc size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
               new-line
               (add-label-to-code lbl-proc)
               (add-line-to-code (PUSH (INDD "R0" "1")))
               (add-line-to-code (CALLA (INDD "R0" "2")))
               (add-line-to-code (DROP "1"))
               (add-line-to-code (POP "R1"))
               (add-line-to-code (DROP "R1"))
               (add-line-to-code (JUMP lbl-no-error))
               (add-label-to-code lbl-no-error)
              ))))) (apply cgen pe_))))
  
  
(define (last_element l)
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))

(define code-gen-lambda
  (lambda (lambda-type)
    (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
      (let* (
	    (parms-of-lambda
		      (cond ((or (eq? lambda-type 'opt) (eq? lambda-type 'simple)) (cadr pe_))
                     ((eq? lambda-type 'variadic) '())))     
            (body (last_element pe_)) 
            (new_env (number->string (+ size-env_ 1)))
            (lbl-cpy-program (index-label "lbl_cpy_program")) 
            (lbl-code (index-label "label_code_"))
            (lbl-exit (index-label "label_exit_"))
            (lbl-copy-loop (index-label "label_copy_loop_"))
            (lbl-end-copy-loop (index-label "label_end_copy_loop_"))
            (lbl-copy-parms-loop (index-label "label_copy_parms_loop_")) 
            (lbl-end-copy-parms-loop (index-label "label_end_copy_parms_loop_"))
            (lbl-third-loop (index-label "lbl_third_loop_")); 
            (lbl-end-third-loop (index-label "lbl_end_third_loop_")) 
	    (ALLOCATING_MEMORY_FOR_NEW_ENV   
	    	(string-append
						    (add-line-to-code (PUSH new_env))
						    (add-line-to-code (CALL "MALLOC"))
						    (add-line-to-code (DROP "1"))
						    (add-line-to-code (MOV "R1" "R0"))
					    	)))
       (string-append
	    ALLOCATING_MEMORY_FOR_NEW_ENV
	    (add-line-to-code (CMP "FP" "2"))
        (add-line-to-code (JUMP_LE lbl-cpy-program))
        (add-line-to-code (MOV "R2" (FPARG "0")))
        (add-line-to-code (MOV "R3" "0"))
        (add-line-to-code (MOV "R4" "1"))
        (add-label-to-code lbl-copy-loop)
        (add-line-to-code (CMP "R3" (number->string size-env_)))
        (add-line-to-code (JUMP_GE lbl-end-copy-loop))
        (add-line-to-code (MOV (INDD "R1" "R4") (INDD "R2" "R3")))
        (add-line-to-code (ADD "R3" "1"))
        (add-line-to-code (ADD "R4" "1"))
        (add-line-to-code (JUMP lbl-copy-loop))
        (add-label-to-code lbl-end-copy-loop)        
        (add-label-to-code lbl-cpy-program)
        (add-line-to-code (PUSH (number->string num-params_)))
        (add-line-to-code (CALL "MALLOC"))
        (add-line-to-code (DROP "1"))
        (add-line-to-code (MOV "R3" "R0"))
        (add-line-to-code (MOV "R5" "0"))
        (add-label-to-code lbl-copy-parms-loop)
        (add-line-to-code (CMP "R5" (number->string num-params_)))
        (add-line-to-code (JUMP_GE lbl-end-copy-parms-loop))
		(add-line-to-code (MOV "R4" "2"))
		(add-line-to-code (ADD "R4" "R5"))
		(add-line-to-code (MOV (INDD "R3" "R5") (FPARG "R4")))
		(add-line-to-code (ADD "R5" "1"))
		(add-line-to-code (JUMP lbl-copy-parms-loop))
		(add-label-to-code lbl-end-copy-parms-loop)
		(add-line-to-code (MOV (INDD "R1" "0") "R3"))
		(add-line-to-code (PUSH "3"))
		(add-line-to-code (CALL "MALLOC"))
		(add-line-to-code (DROP "1"))
		(add-line-to-code (MOV (INDD "R0" "0") "T_CLOSURE"))
		(add-line-to-code (MOV (INDD "R0" "1") "R1"))
		(add-line-to-code (MOV (INDD "R0" "2") (string-append "LABEL("lbl-code")")))
		(add-line-to-code (JUMP lbl-exit))
		(add-label-to-code lbl-code)
		(add-line-to-code (PUSH "FP"))
		(add-line-to-code (MOV "FP" "SP"))
		(cond
          ((eq? lambda-type 'simple) 
           (string-append
            (code-gen body (+ size-env_ 1) (length parms-of-lambda) const-tab_ fvar-tab_ endlabel_)
           	))
          (else
           (let ((parms-len (number->string (length parms-of-lambda))))
             (string-append
              (add-line-to-code (MOV "R2" (FPARG "1")))
              (add-line-to-code (ADD "R2" (IMM "1")))
              (add-line-to-code (MOV "R1" (FPARG "R2")))
              (add-line-to-code (MOV "R6" (FPARG "1")))
              (add-line-to-code (string-append "MOV(R7,"parms-len"+1)"))
              (add-label-to-code lbl-third-loop)
              (add-line-to-code (CMP "R6" "R7"))
              (add-line-to-code (JUMP_LE lbl-end-third-loop))
              (add-line-to-code (PUSH "R1"))
              (add-line-to-code (MOV "R2" (FPARG "R6")))
              (add-line-to-code (PUSH "R2"))
              (add-line-to-code (CALL "MAKE_SOB_PAIR"))
              (add-line-to-code (DROP "2"))
              (add-line-to-code (MOV "R1" "R0"))
              (add-line-to-code (DECR "R6"))
              (add-line-to-code (JUMP lbl-third-loop))
              (add-label-to-code lbl-end-third-loop)
			  (add-line-to-code (MOV "R2" "SP"))
			  (add-line-to-code (SUB "R2" (IMM "5")))
			  (add-line-to-code (SUB "R2" (IMM parms-len)))
			  (add-line-to-code (MOV (STACK "R2") "R1"))
              (code-gen body (+ size-env_ 1) (+ 1 (length parms-of-lambda)) const-tab_ fvar-tab_ endlabel_)
             ))))
         new-line
         (add-line-to-code (POP "FP"))
         (add-line-to-code "RETURN")
         (add-label-to-code lbl-exit)
         )))))

(define code-gen-tc-applic
  (lambda (pe_ size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
    (apply (lambda (tc-applic proc args)
            (let ( 
                  (L-tc-loop (index-label "L_tc_applic"))
                  (L-tc-exit (index-label "L_tc_exit"))
                  (argsNum (number->string (+ 1 (length args))))
                  )
                  (string-append 
                  	(add-line-to-code (PUSH "SOB_NIL"))
                    (apply string-append (map
                        (lambda (arg)
                            (string-append
                            (code-gen arg size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
                            	(add-line-to-code (PUSH "R0"))
                                    ))
                        (reverse args)))
                    (add-line-to-code (PUSH (IMM (number->string (+ 1 (length args))))))
                    (code-gen proc size-env_ num-params_ const-tab_ fvar-tab_ endlabel_)
                    (add-line-to-code (PUSH (INDD "R0" "1")))
                    (add-line-to-code (PUSH (FPARG "-1")))
                    (add-line-to-code (MOV "R2" (FPARG "1")))
                    (add-line-to-code (ADD "R2" argsNum))
                    (add-line-to-code (ADD "R2" "7"))
                    (add-line-to-code (MOV "R3" "SP"))
                    (add-line-to-code (SUB "R3" "R2"))
                    (add-line-to-code (MOV "FP" (FPARG "-2")))
                    (add-line-to-code (MOV "R1" "FP"))
                    (add-line-to-code (MOV "R5" (IMM "0")))
                    (add-line-to-code (MOV "R6" (IMM argsNum)))
                    (add-line-to-code (ADD "R6" (IMM "3")))
                    (add-label-to-code L-tc-loop)
                    (add-line-to-code (CMP "R5" "R6"))
                    (add-line-to-code (JUMP_GE L-tc-exit))
                    (add-line-to-code (MOV "R7" (IMM argsNum)))
                    (add-line-to-code (ADD "R7" (IMM "1")))
                    (add-line-to-code (SUB "R7" "R5"))
                    (add-line-to-code (MOV (STACK "R3") (STARG "R7")))
                    (add-line-to-code (INCR "R3"))
                    (add-line-to-code (INCR "R5"))
                    (add-line-to-code (JUMP L-tc-loop))
                    (add-label-to-code L-tc-exit)
                    (add-line-to-code (MOV "SP" "R3"))
                    (add-line-to-code (JUMPA (INDD "R0" "2")))
                    ))) pe_)))
          
(define code-gen-pvar
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply (lambda (pvar var min_)   
              (string-append        
              	(add-line-to-code (MOV "R0" (FPARG (number->string (+ min_ 2)))))         
               )) pe)))

(define code-gen-bvar
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply (lambda (bvar var maj min_)
            (string-append
            	(add-line-to-code (MOV "R0" (FPARG "0")))
            	(add-line-to-code (MOV "R0" (INDD "R0" (number->string maj))))
            	(add-line-to-code (MOV "R0" (INDD "R0" (number->string min_))))
            )) pe
          )))
          
(define code-gen-fvar
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply (lambda (fvar name)
                  (string-append  
                  	(add-line-to-code (MOV "R0" (IND (number->string (car (Get-associate-i name fvar-tab 2))))))        
                   ))  pe)))     

(define code-gen-define
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (def var val)
              (string-append
               (code-gen val size-env num-params const-tab fvar-tab endlabel)
                new-line
               (add-line-to-code (MOV (IND (number->string (car (Get-associate-i (cadr var) fvar-tab 2)))) "R0"))
               (add-line-to-code (MOV "R0" "SOB_VOID"))
               )) pe)))
               
           
         
(define code-gen-const 
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply
          (lambda (const j)   
              (string-append  
                (add-line-to-code (MOV "R0" (number->string (car (Get-associate-i j const-tab 2)))))            
               )) pe)))
               
(define code-gen-set 
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
	
    (apply 
          (lambda (set var val)
    
          (cond ( (eq? (car var) 'pvar) 
		      (string-append
			(code-gen val size-env num-params const-tab fvar-tab endlabel)
			new-line
			(add-line-to-code (MOV (FPARG (number->string (+ (caddr var) 2))) "R0"))
			(add-line-to-code (MOV "R0" "SOB_VOID"))
		      ))
		( (eq? (car var) 'bvar)
		
		      (string-append
			(code-gen val size-env num-params const-tab fvar-tab endlabel)
			new-line
			(add-line-to-code (PUSH "R10"))
			(add-line-to-code (MOV "R10" (FPARG "0")))
			(add-line-to-code (MOV "R10" (INDD "R10" (number->string (caddr var)))))
			(add-line-to-code (MOV (INDD "R10" (number->string (cadddr var))) "R0"))
			(add-line-to-code (POP "R10"))
			(add-line-to-code (MOV "R0" "SOB_VOID"))
			))
		((eq? (car var) 'fvar)
		      (string-append
			(code-gen val size-env num-params const-tab fvar-tab endlabel)
			new-line
			(add-line-to-code (MOV (IND (number->string (car (Get-associate-i (cadr var) fvar-tab 2)))) "R0"))
			(add-line-to-code (MOV "R0" "SOB_VOID"))
		      ))))
		 pe)))


(define code-gen-box 
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (box var) 
              (string-append
             new-line
             (add-line-to-code (PUSH (IMM "1")))
             (add-line-to-code (CALL "MALLOC"))
             (add-line-to-code (DROP "1"))
             (add-line-to-code (MOV (IND "R0") (FPARG (number->string (+ (caddr var) 2)))))
              )) 
          pe)))

(define code-gen-box-set
   (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (box-set var val)
              (string-append
             new-line
              (code-gen val size-env num-params const-tab fvar-tab endlabel) new-line
              (add-line-to-code (MOV "R11" "R0"))
              (code-gen var size-env num-params const-tab fvar-tab endlabel) new-line
              (add-line-to-code (MOV (IND "R0") "R11"))
              (add-line-to-code (MOV "R0" "SOB_VOID"))
              ))
            pe)))

               
(define code-gen-box-get
	(lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (apply 
          (lambda (box-get var)
              (string-append
             new-line
              (code-gen var size-env num-params const-tab fvar-tab endlabel)new-line
              (add-line-to-code (MOV "R0" (IND "R0")))
              ))
            pe)))

(define code-gen
  (lambda (pe size-env num-params const-tab fvar-tab endlabel)
    (let ((params `(,pe ,size-env ,num-params ,const-tab ,fvar-tab ,endlabel)))
      (cond
        ((parsed-const? pe) (apply code-gen-const params))  
        ((parsed-define? pe) (apply code-gen-define params)) 
        ((parsed-lambda-simple? pe) (apply (code-gen-lambda 'simple) params))
        ((parsed-lambda-opt? pe) (apply (code-gen-lambda 'opt) params))
        ((parsed-lambda-variadic? pe) (apply (code-gen-lambda 'variadic) params)) 
        ((parsed-if3? pe) (apply code-gen-if3 params)) 
        ((parsed-or? pe) (apply code-gen-or params)) 
        ((parsed-seq? pe) (apply code-gen-seq params)) 
        ((parsed-fvar? pe) (apply code-gen-fvar params)) 
        ((parsed-pvar? pe) (apply code-gen-pvar params)) 
        ((parsed-bvar? pe) (apply code-gen-bvar params)) 
        ((parsed-applic? pe) (apply code-gen-applic params)) 
        ((parsed-tc-applic? pe) (apply code-gen-tc-applic params)) 
        ((parsed-set? pe) (apply code-gen-set params)) 
        ((parsed-box? pe) (apply code-gen-box params))
        ((parsed-box-set? pe) (apply code-gen-box-set params))
        ((parsed-box-get? pe) (apply code-gen-box-get params)) 
        (else (error 'code-gen "Error: NO EXPRESSION KNOWN"))))))     

(define write-to-file
  (lambda (target-file str)
    (let ((fp (open-output-file target-file '(replace))))
      (begin
        (display str fp)
        (close-port fp)))))

(define First-sym-addr
  (lambda (constan-table)
    (if (null? constan-table)
        -1  
          (if (eq? (list-ref (list-ref (car constan-table) 2) 0) '\T_SYMBOL)
              (caar constan-table)
              (First-sym-addr (cdr constan-table))))))

(set! number 0)
(define index-label
  (lambda (label-name)
        (set! number (+ number 1))
        (string-append label-name
                       (number->string number))))
                       
(define program-end-label (index-label "L_end_prog")) 

(define void-object (if #f #f))

(define make-prologue
  (lambda (constants-table fvars-table frst_sym_address)
    (let ((cont_label_ (index-label "Cont_label__"))
          (null-addrs (car (Get-associate-i '() constants-table 2)))
          (void_addrs (car (Get-associate-i void-object constants-table 2)))
          (bool-t-addrs (car (Get-associate-i #t constants-table 2)))
          (bool-f-addrs (car (Get-associate-i #f constants-table 2))))
        (string-append
        	(add-to-code "#include <stdlib.h>")
        	(add-to-code "#include <string.h>")
        	(add-to-code "#include <stdio.h>")
        	(add-to-code "#include \"arch/cisc.h\"")
        	(add-to-code "#include \"arch/debug_macros.h.c\"")
        	(add-to-code "#define DO_SHOW 2")
        	(add-to-code "int main() {")
        	(add-line-to-code "int i,j")
        	(add-line-to-code "START_MACHINE")
        	(add-line-to-code (JUMP "CONTINUE"))
        	(add-to-code "#include \"arch/char.lib\"")
        	(add-to-code "#include \"arch/io.lib\"")
        	(add-to-code "#include \"arch/math.lib\"")
        	(add-to-code "#include \"arch/string.lib\"")
        	(add-to-code "#include \"arch/system.lib\"")
        	(add-to-code "#include \"arch/scheme.lib\"")
        	(add-label-to-code "CONTINUE")
     			(add-to-code (string-append "#define SOB_VOID "(number->string void_addrs)))
     			(add-to-code (string-append "#define SOB_NIL "(number->string null-addrs)))
     			(add-to-code (string-append "#define SOB_BOOLEAN_FALSE "(number->string bool-f-addrs)))
     			(add-to-code (string-append "#define SOB_BOOLEAN_TRUE "(number->string bool-t-addrs)))
     			(add-line-to-code (JUMP "AFTER_COMPARE"))
     			(add-label-to-code "L_COMPARE")
     			(add-line-to-code (PUSH "FP"))
     			(add-line-to-code (MOV "FP" "SP"))
     			(add-line-to-code (PUSH "R1"))
     			(add-line-to-code (PUSH "R2"))
     			(add-line-to-code (PUSH "R3"))
     			(add-line-to-code (PUSH "R4"))
     			(add-line-to-code (PUSH "R5"))

     			(add-line-to-code (MOV "R1" (FPARG "0")))
     			(add-line-to-code (MOV "R2" (FPARG "1")))
     			(add-line-to-code (MOV "R3" (INDD "R1" "1")))
     			(add-line-to-code (MOV "R4" (INDD "R2" "1")))
     			(add-line-to-code (CMP (IND "R1") "T_INTEGER"))
     			(add-line-to-code (JUMP_EQ "cmp_nums"))
     			(add-line-to-code (MOV "R1" (INDD "R1" "2")))
     			(add-label-to-code "second_check")
     			
     			(add-line-to-code (CMP (IND "R2") "T_INTEGER"))
     			(add-line-to-code (JUMP_EQ "Two_integers"))
     			(add-line-to-code (MOV "R2" (INDD "R2" "2")))
     			(add-label-to-code "General")
     			(add-line-to-code (MUL "R3" "R2"))
     			(add-line-to-code (MUL "R1" "R4"))
     			(add-line-to-code (CMP "R1" "R3"))
     			(add-line-to-code (JUMP_EQ "nums_eq"))

     			(add-line-to-code (JUMP_LT "nums_lt"))
     			(add-line-to-code (MOV "R0" "1"))
     			(add-line-to-code (JUMP "nums_finish"))
     			(add-label-to-code "nums_eq")

     			(add-line-to-code (MOV "R0" "0"))
     			(add-line-to-code (JUMP "nums_finish"))
     			(add-label-to-code "nums_lt")
     			
		      (add-line-to-code (MOV "R0" "-1"))
     			(add-line-to-code (JUMP "nums_finish"))
     			(add-label-to-code "cmp_nums")
     			(add-line-to-code (MOV "R1" "1"))
     			(add-line-to-code (JUMP "second_check"))
     			(add-label-to-code "Two_integers")
     			(add-line-to-code (MOV "R2" "1"))
     			(add-line-to-code (JUMP "General"))
     			(add-label-to-code "nums_finish")

					(add-line-to-code (POP "R5"))
					(add-line-to-code (POP "R4"))
					(add-line-to-code (POP "R3"))
					(add-line-to-code (POP "R2"))
					(add-line-to-code (POP "R1"))
					(add-line-to-code (POP "FP"))
					(add-line-to-code "RETURN")
					(add-label-to-code "AFTER_COMPARE")
       
(add-to-code ((primitive-bigger constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-remainder constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-rational constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-numerator constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-denominator constants-table fvars-table frst_sym_address)))      
(add-to-code ((primtive-symbol->string constants-table fvars-table frst_sym_address)))
(add-to-code ((prim-cons constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-eq constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-vector constants-table fvars-table frst_sym_address))) 
(add-to-code ((primitive-make-string constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-make-vector constants-table fvars-table frst_sym_address))) 
(add-to-code ((primitive-string-set constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-vector-set constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-vector-length constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-vector-ref constants-table fvars-table frst_sym_address))) 
(add-to-code ((primitive-str-length constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-string-ref constants-table fvars-table frst_sym_address)))
(add-to-code ((primtive-set-cdr! constants-table fvars-table frst_sym_address)))
(add-to-code ((primtive-set-car! constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-procedure? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-pair? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-symbol? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-string? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-zero? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-vector? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-null? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-char? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-integer? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-boolean? constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-char->integer constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-integer->char constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-car constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-cdr constants-table fvars-table frst_sym_address)))
(add-to-code ((prim-string-to-symbol constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-apply constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-symbol-string constants-table fvars-table frst_sym_address)))     
(add-to-code ((primitive-number constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-numbers-equal constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-smaller constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-mul constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-div constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-minus constants-table fvars-table frst_sym_address)))
(add-to-code ((primitive-plus constants-table fvars-table frst_sym_address)))
))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define generate-primitive-closure
    (lambda (lbl addr)   
            (string-append
            	(add-line-to-code (PUSH "3"))
            	(add-line-to-code (CALL "MALLOC"))
            	(add-line-to-code (DROP "1"))
            	(add-line-to-code (MOV (INDD "R0" "0") (IMM "T_CLOSURE")))
            	(add-line-to-code (MOV (INDD "R0" "1") (IMM "0")))
            	(add-line-to-code (MOV (INDD "R0" "2") (string-append "LABEL(" lbl ")")))
            	(add-line-to-code (MOV (IND (number->string addr)) "R0"))
		
			
		)))	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define primitive-apply
  (lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'apply fvars-table))
		      (lbl-apply (index-label "Lprim_apply") )
                      (lbl-closure_apply (index-label "L_primitive_closure_apply_") )
                      (lbl-end_apply (index-label "LPRIM_end_apply"))
                      )
			(string-append
			(add-line-to-code (JUMP lbl-closure_apply))
        (add-label-to-code lbl-apply)
        (add-line-to-code (PUSH "FP"))
        (add-line-to-code (MOV "FP" "SP"))
        (add-line-to-code (MOV "R15" (FPARG "-2")))
        (add-line-to-code (MOV "R4" "FP"))
        (add-line-to-code (SUB "R4" (IMM "4")))
        (add-line-to-code (SUB "R4" (FPARG "1")))
        (add-line-to-code (MOV "R3" (FPARG "2")))
        (add-line-to-code (MOV "R0" (FPARG "3")))
        (add-line-to-code (MOV "R7" (IMM "0")))

        (add-label-to-code "Lprim_apply_list_loop")
        (add-line-to-code (CMP "R0" "SOB_NIL"))
        (add-line-to-code (JUMP_EQ "Lend_apply_list_loop"))
        (add-line-to-code (MOV "R1" (INDD "R0" "1")))
        (add-line-to-code (PUSH "R1"))
        (add-line-to-code (MOV "R0" (INDD "R0" "2")))
        (add-line-to-code (ADD "R7" (IMM "1")))
        (add-line-to-code (JUMP "Lprim_apply_list_loop"))
        
        (add-label-to-code "Lend_apply_list_loop")
        (add-line-to-code (PUSH "SOB_NIL"))
        (add-line-to-code (MOV "R5" "SP"))
        (add-line-to-code (SUB "R5" (IMM "1")))
        (add-line-to-code (MOV "R9" "FP"))

        (add-label-to-code "Lreverse_params")
        (add-line-to-code (CMP "R9" "R5"))
        (add-line-to-code (JUMP_GE "Lapply_in_tp"))
        (add-line-to-code (MOV "R6" (STACK "R5")))
        (add-line-to-code (MOV (STACK "R5") (STACK "R9")))
        (add-line-to-code (MOV (STACK "R9") "R6"))
        (add-line-to-code (SUB "R5" (IMM "1")))
        (add-line-to-code (ADD "R9" (IMM "1")))
        (add-line-to-code (JUMP "Lreverse_params"))

        (add-label-to-code "Lapply_in_tp")
        (add-line-to-code (PUSH "R7+1"))
        (add-line-to-code (PUSH (INDD "R3" "1")))
        (add-line-to-code (PUSH (FPARG "-1")))
        (add-line-to-code (MOV "R5" "FP"))

        (add-label-to-code "Lapply_in_tp_loop")
        (add-line-to-code (CMP "R5" "SP"))
        (add-line-to-code (JUMP_EQ "Lend_prim_apply"))
        (add-line-to-code (MOV (STACK "R4") (STACK "R5")))
        (add-line-to-code (ADD "R4" (IMM "1")))
        (add-line-to-code (ADD "R5" (IMM "1")))
        (add-line-to-code (JUMP "Lapply_in_tp_loop"))

        (add-label-to-code "Lend_prim_apply")
        (add-line-to-code (MOV "SP" "R4"))
        (add-line-to-code (MOV "FP" "R15"))
        (add-line-to-code (JUMPA (INDD "R3" "2")))
        (add-line-to-code (POP "FP"))
        (add-line-to-code "RETURN")

        (add-label-to-code lbl-closure_apply)

        (add-to-code (generate-primitive-closure lbl-apply addr))
        )))))

(define primitive-symbol-string
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'symbol->string fvars-table))
                      (lbl-symbol-string (index-label "Lprim_symbolstringr") )
                      (lbl-closure_symbol-string (index-label "LPRIM_CLOSURE_symbol_string") )
                      (lbl-end_symbol-string (index-label "LPRIM_end_symbolstring") )
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_symbol-string))
				(add-label-to-code lbl-symbol-string)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R0" (FPARG "2")))
				(add-line-to-code (MOV "R0" (INDD "R0" "1")))
				(add-label-to-code lbl-end_symbol-string)
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")
				(add-label-to-code lbl-closure_symbol-string)
				(add-to-code (generate-primitive-closure lbl-symbol-string addr))
			)))))

(define primitive-number
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'number? fvars-table))
                      (lbl-number (index-label "Lprim_number") )
                      (lbl-closure_number (index-label "LPRIM_CLOSURE_number") )
                      (lbl-end_number (index-label "LPRIM_end_number") )
                      )
			(string-append

				(add-line-to-code (JUMP lbl-closure_number))
				(add-label-to-code lbl-number)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))
				(add-line-to-code (MOV "R1" (FPARG "2")))
				(add-line-to-code (CMP (IND "R1") "T_INTEGER"))

				(add-line-to-code (CMP (IND "R1") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-end_number))
				(add-line-to-code (CMP (IND "R1") "T_FRACTION"))
				(add-line-to-code (JUMP_EQ lbl-end_number))
				(add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))


				(add-label-to-code lbl-end_number)
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-closure_number)
				(add-to-code (generate-primitive-closure lbl-number addr))


			)))))

(define primitive-remainder
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'remainder fvars-table))
                      (lbl-remainder (index-label "Lprim_remainder") )
                      (lbl-closure_remainder (index-label "LPRIM_CLOSURE_remainder") )
                      (lbl-end_remainder (index-label "LPRIM_end_remainder") )
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_remainder))
				(add-label-to-code lbl-remainder)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R0" (FPARG "2")))
				(add-line-to-code (MOV "R1" (FPARG "3")))
				(add-line-to-code (MOV "R0" (INDD "R0" "1")))
				(add-line-to-code (MOV "R1" (INDD "R1" "1")))
				(add-line-to-code (REM "R0" "R1"))
				(add-line-to-code (PUSH "R0"))
				(add-line-to-code (CALL "MAKE_SOB_INTEGER"))
				(add-line-to-code (DROP "1"))
				(add-label-to-code lbl-end_remainder)
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")
				(add-label-to-code lbl-closure_remainder)
				(add-to-code (generate-primitive-closure lbl-remainder addr))	
			)))))



(define primitive-rational
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'rational? fvars-table))
                      (lbl-rational (index-label "Lprim_rational") )
                      (lbl-closure_rational (index-label "LPRIM_CLOSURE_rational") )
                      (lbl-end_rational (index-label "LPRIM_end_rational") )
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_rational))
				(add-label-to-code lbl-rational)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))
				(add-line-to-code (MOV "R1" (FPARG "2")))
				(add-line-to-code (CMP (IND "R1") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-end_rational))
				(add-line-to-code (CMP (IND "R1") "T_FRACTION"))
				(add-line-to-code (JUMP_EQ lbl-end_rational))
				(add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))

				(add-label-to-code lbl-end_rational)
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")
				(add-label-to-code lbl-closure_rational)
				(add-to-code (generate-primitive-closure lbl-rational addr))	
			)))))

(define primitive-numerator
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'numerator fvars-table))
                      (lbl-numerator (index-label "Lprim_numerator") )
                      (lbl-closure_numerator (index-label "LPRIM_CLOSURE_numerator") )
                     
                      )
			(string-append

				(add-line-to-code (JUMP lbl-closure_numerator))
				(add-label-to-code lbl-numerator)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R0" (FPARG "2")))
				(add-line-to-code (MOV "R0" (INDD "R0" "1")))
				(add-line-to-code (PUSH "R0"))
				(add-line-to-code (CALL "MAKE_SOB_INTEGER"))
				(add-line-to-code (DROP "1"))

				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")
				(add-label-to-code lbl-closure_numerator)
				(add-to-code (generate-primitive-closure lbl-numerator addr))	
			)))))

(define primitive-denominator
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'denominator fvars-table))
                      (lbl-denominator (index-label "Lprim_denominator") )
                      (IntegerArg (index-label "L_integer") )
                      (EndLabel (index-label "L_end_denom") )
                      (lbl-closure_denominator (index-label "LPRIM_CLOSURE_denominator") )
                     
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_denominator))
				(add-label-to-code lbl-denominator)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R0" (FPARG "2")))
				(add-line-to-code (CMP (IND "R0") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ IntegerArg))
				(add-line-to-code (MOV "R0" (INDD "R0" "2")))
				(add-line-to-code (PUSH "R0"))
				(add-line-to-code (CALL "MAKE_SOB_INTEGER"))
				(add-line-to-code (JUMP EndLabel))

				(add-label-to-code IntegerArg)
				(add-line-to-code (MOV "R0" "1"))
				(add-line-to-code (PUSH "R0"))
				(add-line-to-code (CALL "MAKE_SOB_INTEGER"))

				(add-label-to-code EndLabel)
				(add-line-to-code (DROP "1"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")
				(add-label-to-code lbl-closure_denominator)
				(add-to-code (generate-primitive-closure lbl-denominator addr))	

			
			)))))

(define primitive-mul
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '* fvars-table))
                      (lbl-mul (index-label "Lprim_mul") )
                      (lbl-closure_mul (index-label "LPRIM_CLOSURE_mul") )
                      (lbl-mul-loop (index-label "Lprimitive_minus_loop") )
                      (lbl-mul-intsToFracs (index-label "Lprimitive_mul_intsToFracs") )
                      (lbl-mul-AfterUpdate (index-label "Lprimitive_mul_AfterUpdate") )
                      (lbl-mul-end (index-label "Lprimitive_mul_end") )     
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_mul))
				(add-label-to-code lbl-mul)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (PUSH "R1"))
				(add-line-to-code (PUSH "R2"))
				(add-line-to-code (PUSH "R3"))
				(add-line-to-code (PUSH "R4"))
				(add-line-to-code (PUSH "R5"))
				(add-line-to-code (PUSH "R6"))
				(add-line-to-code (PUSH "R7"))
				(add-line-to-code (MOV "R1" (IMM "1")))
				(add-line-to-code (MOV "R2" (IMM "1")))
				(add-line-to-code (MOV "R5" (IMM "0")))

				
				(add-label-to-code lbl-mul-loop)
				(add-line-to-code (CMP "R5" (string-append (FPARG "1") "-1")))
				(add-line-to-code (JUMP_EQ lbl-mul-end))
				(add-line-to-code (MOV "R6" (FPARG "R5+2")))
				(add-line-to-code (CMP (IND "R6") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-mul-intsToFracs))
				(add-line-to-code (MOV "R3" (INDD "R6" (IMM "1"))))
				(add-line-to-code (MOV "R4" (INDD "R6" (IMM "2"))))

				(add-label-to-code lbl-mul-AfterUpdate)
				(add-line-to-code (MUL "R1" "R3"))
				(add-line-to-code (MUL "R2" "R4"))
				(add-line-to-code (INCR "R5"))
				(add-line-to-code (JUMP lbl-mul-loop))

				(add-label-to-code lbl-mul-intsToFracs)
				(add-line-to-code (MOV "R3" (INDD "R6" "1")))
				(add-line-to-code (MOV "R4" (IMM "1")))
				(add-line-to-code (JUMP lbl-mul-AfterUpdate))

				(add-label-to-code lbl-mul-end)
				(add-line-to-code (PUSH "R2"))
				(add-line-to-code (PUSH "R1"))
				(add-line-to-code (CALL "MAKE_SOB_FRACTION"))
				(add-line-to-code (DROP "2"))
				(add-line-to-code (POP "R7"))
				(add-line-to-code (POP "R6"))
				(add-line-to-code (POP "R5"))
				(add-line-to-code (POP "R4"))
				(add-line-to-code (POP "R3"))
				(add-line-to-code (POP "R2"))
				(add-line-to-code (POP "R1"))
				(add-line-to-code (POP "FP"))

				(add-line-to-code "RETURN")
				(add-label-to-code lbl-closure_mul)
				(add-to-code (generate-primitive-closure lbl-mul addr))	
			)))))

(define primitive-div
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '/ fvars-table))
                      (lbl-div (index-label "Lprim_div")  )
                      (lbl-closure_div (index-label "LPRIM_CLOSURE_div")  )
                      (lbl-div-loop (index-label "Lprimitive_div_loop")  )
                      (lbl-div-intsToFracs (index-label "Lprimitive_div_intsToFracs")  )
                      (lbl-div-AfterUpdate (index-label "Lprimitive_div_AfterUpdate")  )
                      (lbl-div-One_arg (index-label "Lprimitive_div_ONe_ARG")  )
                      (lbl-div-end (index-label "Lprimitive_div_end")  )  
                      (lbl-div-Integer (index-label "L_Integer"))
                      (lbl-int "L_little_int")
                      )
			(string-append

				(add-line-to-code (JUMP lbl-closure_div))
				(add-label-to-code lbl-div)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))

				(add-line-to-code (MOV "R5" (FPARG "2")))
				(add-line-to-code (MOV "R1" (INDD "R5" "1")))
				(add-line-to-code (MOV "R2" (IMM "1")))
				(add-line-to-code (CMP (IND "R5") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-div-Integer))
				(add-line-to-code (MOV "R2" (INDD "R5" "2")))

				(add-label-to-code lbl-div-Integer)
				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (CMP "R6" "1"))
				(add-line-to-code (JUMP_EQ lbl-div-One_arg))
				(add-line-to-code (MOV "R5" "1"))

				(add-label-to-code lbl-div-loop)
				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (CMP "R5" "R6"))

				(add-line-to-code (JUMP_EQ lbl-div-end))
				(add-line-to-code (MOV "R7" "R5"))
				(add-line-to-code (INCR "R7"))
				(add-line-to-code (INCR "R7"))
				(add-line-to-code (MOV "R6" (FPARG "R7")))
				(add-line-to-code (CMP (IND "R6") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-div-intsToFracs))
				(add-line-to-code (MOV "R3" (INDD "R6" "1")))
				(add-line-to-code (MOV "R4" (INDD "R6" "2")))

				(add-label-to-code lbl-div-AfterUpdate)
				(add-line-to-code (MUL "R1" "R4"))
				(add-line-to-code (MUL "R2" "R3"))
				(add-line-to-code (INCR "R5"))
				(add-line-to-code (JUMP lbl-div-loop))

				(add-label-to-code lbl-div-intsToFracs)
				(add-line-to-code (MOV "R3" (INDD "R6" "1")))
				(add-line-to-code (MOV "R4" (IMM "1")))
				(add-line-to-code (JUMP lbl-div-AfterUpdate))

				(add-label-to-code lbl-div-end)
				(add-line-to-code (PUSH "R2"))
				(add-line-to-code (PUSH "R1"))
				(add-line-to-code (CALL "MAKE_SOB_FRACTION"))
				(add-line-to-code (DROP "2"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-div-One_arg)
				(add-line-to-code (CMP (IND "R5") "T_FRACTION"))
				(add-line-to-code (JUMP_NE lbl-int))
				(add-line-to-code (MOV "R1" (INDD "R5" "2")))
				(add-line-to-code (MOV "R2" (INDD "R5" "1")))
				(add-line-to-code (JUMP lbl-div-end))

				(add-label-to-code lbl-int)
				(add-line-to-code (MOV "R1" (IMM "1")))
				(add-line-to-code (MOV "R2" (INDD "R5" "1")))
				(add-line-to-code (JUMP lbl-div-end))

				(add-label-to-code lbl-closure_div)
				(add-to-code (generate-primitive-closure lbl-div addr))
			)))))

(define primitive-minus
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '- fvars-table))
                      (lbl-minus (index-label "Lprim_minus")  )
                      (lbl-closure_minus (index-label "LPRIM_CLOSURE_minus")  )
                      (lbl-minus-loop (index-label "Lprimitive_minus_loop")  )
                      (lbl-minus-intsToFracs (index-label "Lprimitive_minus_intsToFracs")  )
                      (lbl-minus-AfterUpdate (index-label "Lprimitive_minus_AfterUpdate")  )
                      (lbl-minus-One_arg (index-label "Lprimitive_minus_ONe_ARG")  )
                      (lbl-minus-end (index-label "Lprimitive_minus_end")  )
                      (lbl-minus-Integer (index-label "L_IntegerR"))
                      (lbl-int "L_little_int_2")
                      )
      (string-append
      	(add-line-to-code (JUMP lbl-closure_minus))
				(add-label-to-code lbl-minus)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R5" (FPARG "2")))
				(add-line-to-code (MOV "R1" (INDD "R5" "1")))
				(add-line-to-code (MOV "R2" (IMM "1")))
				(add-line-to-code (CMP (IND "R5") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-minus-Integer))
				(add-line-to-code (MOV "R2" (INDD "R5" "2")))

				(add-label-to-code lbl-minus-Integer)
				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (CMP "R6" "1"))
				(add-line-to-code (JUMP_EQ lbl-minus-One_arg))
				(add-line-to-code (MOV "R5" "1"))

				(add-label-to-code lbl-minus-loop)
				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (CMP "R5" "R6"))
				(add-line-to-code (JUMP_EQ lbl-minus-end))
				(add-line-to-code (MOV "R7" "R5"))
				(add-line-to-code (INCR "R7"))
				(add-line-to-code (INCR "R7"))
				(add-line-to-code (MOV "R6" (FPARG "R7")))
				(add-line-to-code (CMP (IND "R6") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-minus-intsToFracs))
				(add-line-to-code (MOV "R3" (INDD "R6" "1")))
				(add-line-to-code (MOV "R4" (INDD "R6" "2")))

				(add-label-to-code lbl-minus-AfterUpdate)
				(add-line-to-code (MUL "R1" "R4"))
				(add-line-to-code (MUL "R4" "R2"))
				(add-line-to-code (MUL "R3" "R2"))
				(add-line-to-code (SUB "R1" "R3"))
				(add-line-to-code (MOV "R2" "R4"))
				(add-line-to-code (INCR "R5"))
				(add-line-to-code (JUMP lbl-minus-loop))

				(add-line-to-code (JUMP lbl-minus-loop))
				(add-label-to-code lbl-minus-intsToFracs)
				(add-line-to-code (MOV "R3" (INDD "R6" "1")))
				(add-line-to-code (MOV "R4" (IMM "1")))
				(add-line-to-code (JUMP lbl-minus-AfterUpdate))
			
				(add-label-to-code lbl-minus-end)
				(add-line-to-code (PUSH "R2"))
				(add-line-to-code (PUSH "R1"))
				(add-line-to-code (CALL "MAKE_SOB_FRACTION"))
				(add-line-to-code (DROP "2"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-minus-One_arg)
				(add-line-to-code (CMP (IND "R5") "T_FRACTION"))
				(add-line-to-code (JUMP_NE lbl-int))
				(add-line-to-code (MUL "R1" "-1"))
				(add-line-to-code (JUMP_NE lbl-minus-end))
				(add-label-to-code lbl-int)
				(add-line-to-code (MUL "R1" "-1"))
				(add-line-to-code (MOV "R2" (IMM "1")))
				(add-line-to-code (JUMP lbl-minus-end))

				(add-label-to-code lbl-closure_minus)
				(add-to-code (generate-primitive-closure lbl-minus addr))
			)))))

(define primitive-plus
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '+ fvars-table))
                      (lbl-plus (index-label "Lprim_PLUS")  )
                      (lbl-closure_plus  (index-label "LPRIM_CLOSURE_PLUS")  )
                      (lbl-plus-loop (index-label "Lprimitive_plusl_loop")  )
                      (lbl-plus-intsToFracs (index-label "Lprimitive_plus_intsToFracs")  )
                      (lbl-plus-AfterUpdate (index-label "Lprimitive_plus_AfterUpdate")  )
                      (lbl-plus-end (index-label "Lprimitive_plus_end")  )     
                      )
		
			(string-append
				(add-line-to-code (JUMP lbl-closure_plus))
				(add-label-to-code lbl-plus)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R1" "0"))
				(add-line-to-code (MOV "R2" "1"))
				(add-line-to-code (MOV "R5" "0"))
				(add-line-to-code (MOV "R8" "0"))
				(add-line-to-code (INCR "R8"))
				(add-line-to-code (INCR "R8"))

				(add-label-to-code lbl-plus-loop)
				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (CMP "R5" "R6"))
				(add-line-to-code (JUMP_EQ lbl-plus-end))
				(add-line-to-code (MOV "R9" (FPARG "R8")))
				(add-line-to-code (MOV "R6" "R9"))
				(add-line-to-code (CMP (IND "R6") "T_INTEGER"))
				(add-line-to-code (JUMP_EQ lbl-plus-intsToFracs))
				(add-line-to-code (MOV "R3" (INDD "R6" "1")))
				(add-line-to-code (MOV "R4" (INDD "R6" "2")))

				(add-label-to-code lbl-plus-AfterUpdate)
				(add-line-to-code (MUL "R1" "R4"))
				(add-line-to-code (MUL "R4" "R2"))
				(add-line-to-code (MUL "R3" "R2"))
				(add-line-to-code (ADD "R1" "R3"))
				(add-line-to-code (MOV "R2" "R4"))
				(add-line-to-code (INCR "R5"))
				(add-line-to-code (INCR "R8"))
				(add-line-to-code (JUMP lbl-plus-loop))

				(add-label-to-code lbl-plus-intsToFracs)
				(add-line-to-code (MOV "R3" (INDD "R6" "1")))
				(add-line-to-code (MOV "R4" (IMM "1")))
				(add-line-to-code (JUMP lbl-plus-AfterUpdate))

				(add-label-to-code lbl-plus-end)
				(add-line-to-code (PUSH "R2"))
				(add-line-to-code (PUSH "R1"))
				(add-line-to-code (CALL "MAKE_SOB_FRACTION"))
				(add-line-to-code (DROP "2"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-closure_plus)
				(add-to-code (generate-primitive-closure lbl-plus addr))
			)))))


(define primitive-numbers-equal
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '= fvars-table))
                      (lbl-numbers-equal (index-label "Lprim_numbers_equal")  )
                      (lbl-closure_numbers-equal  (index-label "Lmake_numbers_equal")  )
                      (lbl-numbers-equal-loop (index-label "Lprimitive_numbersequal_loop")  )
                      (lbl-numbers-equalr-true (index-label "Lprimitive_numbersequal_true")  )
                      (lbl-numbers-equal-false (index-label "Lprimitive_numbersequal_false")  )
                      (lbl-numbers-equal-end (index-label "Lprimitive_numbersequal_end")  )     
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_numbers-equal))
				(add-label-to-code lbl-numbers-equal)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))
				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (MOV "R1" "R6"))
				(add-line-to-code (MOV "R7" "SOB_BOOLEAN_TRUE"))
				(add-line-to-code (MOV "R2" (IMM "1")))

				(add-label-to-code lbl-numbers-equal-loop)
				(add-line-to-code (CMP "R1" "R2"))
				(add-line-to-code (JUMP_EQ lbl-numbers-equal-end))

				(add-line-to-code (MOV "R9" "R2"))
				(add-line-to-code (INCR "R9"))
				(add-line-to-code (MOV "R3" (FPARG "R9")))
				(add-line-to-code (INCR "R9"))
				(add-line-to-code (MOV "R4" (FPARG "R9")))

				(add-line-to-code (PUSH "R3"))
				(add-line-to-code (PUSH "R4"))
				(add-line-to-code (CALL "L_COMPARE"))

				(add-line-to-code (DROP "2"))
				(add-line-to-code (CMP "R0" "0"))
				(add-line-to-code (JUMP_NE lbl-numbers-equal-false))
				(add-line-to-code (INCR "R2"))
				(add-line-to-code (JUMP lbl-numbers-equal-loop))

				(add-label-to-code lbl-numbers-equal-false)
				(add-line-to-code (MOV "R7" "SOB_BOOLEAN_FALSE"))

				(add-label-to-code lbl-numbers-equal-end)
				(add-line-to-code (MOV "R0" "R7"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-closure_numbers-equal)
				(add-to-code (generate-primitive-closure lbl-numbers-equal addr))
			)))))

(define primitive-smaller
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '< fvars-table))
                      (lbl-smaller (index-label "Lprim_smaller")  )
                      (lbl-closure_smaller (index-label "Lmake_smaller")  )
                      (lbl-smaller-loop (index-label "Lprimitive_smaller_loop")  )
                      (lbl-smaller-true (index-label "Lprimitive_smaller_true")  )
                      (lbl-smaller-false (index-label "Lprimitive_smaller_false")  )
                      (lbl-smaller-end (index-label "Lprimitive_smaller_end")  )
                      )
    	(string-append
    		(add-line-to-code (JUMP lbl-closure_smaller))
				(add-label-to-code lbl-smaller)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))

				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (MOV "R1" "R6"))
				(add-line-to-code (MOV "R7" "SOB_BOOLEAN_TRUE"))
				(add-line-to-code (MOV "R2" (IMM "1")))

				(add-label-to-code lbl-smaller-loop)
				(add-line-to-code (CMP "R1" "R2"))
				(add-line-to-code (JUMP_EQ lbl-smaller-end))

				(add-line-to-code (MOV "R9" "R2"))
				(add-line-to-code (INCR "R9"))
				(add-line-to-code (MOV "R3" (FPARG "R9")))
				(add-line-to-code (INCR "R9"))
				(add-line-to-code (MOV "R4" (FPARG "R9")))
				(add-line-to-code (PUSH "R3"))
				(add-line-to-code (PUSH "R4"))
				(add-line-to-code (CALL "L_COMPARE"))

				(add-line-to-code (DROP "2"))
				(add-line-to-code (CMP "R0" "-1"))
				(add-line-to-code (JUMP_NE lbl-smaller-false))
				(add-line-to-code (INCR "R2"))
				(add-line-to-code (JUMP lbl-smaller-loop))

				(add-label-to-code lbl-smaller-false)
				(add-line-to-code (MOV "R7" "SOB_BOOLEAN_FALSE"))

				(add-label-to-code lbl-smaller-end)
				(add-line-to-code (MOV "R0" "R7"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-closure_smaller)
				(add-to-code (generate-primitive-closure lbl-smaller addr))
			)))))
	
(define primitive-bigger
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '> fvars-table))
                      (lbl-bigger (index-label "Lprim_bigger")  )
                      (lbl-closure_bigger (index-label "Lmake_bigger")  )
                      (lbl-bigger-loop (index-label "Lprimitive_bigger_loop")  )
                      (lbl-bigger-true (index-label "Lprimitive_bigger_true")  )
                      (lbl-bigger-false (index-label "Lprimitive_bigger_false")  )
                      (lbl-bigger-end (index-label "Lprimitive_bigger_end")  ))
			(string-append
				(add-line-to-code (JUMP lbl-closure_bigger))
				(add-label-to-code lbl-bigger)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))

				(add-line-to-code (MOV "R6" (FPARG "1")))
				(add-line-to-code (DECR "R6"))
				(add-line-to-code (MOV "R1" "R6"))
				(add-line-to-code (MOV "R7" "SOB_BOOLEAN_TRUE"))
				(add-line-to-code (MOV "R2" (IMM "1")))

				(add-label-to-code lbl-bigger-loop)
				(add-line-to-code (CMP "R1" "R2"))
				(add-line-to-code (JUMP_EQ lbl-bigger-end))

				(add-line-to-code (MOV "R9" "R2"))
				(add-line-to-code (INCR "R9"))
				(add-line-to-code (MOV "R3" (FPARG "R9")))
				(add-line-to-code (INCR "R9"))
				(add-line-to-code (MOV "R4" (FPARG "R9")))
				(add-line-to-code (PUSH "R3"))
				(add-line-to-code (PUSH "R4"))
				(add-line-to-code (CALL "L_COMPARE"))

				(add-line-to-code (DROP "2"))
				(add-line-to-code (CMP "R0" "1"))
				(add-line-to-code (JUMP_NE lbl-bigger-false))
				(add-line-to-code (INCR "R2"))
				(add-line-to-code (JUMP lbl-bigger-loop))

				(add-label-to-code lbl-bigger-false)
				(add-line-to-code (MOV "R7" "SOB_BOOLEAN_FALSE"))

				(add-label-to-code lbl-bigger-end)
				(add-line-to-code (MOV "R0" "R7"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-closure_bigger)
				(add-to-code (generate-primitive-closure lbl-bigger addr))
			)))))

(define primtive-symbol->string
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'symbol->string fvars-table))
                      (lbl-symbol->string (index-label "Lprim_symbol_string")  )
                      (lbl-closure_symbol->string (index-label "Lmake_symbol_string")  )
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_symbol->string))
				(add-label-to-code lbl-symbol->string)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))

				(add-line-to-code (MOV "R0" (FPARG "2")))
				(add-line-to-code (MOV "R0" (INDD "R0" "1")))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-closure_symbol->string)
				(add-to-code (generate-primitive-closure lbl-symbol->string addr))
			)))))

(define prim-cons
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'cons fvars-table))
                      (lbl-cons (index-label "Lprim_cons")  )
                      (lbl-closure_cons (index-label "Lmake_cons")  )
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure_cons))
				(add-label-to-code lbl-cons)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))

				(add-line-to-code (PUSH (FPARG "3")))
				(add-line-to-code (PUSH (FPARG "2")))
				(add-line-to-code (CALL "MAKE_SOB_PAIR"))
				(add-line-to-code (DROP "2"))
				(add-line-to-code (POP "FP"))
				(add-line-to-code "RETURN")

				(add-label-to-code lbl-closure_cons)
				(add-to-code (generate-primitive-closure lbl-cons addr))
			)))))

(define primitive-eq
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'eq? fvars-table))
                      (lbl-eq (index-label "Lprim_eq")  )
                      (lbl-closure-eq (index-label "Lmake_eq")  )
                      (lbl-eq-cmp-addr (index-label "Leq_cmp_addr"))
                      (lbl-eq-cmp-val (index-label "Leq_cmp_val"))
                      (lbl-is-eq (index-label "Lis_eq"))
                      (lbl-not-eq (index-label "Lnot_eq"))
                      (Lbl-end-eq (index-label "Lend_eq")  )
                      (lbl-is-eq-Fraction (index-label "Leq_fraction")  )
                      (lbl-is-eq-Symbol (index-label "Leq_sym")  )
                      )
			(string-append
				(add-line-to-code (JUMP lbl-closure-eq))
				(add-label-to-code lbl-eq)
				(add-line-to-code (PUSH "FP"))
				(add-line-to-code (MOV "FP" "SP"))

				(add-line-to-code (MOV "R1" (FPARG "2")))
				(add-line-to-code (MOV "R2" (FPARG "3")))
				(add-line-to-code (MOV "R5" "SOB_BOOLEAN_TRUE"))
				(add-line-to-code (CMP (INDD "R1" "0") (INDD "R2" "0")))
				(add-line-to-code (JUMP_NE lbl-not-eq))

                (add-line-to-code (CMP (INDD "R1" "0") "T_SYMBOL"))
                (add-line-to-code (JUMP_EQ lbl-is-eq-Symbol))
                (add-line-to-code (CMP (INDD "R1" "0") "T_FRACTION"))
                (add-line-to-code (JUMP_EQ lbl-is-eq-Fraction))
                (add-line-to-code (CMP (INDD "R1" "0") "T_NIL"))
                (add-line-to-code (JUMP_EQ Lbl-end-eq))
                (add-line-to-code (CMP (INDD "R1" "0") "T_VOID"))
                (add-line-to-code (JUMP_EQ Lbl-end-eq))

                (add-line-to-code (CMP (INDD "R1" "0") "T_BOOL"))
                (add-line-to-code (JUMP_EQ lbl-eq-cmp-addr))
                (add-line-to-code (CMP (INDD "R1" "0") "T_STRING"))
                (add-line-to-code (JUMP_EQ lbl-eq-cmp-addr))
                (add-line-to-code (CMP (INDD "R1" "0") "T_VECTOR"))
                (add-line-to-code (JUMP_EQ lbl-eq-cmp-addr))
                (add-line-to-code (CMP (INDD "R1" "0") "T_PAIR"))
                (add-line-to-code (JUMP_EQ lbl-eq-cmp-addr))

                (add-line-to-code (CMP (INDD "R1" "0") "T_CLOSURE"))
                (add-line-to-code (JUMP_NE lbl-eq-cmp-val))

                (add-line-to-code (CMP (INDD "R1" "0") (INDD "R2" "0")))
                (add-line-to-code (JUMP_NE lbl-not-eq))
                (add-line-to-code (CMP (INDD "R1" "1") (INDD "R2" "1")))
                (add-line-to-code (JUMP_NE lbl-not-eq))
                (add-line-to-code (CMP (INDD "R1" "2") (INDD "R2" "2")))
                (add-line-to-code (JUMP_NE lbl-not-eq))

                (add-line-to-code (JUMP Lbl-end-eq))

                (add-label-to-code lbl-eq-cmp-addr)
                (add-line-to-code (CMP "R1" "R2"))
                (add-line-to-code (JUMP_EQ Lbl-end-eq))
                (add-line-to-code (JUMP lbl-not-eq))

                (add-label-to-code lbl-eq-cmp-val)
                (add-line-to-code (CMP (INDD "R2" "1") (INDD "R1" "1")))
                (add-line-to-code (JUMP_EQ Lbl-end-eq))
                (add-line-to-code (JUMP lbl-not-eq))

                (add-label-to-code lbl-is-eq-Fraction)
                (add-line-to-code (CMP (INDD "R2" "1") (INDD "R1" "1")))
                (add-line-to-code (JUMP_NE lbl-not-eq))
                (add-line-to-code (CMP (INDD "R2" "2") (INDD "R1" "2")))
                (add-line-to-code (JUMP_EQ Lbl-end-eq))
                (add-line-to-code (JUMP lbl-not-eq))

                (add-label-to-code lbl-is-eq-Symbol)
                (add-line-to-code (CMP (INDD "R2" "1") (INDD "R1" "1")))
                (add-line-to-code (JUMP_EQ Lbl-end-eq))

                (add-label-to-code lbl-not-eq)
                (add-line-to-code (MOV "R5" "SOB_BOOLEAN_FALSE"))

                (add-label-to-code Lbl-end-eq)
                (add-line-to-code (MOV "R0" "R5"))

                (add-line-to-code (POP "FP"))
                (add-line-to-code "RETURN")

                (add-label-to-code lbl-closure-eq)
                (add-to-code (generate-primitive-closure lbl-eq addr)))))))

(define primitive-vector
  (lambda (constants-table fvars-table frst_sym_address)
    (lambda ()
        (let ((addr (search-fvar 'vector fvars-table))
                (lbl-vector (index-label "Lprim_vector"))
                (lbl-closure_vector (index-label "Lmake_vector"))
                (Lbl_vector_loop (index-label "Lvector_loop"))
                (Lbl_vector_end (index-label "Lvector_end")))
            (string-append
                    (add-line-to-code (JUMP lbl-closure_vector))
                    (add-label-to-code lbl-vector)
                    (add-line-to-code (PUSH "FP"))
                    (add-line-to-code (MOV "FP" "SP"))
                    (add-line-to-code (MOV "R1" (IMM "0")))
                    (add-line-to-code (MOV "R2" (FPARG "1-1")))
                    (add-line-to-code (MOV "R3" "R1"))
                    (add-line-to-code (ADD "R3" (IMM "2")))
                    (add-label-to-code Lbl_vector_loop)
                    (add-line-to-code (CMP "R2" "R1"))
                    (add-line-to-code (JUMP_EQ Lbl_vector_end))
                    (add-line-to-code (PUSH (FPARG "R3")))
                    (add-line-to-code (ADD "R3" (IMM "1")))
                    (add-line-to-code (ADD "R1" (IMM "1")))
                    (add-line-to-code (JUMP Lbl_vector_loop))
                    (add-label-to-code Lbl_vector_end)
                    (add-line-to-code (PUSH "R2"))
                    (add-line-to-code (CALL "MAKE_SOB_VECTOR"))
                    (add-line-to-code (ADD "R2" (IMM "1")))
                    (add-line-to-code (DROP "R2"))
                    (add-line-to-code (POP "FP"))
                    (add-line-to-code "RETURN")
                    
                    (add-label-to-code lbl-closure_vector)
                    (generate-primitive-closure lbl-vector addr) new-line)))))			
		
(define primitive-make-string
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'make-string fvars-table))
                      (lbl-make-string (index-label "Lprim_maketring")  )
                      (lbl-closure-make-string (index-label "Lmake_make_string")  )
                      (Lbl_make-string_one_param (index-label "Lmakestring_one_param")  )
                      (Lbl_make-string_loop (index-label "Lmakestring_loop")  )
                      (Lbl_make-string_end (index-label "Lmakestring_end")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-make-string))
			(add-label-to-code lbl-make-string)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R2" "0"))
			(add-line-to-code (MOV "R3" (IMM "0")))
			(add-line-to-code (MOV "R1" (FPARG "2")))
			(add-line-to-code (MOV "R1" (INDD "R1" "1")))
			(add-line-to-code (MOV "R5" (FPARG "1-1")))
			(add-line-to-code (CMP "R5" "1"))
			(add-line-to-code (JUMP_EQ Lbl_make-string_one_param))
			(add-line-to-code (MOV "R2" (FPARG "3")))
			(add-label-to-code Lbl_make-string_loop)
			(add-line-to-code (CMP "R3" "R1"))
			(add-line-to-code (JUMP_EQ Lbl_make-string_end))
			(add-line-to-code (PUSH (INDD "R2" "1")))
			(add-line-to-code (ADD "R3" (IMM "1")))
			(add-line-to-code (JUMP Lbl_make-string_loop))
			(add-label-to-code Lbl_make-string_one_param)
			(add-line-to-code (CMP "R3" "R1"))
			(add-line-to-code (JUMP_EQ Lbl_make-string_end))
			(add-line-to-code (PUSH "R2"))
			(add-line-to-code (ADD "R3" (IMM "1")))
			(add-line-to-code (JUMP Lbl_make-string_one_param))
			(add-label-to-code Lbl_make-string_end)
			(add-line-to-code (PUSH "R1"))
			(add-line-to-code (CALL "MAKE_SOB_STRING"))
			(add-line-to-code (ADD "R1" (IMM "1")))
			(add-line-to-code (DROP "R1"))
                        (add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
			(add-label-to-code lbl-closure-make-string)
                        (generate-primitive-closure lbl-make-string addr) new-line)))))

(define primitive-make-vector
  (lambda (constants-table fvars-table frst_sym_address)
    (lambda ()
        (let ((addr (search-fvar 'make-vector fvars-table))
                (lbl-make-vector (index-label "Lprim_make_vector")  )
                (lbl-closure-make-vector (index-label "Lmake_make_vector")  )
                
                (Lbl_vec_loop (index-label "Lvec_loop")  )
                (Lbl_vec_end (index-label "Lvec_end")  ))
            (string-append
                (add-line-to-code (JUMP lbl-closure-make-vector))
                (add-label-to-code lbl-make-vector)
                (add-line-to-code (PUSH "FP"))
                (add-line-to-code (MOV "FP" "SP"))
                (add-line-to-code (PUSH "2"))
                (add-line-to-code (CALL "MALLOC"))
                (add-line-to-code (DROP "1"))
                (add-line-to-code (MOV (IND "R0") "T_INTEGER"))
                (add-line-to-code (MOV (INDD "R0" "1") (IMM "0")))
                (add-line-to-code (MOV "R2" "R0"))
                (add-line-to-code (MOV "R3" (IMM "0")))
                (add-line-to-code (MOV "R1" (FPARG "2")))
                (add-line-to-code (MOV "R1" (INDD "R1" "1")))
                (add-line-to-code (MOV "R5" (FPARG "1-1")))
                (add-line-to-code (CMP "R5" "1"))
                (add-line-to-code (JUMP_EQ Lbl_vec_loop))
                (add-line-to-code (MOV "R2" (FPARG "3")))
                (add-label-to-code Lbl_vec_loop)
                (add-line-to-code (CMP "R3" "R1"))
                (add-line-to-code (JUMP_EQ Lbl_vec_end))
                (add-line-to-code (PUSH "R2"))
                (add-line-to-code (ADD "R3" (IMM "1")))
                (add-line-to-code (JUMP Lbl_vec_loop))
                (add-label-to-code Lbl_vec_end)
                (add-line-to-code (PUSH "R1"))
                (add-line-to-code (CALL "MAKE_SOB_VECTOR"))
                (add-line-to-code (ADD "R1" (IMM "1")))
                (add-line-to-code (DROP "R1"))
                (add-line-to-code (POP "FP"))
                (add-line-to-code "RETURN")
                (add-label-to-code lbl-closure-make-vector)
                (generate-primitive-closure lbl-make-vector addr) new-line)))))

(define primitive-string-set
  (lambda (constants-table fvars-table frst_sym_address)
    (lambda ()
        (let ((addr (search-fvar 'string-set! fvars-table))
                (lbl-string-set (index-label "Lprim_string_set")  )
                (lbl-closure-string-set (index-label "Lmake_string_set")  ))
        (string-append
                (add-line-to-code (JUMP lbl-closure-string-set))
                (add-label-to-code lbl-string-set)
                (add-line-to-code (PUSH "FP"))
                (add-line-to-code (MOV "FP" "SP"))
                (add-line-to-code (MOV "R0" (FPARG "2")))
                (add-line-to-code (MOV "R1" (FPARG "3")))
                (add-line-to-code (MOV "R2" (FPARG "4")))
                (add-line-to-code (MOV "R2" (INDD "R2" "1")))
                (add-line-to-code (MOV "R1" (INDD "R1" "1")))
                (add-line-to-code (ADD "R1" (IMM "2")))
                (add-line-to-code (MOV (INDD "R0" "R1") "R2"))
                (add-line-to-code (MOV "R0" "SOB_VOID"))
                (add-line-to-code (POP "FP"))
                (add-line-to-code "RETURN")
                (add-label-to-code lbl-closure-string-set)
                (generate-primitive-closure lbl-string-set addr) new-line)))))
	
(define primitive-vector-set
  (lambda (constants-table fvars-table frst_sym_address)
    (lambda ()
        (let ((addr (search-fvar 'vector-set! fvars-table))
                (lbl-vector-set (index-label "Lprim_vector_set")  )
                (lbl-closure-vector-set (index-label "Lmake_vector_set")  ))
        (string-append
                (add-line-to-code (JUMP lbl-closure-vector-set))
                (add-label-to-code lbl-vector-set)
                (add-line-to-code (PUSH "FP"))
                (add-line-to-code (MOV "FP" "SP"))
                (add-line-to-code (MOV "R0" (FPARG "2")))
                (add-line-to-code (MOV "R1" (FPARG "3")))
                (add-line-to-code (MOV "R2" (FPARG "4")))
                (add-line-to-code (MOV "R1" (INDD "R1" "1")))
                (add-line-to-code (ADD "R1" (IMM "2")))
                (add-line-to-code (MOV (INDD "R0" "R1") "R2"))
                (add-line-to-code (MOV "R0" "SOB_VOID"))
                (add-line-to-code (POP "FP"))
                (add-line-to-code "RETURN")
                (add-label-to-code lbl-closure-vector-set)
                (generate-primitive-closure lbl-vector-set addr) new-line)))))
			
(define primitive-vector-length
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-length fvars-table))
                      (lbl-vector-length (index-label "Lprim_vector_length")  )
                      (lbl-closure-vector-length (index-label "Lmake_vector_length")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-vector-length))
			(add-label-to-code lbl-vector-length)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "1")))
			(add-line-to-code (PUSH "R0"))
			(add-line-to-code (CALL "MAKE_SOB_INTEGER"))
			(add-line-to-code (DROP "1"))
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
                        (add-label-to-code lbl-closure-vector-length)
                        (generate-primitive-closure lbl-vector-length addr) new-line)))))		
			
(define primitive-vector-ref
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-ref fvars-table))
                      (lbl-vector-ref (index-label "Lprim_vector_ref")  )
                      (lbl-closure-vector-ref (index-label "Lmake_vector_ref")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-vector-ref))
			(add-label-to-code lbl-vector-ref)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R1" (FPARG "2")))
			(add-line-to-code (MOV "R2" (FPARG "3")))
			(add-line-to-code (MOV "R5" (INDD "R2" "1")))
			(add-line-to-code (ADD "R5" (IMM "2")))
			(add-line-to-code (MOV "R0" (INDD "R1" "R5")))
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")

			(add-label-to-code lbl-closure-vector-ref)
                        (generate-primitive-closure lbl-vector-ref addr) new-line)))))
			
			
			
			
(define primitive-str-length
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-length fvars-table))
                      (lbl-str-length (index-label "Lprim_str_length")  )
                      (lbl-closure-str-length (index-label "Lmake_str_length")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-str-length))
			(add-label-to-code lbl-str-length)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "1")))
			(add-line-to-code (PUSH "R0"))
			(add-line-to-code (CALL "MAKE_SOB_INTEGER"))
			(add-line-to-code (DROP "1"))
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
                        (add-label-to-code lbl-closure-str-length)
                        (generate-primitive-closure lbl-str-length addr) new-line
                        )))))
			

(define primitive-string-ref
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-ref fvars-table))
                      (lbl-string-ref (index-label "Lprim_string_ref")  )
                      (lbl-closure-string-ref (index-label "Lmake_string_ref")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-string-ref))
			(add-label-to-code lbl-string-ref)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R1" (FPARG "3")))
			(add-line-to-code (MOV "R1" (INDD "R1" "1")))
			(add-line-to-code (ADD "R1" (IMM "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "R1")))
			(add-line-to-code (PUSH "R0"))
			(add-line-to-code (CALL "MAKE_SOB_CHAR"))
			(add-line-to-code (DROP "1"))
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
			(add-label-to-code lbl-closure-string-ref)
                        (generate-primitive-closure lbl-string-ref addr) new-line)))))			

(define primtive-set-cdr!
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'set-cdr! fvars-table))
                      (lbl-set-cdr! (index-label "Lprim_setcdr")  )
                      (lbl-closure-set-cdr! (index-label "Lmake_set_cdr")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-set-cdr!))
			(add-label-to-code lbl-set-cdr!)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R1" (FPARG "2")))
			(add-line-to-code (MOV "R2" (FPARG "3")))
			(add-line-to-code (MOV (INDD "R1" "2") "R2"))
			(add-line-to-code (MOV "R0" "SOB_VOID"))
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
			(add-label-to-code lbl-closure-set-cdr!)
                        (generate-primitive-closure lbl-set-cdr! addr) new-line
			
			)))))
			
(define primtive-set-car!
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'set-car! fvars-table))
                      (lbl-set-car! (index-label "Lprim_set_car")  )
                      (lbl-closure-set-car!(index-label "Lmake_set_car")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-set-car!))
			(add-label-to-code lbl-set-car!)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R1" (FPARG "2")))
			(add-line-to-code (MOV "R2" (FPARG "3")))
			(add-line-to-code (MOV (INDD "R1" "1") "R2"))
			(add-line-to-code (MOV "R0" "SOB_VOID"))
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
			(add-label-to-code lbl-closure-set-car!)
                        (generate-primitive-closure lbl-set-car! addr) new-line)))))
			
(define primitive-procedure?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'procedure? fvars-table))
                      (lbl-procedure? (index-label "Lprim_procedure")  )
                      (lbl-closure-procedure? (index-label "Lmake_procedure")  )
                     (lbl-is-procedure (index-label "L_is_procedure")  )
                     (lbl-end-procedure (index-label "L_end_procedure")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-procedure?))
			(add-label-to-code lbl-procedure?)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "0")))
			(add-line-to-code (CMP "R0" "T_CLOSURE"))
			(add-line-to-code (JUMP_EQ lbl-is-procedure))
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))
			(add-line-to-code (JUMP lbl-end-procedure))
			(add-label-to-code lbl-is-procedure)
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))
			(add-label-to-code lbl-end-procedure)
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
		(add-label-to-code lbl-closure-procedure?)
                (generate-primitive-closure lbl-procedure? addr) new-line
		)))))
			
			
(define primitive-pair?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'pair? fvars-table))
                      (lbl-pair? (index-label "Lprim_pair")  )
                      (lbl-closure-pair? (index-label "Lmake_pair")  )
                     (lbl-is-pair (index-label "L_is_pair")  )
                     (lbl-end-pair (index-label "L_end_pair")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-pair?))
			(add-label-to-code lbl-pair?)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "0")))
			(add-line-to-code (CMP "R0" "T_PAIR"))
			(add-line-to-code (JUMP_EQ lbl-is-pair))
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))
			(add-line-to-code (JUMP lbl-end-pair))
			(add-label-to-code lbl-is-pair)
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))
			(add-label-to-code lbl-end-pair)
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
                        (add-label-to-code lbl-closure-pair?)
                (generate-primitive-closure lbl-pair? addr) new-line
		)))))
			
(define primitive-symbol?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'symbol? fvars-table))
                      (lbl-symbol? (index-label "Lprim_symbol")  )
                      (lbl-closure-symbol? (index-label "Lmake_symbol")  )
                     (lbl-is-symbol (index-label "L_is_symbol")  )
                     (lbl-end-symbol (index-label "L_end_symbol")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-symbol?))
			(add-label-to-code lbl-symbol?)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "0")))
			(add-line-to-code (CMP "R0" "T_SYMBOL"))
			(add-line-to-code (JUMP_EQ lbl-is-symbol))
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))
			(add-line-to-code (JUMP lbl-end-symbol))
			(add-label-to-code lbl-is-symbol)
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))
			(add-label-to-code lbl-end-symbol)
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
                        (add-label-to-code lbl-closure-symbol?)
                (generate-primitive-closure lbl-symbol? addr) new-line
		)))))
;; 		
(define primitive-string?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string? fvars-table))
                      (lbl-string? (index-label "Lprim_string")  )
                      (lbl-closure-string? (index-label "Lmake_string")  )
                     (lbl-is-string (index-label "L_is_string")  )
                     (lbl-end-string (index-label "L_end_string")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-string?))
			(add-label-to-code lbl-string?)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "0")))
			(add-line-to-code (CMP "R0" "T_STRING"))
			(add-line-to-code (JUMP_EQ lbl-is-string))
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))
			(add-line-to-code (JUMP lbl-end-string))
			(add-label-to-code lbl-is-string)
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))
			(add-label-to-code lbl-end-string)
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
		(add-label-to-code lbl-closure-string?)
                (generate-primitive-closure lbl-string? addr) new-line
		)))))

(define primitive-zero?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'zero? fvars-table))
                      (lbl-zero? (index-label "Lprim_zero")  )
                      (lbl-closure-zero? (index-label "Lmake_zero")  )
                     (lbl-is-zero? (index-label "L_is_zero")  )
                     (lbl-end-zero (index-label "L_end_zero")  ))
		(string-append
			(add-line-to-code (JUMP lbl-closure-zero?))
			(add-label-to-code lbl-zero?)
			(add-line-to-code (PUSH "FP"))
			(add-line-to-code (MOV "FP" "SP"))
			(add-line-to-code (MOV "R0" (FPARG "2")))
			(add-line-to-code (MOV "R0" (INDD "R0" "1")))
			(add-line-to-code (CMP "R0" (IMM "0")))
			(add-line-to-code (JUMP_EQ lbl-is-zero?))
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))
			(add-line-to-code (JUMP lbl-end-zero))
			(add-label-to-code lbl-is-zero?)
			(add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))
			(add-label-to-code lbl-end-zero)
			(add-line-to-code (POP "FP"))
			(add-line-to-code "RETURN")
		(add-label-to-code lbl-closure-zero?)
                (generate-primitive-closure lbl-zero? addr) new-line
		)))))
		
(define primitive-vector?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector? fvars-table))
                      (lbl-vector? (index-label "Lprim_vector")  )
                      (lbl-closure-vector? (index-label "Lmake_vector")  )
                     (lbl-is-vector (index-label "L_is_vector")  )
                     (lbl-end-vector (index-label "L_end_vector")  ))
		(string-append
			"JUMP("lbl-closure-vector?");" new-line
			lbl-vector?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_VECTOR);" new-line
			"	JUMP_EQ("lbl-is-vector");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-vector");" new-line
			lbl-is-vector":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-vector":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-vector?":" new-line
                (generate-primitive-closure lbl-vector? addr) new-line
		)))))

(define primitive-null?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'null? fvars-table))
                      (lbl-null? (index-label "Lprim_null")  )
                      (lbl-closure-null? (index-label "Lmake_null")  )
                     (lbl-is-null (index-label "L_is_null")  )
                     (lbl-end-null (index-label "L_end_null")  ))
		(string-append
			"JUMP("lbl-closure-null?");" new-line
			lbl-null?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_NIL);" new-line
			"	JUMP_EQ("lbl-is-null");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-null");" new-line
			lbl-is-null":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-null":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-null?":" new-line
                (generate-primitive-closure lbl-null? addr) new-line
		)))))

(define primitive-char?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'char? fvars-table))
                      (lbl-char? (index-label "Lprim_char")  )
                      (lbl-closure-char? (index-label "Lmake_char")  )
                     (lbl-is-char (index-label "L_is_char")  )
                     (lbl-end-char (index-label "L_end_char")  ))
		(string-append
			"JUMP("lbl-closure-char?");" new-line
			lbl-char?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_CHAR);" new-line
			"	JUMP_EQ("lbl-is-char");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-char");" new-line
			lbl-is-char":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-char":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-char?":" new-line
                (generate-primitive-closure lbl-char? addr) new-line
		)))) 	)		

(define primitive-integer?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'integer? fvars-table))
                      (lbl-integer? (index-label "Lprim_integer")  )
                      (lbl-closure-integer? (index-label "Lmake_integer")  )
                     (lbl-is-integer (index-label "L_is_integer")  )
                     (lbl-end-integer (index-label "L_end_integer")  ))
		(string-append
            (add-line-to-code (JUMP lbl-closure-integer?))
            (add-label-to-code lbl-integer?)
            (add-line-to-code (PUSH "FP"))
            (add-line-to-code (MOV "FP" "SP"))
            (add-line-to-code (MOV "R0" (FPARG "2")))
            (add-line-to-code (MOV "R0" (INDD "R0" "0")))
            (add-line-to-code (CMP "R0" "T_INTEGER"))
            (add-line-to-code (JUMP_EQ lbl-is-integer))
            (add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))
            (add-line-to-code (JUMP lbl-end-integer))

            (add-label-to-code lbl-is-integer)
            (add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))

            (add-label-to-code lbl-end-integer)
            (add-line-to-code (POP "FP"))
            (add-line-to-code "RETURN")

            (add-label-to-code lbl-closure-integer?)
            (add-to-code (generate-primitive-closure lbl-integer? addr))
		)))))
		
(define primitive-boolean?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'boolean? fvars-table))
                      (lbl-boolean? (index-label "Lprim_boolean")  )
                      (lbl-closure-boolean? (index-label "Lmake_boolean")  )
                     (lbl-is-boolean (index-label "L_is_boolean")  )
                     (lbl-end-boolean (index-label "L_end_boolean")  ))
		(string-append
            (add-line-to-code (JUMP lbl-closure-boolean?))
            (add-label-to-code lbl-boolean?)
            (add-line-to-code (PUSH "FP"))
            (add-line-to-code (MOV "FP" "SP"))
            (add-line-to-code (MOV "R0" (FPARG "2")))
            (add-line-to-code (MOV "R0" (INDD "R0" "0")))
            (add-line-to-code (CMP "R0" "T_BOOL"))
            (add-line-to-code (JUMP_EQ lbl-is-boolean))
            (add-line-to-code (MOV "R0" "SOB_BOOLEAN_FALSE"))
            (add-line-to-code (JUMP lbl-end-boolean))

            (add-label-to-code lbl-is-boolean)
            (add-line-to-code (MOV "R0" "SOB_BOOLEAN_TRUE"))

            (add-label-to-code lbl-end-boolean)
            (add-line-to-code (POP "FP"))
            (add-line-to-code "RETURN")

            (add-label-to-code lbl-closure-boolean?)
            (add-to-code (generate-primitive-closure lbl-boolean? addr))
		)))))

(define primitive-char->integer
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'char->integer fvars-table))
                      (lbl-char->integer (index-label "Lprim_char_integer")  )
                      (lbl-closure-char->integer (index-label "Lmake_char_integer")  ))
		(string-append
            (add-line-to-code (JUMP lbl-closure-char->integer))
            (add-label-to-code lbl-char->integer)
            (add-line-to-code (PUSH "FP"))
            (add-line-to-code (MOV "FP" "SP"))
            (add-line-to-code (MOV "R0" (FPARG "2")))
            (add-line-to-code (MOV "R0" (INDD "R0" "1")))
            (add-line-to-code (PUSH "R0"))
            (add-line-to-code (CALL "MAKE_SOB_INTEGER"))
            (add-line-to-code (DROP "1"))
            (add-line-to-code (POP "FP"))
            (add-line-to-code "RETURN")

            (add-label-to-code lbl-closure-char->integer)
            (add-to-code (generate-primitive-closure lbl-char->integer addr))
		)))))
			
(define primitive-integer->char
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'integer->char fvars-table))
                      (lbl-integer->char (index-label "Lprim_integer_char")  )
                      (lbl-closure-integer->char (index-label "Lmake_integer_char")  ))
		(string-append
            (add-line-to-code (JUMP lbl-closure-integer->char))
            (add-label-to-code lbl-integer->char)
            (add-line-to-code (PUSH "FP"))
            (add-line-to-code (MOV "FP" "SP"))
            (add-line-to-code (MOV "R0" (FPARG "2")))
            (add-line-to-code (MOV "R0" (INDD "R0" "1")))
            (add-line-to-code (PUSH "R0"))
            (add-line-to-code (CALL "MAKE_SOB_CHAR"))
            (add-line-to-code (DROP "1"))
            (add-line-to-code (POP "FP"))
            (add-line-to-code "RETURN")

            (add-label-to-code lbl-closure-integer->char)
            (add-to-code (generate-primitive-closure lbl-integer->char addr))
		)))))
		
(define primitive-car
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'car fvars-table))
                      (lbl-car (index-label "Lprim_car")  )
                      (lbl-closure-car (index-label "Lmake_primitive_car")  ))
                      
		(string-append
            (add-line-to-code (JUMP lbl-closure-car))
            (add-label-to-code lbl-car)
            (add-line-to-code (PUSH "FP"))
            (add-line-to-code (MOV "FP" "SP"))
            (add-line-to-code (MOV "R0" (FPARG "2")))
            (add-line-to-code (MOV "R0" (INDD "R0" "1")))
            (add-line-to-code (POP "FP"))
            (add-line-to-code "RETURN")

            (add-label-to-code lbl-closure-car)
            (add-to-code (generate-primitive-closure lbl-car addr))
			)))))
			
(define primitive-cdr
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'cdr fvars-table))
                      (lbl-cdr (index-label "Lprim_cdr")  )
                      (lbl-closure-cdr (index-label "Lmake_primitive_cdr")  ))
                      
		(string-append
            (add-line-to-code (JUMP lbl-closure-cdr))
            (add-label-to-code lbl-cdr)
            (add-line-to-code (PUSH "FP"))
            (add-line-to-code (MOV "FP" "SP"))
            (add-line-to-code (MOV "R0" (FPARG "2")))
            (add-line-to-code (MOV "R0" (INDD "R0" "2")))
            (add-line-to-code (POP "FP"))
            (add-line-to-code "RETURN")

            (add-label-to-code lbl-closure-cdr)
            (add-to-code (generate-primitive-closure lbl-cdr addr))
			)))))

(define prim-string-to-symbol
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string->symbol fvars-table))
                      (lbl-str->sym (index-label "Lprim_str_sym")  )
                      (lbl-closure-str->sym (index-label "Lmake_prim_str_sym")  )
                      (lbl-done (index-label "L_str_sym_done")  )
                      (lbl-loop (index-label "L_str_sym_loop"))
                      (lbl-new-sym (index-label "L_str_sym_new_sym"))
                      )
                      
		(string-append
            (add-line-to-code (JUMP lbl-closure-str->sym))
            (add-label-to-code lbl-str->sym)
            (add-line-to-code (PUSH "FP"))
            (add-line-to-code (MOV "FP" "SP"))
            (add-line-to-code (PUSH "R11"))
            (add-line-to-code (PUSH "R12"))
            (add-line-to-code (PUSH "R13"))
            (add-line-to-code (PUSH "R14"))
            (add-line-to-code (MOV "R12" (FPARG "2")))
            (add-line-to-code (MOV "R11" (number->string frst_sym_address)))

            (add-label-to-code lbl-loop)
            (add-line-to-code (CMP "R11" (IMM "-1")))
            (add-line-to-code (JUMP_EQ lbl-new-sym))
            (add-line-to-code (MOV "R13" (INDD "R11" "1")))
            (add-line-to-code (MOV "R0" "R11"))

            (add-line-to-code (MOV "R15" "SOB_BOOLEAN_FALSE"))
            (add-line-to-code (CMP (INDD "R13" "1") (INDD "R12" "1")))
            (add-line-to-code (JUMP_NE "L_compare_strings_finish"))
            (add-line-to-code (MOV "R3" (INDD "R13" "1")))
            (add-line-to-code (ADD "R3" "2"))
            (add-line-to-code (MOV "R4" "2"))

            (add-label-to-code "L_compare_strings_loop")
            (add-line-to-code (CMP "R4" "R3"))
            (add-line-to-code (JUMP_EQ "L_compare_strings_true"))
            (add-line-to-code (CMP (INDD "R13" "R4") (INDD "R12" "R4")))
            (add-line-to-code (JUMP_NE "L_compare_strings_finish"))
            (add-line-to-code (INCR "R4"))
            (add-line-to-code (JUMP "L_compare_strings_loop"))

            (add-label-to-code "L_compare_strings_true")
            (add-line-to-code (MOV "R15" "SOB_BOOLEAN_TRUE"))

            (add-label-to-code "L_compare_strings_finish")
            (add-line-to-code (CMP "R15" "SOB_BOOLEAN_TRUE"))
            (add-line-to-code (JUMP_EQ lbl-done))
            (add-line-to-code (MOV "R14" "R11"))
            (add-line-to-code (MOV "R11" (INDD "R11" "2")))
            (add-line-to-code (JUMP lbl-loop))

            (add-label-to-code lbl-new-sym)
            (add-line-to-code (PUSH (IMM "3")))
            (add-line-to-code (CALL "MALLOC"))
            (add-line-to-code (DROP "1"))
            (add-line-to-code (MOV (INDD "R14" "2") (IMM "R0")))
            (add-line-to-code (MOV (IND "R0") "T_SYMBOL"))
            (add-line-to-code (MOV (INDD "R0" "1") (IMM "R12")))
            (add-line-to-code (MOV (INDD "R0" "2") (IMM "-1")))

            (add-label-to-code lbl-done)
            (add-line-to-code (POP "R14"))
            (add-line-to-code (POP "R13"))
            (add-line-to-code (POP "R12"))
            (add-line-to-code (POP "R11"))

            (add-line-to-code (POP "FP"))
            (add-line-to-code "RETURN")

            (add-label-to-code lbl-closure-str->sym)
            (add-to-code (generate-primitive-closure lbl-str->sym addr))
			)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define initiate-memory
    (lambda (const-tab_ fvar-tab_)
    (let* (
           (num-of-constansts (get-length-of-const-table const-tab_))
           (first-address (caar const-tab_))
           (last-address (+ first-address num-of-constansts (get-length-of-fvar-table fvar-tab_) 1)))
      (string-append
      new-line
      (add-line-to-code (string-append "long mem["(number->string num-of-constansts)"] = {"(create-cs const-tab_)"}"))
      (add-line-to-code (string-append "memcpy(&ADDR("(number->string first-address)"), mem, sizeof(mem))"))
      (add-line-to-code (MOV (IND "0") (number->string last-address)))
       ))))

(define epilogue
  (string-append
    (add-label-to-code program-end-label)
    (add-line-to-code "STOP_MACHINE")
    (add-line-to-code "return 0")
    (add-line-to-code "}")
))

(define gen-epilogue-sexpr
  (lambda (lbl) 
  (let ((L_epilogueSexpr (index-label lbl))
	(DOne_label (index-label "Done__lbl"))
  )
      (string-append
        (add-label-to-code L_epilogueSexpr)
        (add-line-to-code (CMP "R0" "SOB_VOID"))
        (add-line-to-code (JUMP_EQ DOne_label))
        (add-line-to-code (PUSH "R0"))
        (add-line-to-code (CALL "WRITE_SOB"))
        (add-line-to-code (CALL "NEWLINE"))
        (add-line-to-code (DROP "1"))

        (add-label-to-code DOne_label)
       ))))
 
(define compile-scheme-file      
  (lambda (src target)
  (let* ( (sexprs  (test-s <sexpr> (file->string src)))
	    (with-support-sexprs (test-s <sexpr> (file->string "SchemePrims.scm")))
	    (pe-sexprs-list (map (lambda (sexpr) (ass3-parser sexpr)) sexprs ))	    
	    (pe-With-support (map (lambda (expr)
                          (ass3-parser expr))
                        with-support-sexprs))                       
             (pe-lst (append pe-With-support pe-sexprs-list)) 
            ; (pe-lst pe-sexprs-list)
             (constants-table (create-const-table pe-lst 300))
             (constants-table-length (get-length-of-const-table constants-table))
             (init_fvars (Fvars-init (+ 300 constants-table-length)))
             (inits-fvars-withOut_address (Fvars-init-withoutadresses init_fvars))
              (init_fvars_length (length init_fvars))
              (fvars-table (create-fvar-table pe-lst (+ 300 constants-table-length init_fvars_length) inits-fvars-withOut_address))
              (fvars-table (append init_fvars fvars-table))
            (memory-init (initiate-memory constants-table fvars-table))
              (addr-of-first-sym (First-sym-addr (reverse constants-table)))
              (prologue (make-prologue constants-table fvars-table addr-of-first-sym))
              (last-address (+ 300 constants-table-length init_fvars_length 1))
              (outCode 
              (apply string-append (map
                                    (lambda (x)
                                        (string-append
                                         (code-gen x 0 0 constants-table fvars-table "L_epilogueSexpr")
                                         (gen-epilogue-sexpr "L_epilogueSexpr")))
                                    pe-lst)))
              (final-code (string-append prologue memory-init outCode epilogue ))
	    ) 
        (write-to-file target final-code))))