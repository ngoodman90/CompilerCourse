
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

(define my-map 
  (lambda (func lst)
    (if (null? lst)
        '()
        (cons (func (car lst)) (my-map func (cdr lst))))))

(define e-n-d-2
  (lambda (lst)
    (e-n-d-3 (e-n-d-1 lst
        (lambda (x y)
            (if (null? x) 
                lst
                `(,(transform_letrec_to_lambda `(letrec ,(my-map (lambda (i)  `(,(cadr (cadr i)) ,(caddr i))) x) ,@y)))))))))

(define e-n-d-3
  (lambda (lst)     
    (my-map
        (lambda (x)
            (cond ((or (null? x) (not (list? x))) x)
                ((or (lambda-simple? x) (lambda-var? x)) `(,(car x) ,(cadr x) ,@(e-n-d-2 (cddr x))))
                ((lambda-opt? x) `(,(car x) ,(cadr x) ,(caddr x) ,@(e-n-d-2 (cdddr x))))
                (else (e-n-d-3 x))))
            lst)))

(define eliminate-nested-defines
  (lambda (lst) 
    (car (e-n-d-3 `(,lst)))))

; ####################################### Eliminate Nested Defines #######################################

;; (define first-part
;;     (lambda (begining ending) begining))
;;     
;; (define second-part
;;     (lambda (begining ending) ending))
;;     
;; (define perform-define-split
;;     (lambda (var func)
;;         (if (not (null? var))
;;             (perform-define-split
;;                 (cdr var)
;;                 (lambda (expr1 expr2)
;;                         (if (eq? (caar var) 'def)
;;                             (func (cons (car var) expr1) expr2)
;;                             (if (eq? (caar var) 'seq)
;; 			        (perform-define-split
;;                                     (cadar var)
;;                                     (lambda (expr3 expr4)
;;                                             (func (append expr3 expr1)
;;                                                   (append expr4 expr2))))
;;                                 (func expr1 (cons (car var) expr2))))))
;;             (func '() '()))))
;;                  
;; (define handle-sequence
;;     (lambda (expr1 expr2)
;;         `(seq (,@(map (lambda (a) `(set ,(cadr a) ,(eliminate-nested-defines (caddr a)))) expr1)
;;                ,@(map eliminate-nested-defines expr2)))))
;;             
;; (define eliminate-inner-lambda-nested-defines
;;     (lambda (content)
;;         (let ((expr1 (perform-define-split content first-part))
;;               (expr2 (perform-define-split content second-part)))
;;              (if (not (null? expr1))
;;                 `((applic (lambda-simple ,(map cadadr expr1) ,(handle-sequence expr1 expr2)) ,(map (lambda (a) '(const #f)) expr1)))
;;                 (map eliminate-nested-defines content)))))
;;       
;; (define eliminate-nested-defines
;;     (lambda (expr)
;;         (let
;;             ((first-expr (car expr))
;;              (rest-expr (cdr expr)))
;;             (cond ((or (eq? first-expr 'const) (eq? first-expr 'var)) expr)
;;                   ((eq? first-expr 'seq) (map eliminate-nested-defines (car rest-expr)))
;;                   ((eq? first-expr 'def) `(def ,(car rest-expr) ,(eliminate-nested-defines (cadr rest-expr))))
;;                   ((eq? first-expr 'lambda-simple) `(lambda-simple ,(car rest-expr) ,@(eliminate-inner-lambda-nested-defines (cdr rest-expr))))
;;                   ((eq? first-expr 'lambda-var) `(lambda-var ,(car rest-expr) ,@(eliminate-inner-lambda-nested-defines (cdr rest-expr))))
;;                   ((eq? first-expr 'lambda-opt) `(lambda-opt ,(car rest-expr) ,(cadr rest-expr) ,@(eliminate-inner-lambda-nested-defines (cddr rest-expr))))
;;                   ((eq? first-expr 'applic) `(applic ,@(map eliminate-nested-defines rest-expr)))
;;                   ((list? first-expr) (list* (eliminate-nested-defines first-expr) (map eliminate-nested-defines rest-expr)))
;;                   (else expr)))))

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
        
(define box-set_help
  (lambda (lambdas)
    (let* ((parama1 (get_lambda_parameters lambdas))
            (body  (get_lambda_body lambdas))  
        
        (lambdd (get_lambda lambdas parama1 body))
        
        (body_1  (cond ((eq? (car lambdd) 'lambda-simple)  (lambda-simple-body lambdd))
                            ((eq? (car lambdd) 'lambda-opt) 	(lambda-opt-body lambdd))
                            ((eq? (car lambdd) 'lambda-var) (lambda-var-body lambdd))   ))
        ;this willl give us list of if the three conditions occur ==> (set (pvar name minor) (box (pvar name minor)))	
        (sets_to_add_after_lambda 
        (filter (lambda (t) (not (null? t))) (my-map 
                                    (lambda (p)
                (if (and  (bound? body `(var ,p))  (var_set? body `(var ,p)) (variable_appears? body `(var ,p))) 
                        `(set (var ,p) (box (var ,p)))
                        '())) parama1))))

        ((lambda (xxx) 
            (cond ((lambda-var? xxx)
            `( ,(car xxx) ,@parama1 ,(caddr xxx)))
                ((lambda-opt? xxx)
                `( ,(car xxx) ,(reverse (cdr (reverse parama1))) ,(car (reverse parama1)) ,(caddr xxx)) )
            ((lambda-simple? xxx)
            `( ,(car xxx) ,parama1 ,(caddr xxx)))
        
        ))
        ;;now we want to add 
        (if (> (length sets_to_add_after_lambda)  0)
        (if (equal? (car body_1) 'seq)
            `( ,(car lambdd) ,parama1   (seq (,@sets_to_add_after_lambda ,@(cadr  body_1))))
        `( ,(car lambdd) ,parama1   (seq (,@sets_to_add_after_lambda , body_1))))
        `( ,(car lambdd) ,parama1   , body_1))))))
      
(define box-set_
  (lambda (eliminate_pes)
     (map
        (lambda (x)
            (if (or (null? x) (not (list? x) ) )
                x
                (if (lambda-expr? x) 
                    (box-set_ (box-set_help x)) 
                    (box-set_ x))))
        eliminate_pes)))
   
(define box-set
          (lambda (eliminate_pes)
    (car (box-set_ `(,eliminate_pes)))))
    
; ####################################### Boxing of Variables #######################################
      
(define not-pair-or-null?
    (lambda (expr)
        (or (not (pair? expr)) (null? expr))))

(define loop
    (lambda (func var expr)
        (or (func var (car expr)) (func var (cdr expr)))))
        
(define lambda-simple-condition?
    (lambda (var expr)
        (and (equal? 'lambda-simple (car expr)) (member var (cadr expr)))))
        
(define lambda-opt-condition?
    (lambda (var expr)
        (and (equal? 'lambda-opt (car expr)) (or (member var (cadr expr)) (equal? var (caddr expr))))))
        
(define lambda-var-condition?
    (lambda (var expr)
        (and (equal? 'lambda-var (car expr)) (equal? var (cadr expr)))))
        
(define inner-member-to-bound?
    (lambda (var expr)
        (cond ((equal? var expr) #t)
            ((not-pair-or-null? expr) #f)
            ((equal? (car expr) 'const) #f)
            ((member var expr) #t)
            ((lambda-simple-condition? var expr) #f)
            ((lambda-opt-condition? var expr) #f)
            ((lambda-var-condition? var expr) #f)
            (else (loop inner-member-to-bound? var expr)))))
            
(define to-bound?
    (lambda (expr var)
        (cond ((not-pair-or-null? expr) #f)
            ((equal? 'lambda-simple (car expr))
                (and (inner-member-to-bound? var (caddr expr)) (not (member var (cadr expr)))))
            ((equal? 'lambda-opt (car expr))
                (and (inner-member-to-bound? var (cadddr expr)) (not (or (member var (cadr expr)) (equal? var (caddr expr))))))
            ((equal? 'lambda-var (car expr))
                (and (inner-member-to-bound? var (caddr expr)) (not (equal? var (cadr expr)))))
            (else (loop to-bound? var expr)))))
            
(define variable-to-read?
    (lambda (expr var)
        (cond ((equal? var expr) #t)
            ((not-pair-or-null? expr) #f)
            ((equal? (car expr) 'const) #f)
            ((equal? (car expr) 'set) (variable-to-read? (cddr expr) var))
            ((lambda-simple-condition? var expr) #f)
            ((lambda-opt-condition? var expr) #f)
            ((lambda-var-condition? var expr) #f)
            (else (loop variable-to-read? var expr)))))

(define variable-to-write?
    (lambda (expr var)
        (cond ((not-pair-or-null? expr) #f)
            ((and (equal? (car expr) 'set) (equal? (cadadr expr) var)) #t)
            ((lambda-simple-condition? var expr) #f)
            ((lambda-opt-condition? var expr) #f)
            ((lambda-var-condition? var expr) #f)
            (else (loop variable-to-write? var expr)))))

(define remove
    (lambda (lst part-to-remove)
        (if (null? lst)
            '()
            (if (equal? (car lst) part-to-remove)
                (remove (cdr lst) part-to-remove)
                (append (list (car lst)) (remove (cdr lst) part-to-remove))))))

(define remove-parts
    (lambda (lst part-to-remove)
        (if (null? part-to-remove) lst
            (remove-parts (remove lst (car part-to-remove)) (cdr part-to-remove)))))

(define inner-boxing
    (lambda (expr list-vars)
        (cond ((or (not (pair? expr)) (null? expr) (null? list-vars)) expr)
            ((and (equal? (car expr) 'var) (member (cadr expr) list-vars)) `(box-get ,expr))
            ((and (equal? (car expr) 'set) (equal? (caadr expr) 'var) (member (cadadr expr) list-vars)) 
            `(box-set ,(cadr expr) ,@(inner-boxing (cddr expr) list-vars)))
            ((equal? 'lambda-simple (car expr))  
            `(,(car expr) ,(cadr expr) ,(inner-boxing (caddr expr) (remove-parts list-vars (cadr expr)))))
            ((equal? 'lambda-opt (car expr))
            `(,(car expr) ,(cadr expr) ,(caddr expr) ,(inner-boxing (cadddr expr) (remove-parts list-vars (cons (caddr expr) (cadr expr))))))
            ((equal? 'lambda-var (car expr))
            `(,(car expr) ,(cadr expr) ,(inner-boxing (caddr expr) (remove-parts list-vars (list (cadr expr))))))
            (else `(,(inner-boxing (car expr) list-vars) ,@(inner-boxing (cdr expr) list-vars))))))

(define perform-boxing?
    (lambda (expr var)
        (and (to-bound? expr var) (variable-to-read? expr var) (variable-to-write? expr var))))

(define create-box
    (lambda (boxed-expr)
        (map (lambda (a) `(set (var ,a) (box (var ,a)))) boxed-expr)))
        
(define lambda-var-boxing
    (lambda (expr)
        (if (perform-boxing? (cddr expr) (cadr expr))
            `(,(create-box (list (cadr expr))) ,(inner-boxing (cddr expr) (list (cadr expr))))
            '(()()))))
    
(define lambda-opt-boxing
    (lambda (expr)
        (set! boxed-list (filter (lambda (x)  (perform-boxing? (cdddr expr) x)) (cons (caddr expr) (cadr expr))))
        `(,(create-box boxed-list) ,(inner-boxing (cdddr expr) boxed-list))))
        
(define lambda-simple-boxing
    (lambda (expr)
        (set! boxed-list (filter (lambda (x)  (perform-boxing? (cddr expr) x)) (cadr expr)))
        `(,(create-box boxed-list) ,(inner-boxing (cddr expr) boxed-list))))
         
(define lambda-box-set
    (lambda (boxed-variables)
        (if (equal? 'seq (caaadr boxed-variables))
            (box-set (cadar (cadr boxed-variables)))
            (box-set (cadr boxed-variables)))))
         
;; (define box-set
;;     (lambda (expr)
;;         (cond   ((not-pair-or-null? expr) expr)                                                                                 ; do nothing
;;                 ((and (equal? 'lambda-simple (car expr)) (< 2 (length expr)) (not (null? (car (lambda-simple-boxing expr)))))   ; lambda-simple
;;                  (begin (set! boxed-variables (lambda-simple-boxing expr))
;;                         `(,(car expr) ,(cadr expr) (seq (,@(car boxed-variables) ,@(lambda-box-set boxed-variables))))))
;;                 ((and (equal? 'lambda-opt (car expr)) (< 2 (length expr)) (not (null? (car (lambda-opt-boxing expr)))))         ; lambda-opt
;;                  (begin (set! boxed-variables (lambda-opt-boxing expr)) 
;;                         `(,(car expr) ,(cadr expr) ,(caddr expr) (seq (,@(car boxed-variables) ,@(lambda-box-set boxed-variables))))))
;;                 ((and (equal? 'lambda-var (car expr)) (< 2 (length expr)) (not (null? (car (lambda-var-boxing expr)))))         ; lambda-var
;;                  (begin (set! boxed-variables (lambda-var-boxing expr))
;;                         `(,(car expr) ,(cadr expr) (seq (,@(car boxed-variables) ,@(lambda-box-set boxed-variables))))))
;;                 (else `(,(box-set (car expr)) ,@(box-set (cdr expr)))))))                                                       ; everything else
	      
; ####################################### Removing Redundant Applications #######################################
    
(define null-or-not-pair?
  (lambda (expr)
    (or (null? expr) (not (pair? expr)))))
    
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

; ####################################### Annotating Variables With Their Lexical Address #######################################

            
;;here we implement part 6

;good
(set! c_lambdas_counter -1)		  
(define change_pvar
    (lambda (lambds var level place)
    (my-map (lambda (x)
    (if (and (not (null? x)) (list? x))
	      
	    (if (lambda-expr? x ) (change_pvar x  var (+ 1 level) place)
	    (if (and (= 0 level) (equal? x var) )
		`(pvar ,(cadr var) ,place)
		(if (and (not (= 0 level)) (equal? x var) )
		  `(bvar ,(cadr var) ,(-  level 1) ,place)
		  (change_pvar x  var level place)
		  
		)
		))
		 
		  x)
	   ) lambds)
    
	))
	
(define it
  (lambda (lambdas parms number)
    (if (null? parms)
        lambdas
        (if (equal? void parms)
        (it (change_pvar lambdas  `(var ,(car parms)) 0  number) '() (+ 1 number))
    (it (change_pvar lambdas  `(var ,(car parms)) 0  number) (cdr parms) (+ 1 number))
      )))
)


(define pe->lex-pe_help
      (lambda (lambdas)
	  (let* ((parama1 (cond ((eq? (car lambdas) 'lambda-simple)  (lambda-simple-parameters lambdas))
				  ((eq? (car lambdas) 'lambda-opt) 	(lambda-opt-parameters lambdas))
				  ((eq? (car lambdas) 'lambda-var) (lambda-var-parameters lambdas))   ))
	      
		
		;(lambdd (it  lambdas parama1 0))
		
		
		;this willl give us list of if the three conditions occur ==> (set (pvar name minor) (box (pvar name minor)))	
		
		)
	
                (it  lambdas parama1 0)
		 ;lambdd 
	
				  
      )))
      
(define var_toFvar
  (lambda (pes)
     (my-map (lambda (y)
	    (if (or (null? y) (not (list? y) ) )
		  y
		 (if (equal? (car y) 'var) 
		 `(fvar ,(cadr y))
		 (var_toFvar y)
	      
	      )
	      ))pes)))
      
(define pe->lex-pe_rec
      (lambda (pes)
     (my-map (lambda (y)
	    (if (or (null? y) (not (list? y) ) )
		  y
		 (if (lambda-expr? y) 
		 (pe->lex-pe_help (pe->lex-pe_rec y))
		 (pe->lex-pe_rec y))))pes)))
	      
(define pe->lex-pe
          (lambda (pes)
    (car (var_toFvar(pe->lex-pe_rec `(,pes))))))

; ####################################### Annotating Variables With Their Lexical Address #######################################

(define tag-var?
  (lambda (expr)
    (equal? 'var (car expr))))

(define this-var?
  (lambda (expr var)
    (and (tag-var? expr) (equal? var (cadr expr)))))

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

;; (define pe->lex-pe
;;   (lambda (expr)
;;     (cond ((null-or-not-pair? expr) expr)
;;           ((tag-var? expr) `(fvar ,@(cdr expr)))
;;           ((lambda-expr? expr) (lex-pe-lambda expr))
;;           (else `(,(pe->lex-pe (car expr)) ,@(pe->lex-pe (cdr expr)))))
;;     ))
    
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
                  (lbl-else (label-marker "L_IF3_else_"))
                  (lbl-exit (label-marker "L_IF3_exit_")))
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
                  (lbl-exit (label-marker "L_or_exit_"))
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
                  (lbl-proc (label-marker "L_proc_"))
                  (lbl-error (label-marker "L_proc_err_"))
                  (lbl-no-error (label-marker "L_no_proc_err_"))
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
            (lbl-cpy-program (label-marker "lbl_cpy_program")) 
            (lbl-code (label-marker "label_code_"))
            (lbl-exit (label-marker "label_exit_"))
            (lbl-copy-loop (label-marker "label_copy_loop_"))
            (lbl-end-copy-loop (label-marker "label_end_copy_loop_"))
            (lbl-copy-parms-loop (label-marker "label_copy_parms_loop_")) 
            (lbl-end-copy-parms-loop (label-marker "label_end_copy_parms_loop_"))
            (lbl-third-loop (label-marker "lbl_third_loop_")); 
            (lbl-end-third-loop (label-marker "lbl_end_third_loop_")) 
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
                  (L-tc-loop (label-marker "L_tc_applic"))
                  (L-tc-exit (label-marker "L_tc_exit"))
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
(define label-marker
  (lambda (label-name)
        (set! number (+ number 1))
        (string-append label-name
                       (number->string number))))
                       
(define program-end-label (label-marker "L_end_prog")) 

(define void-object (if #f #f))

(define make-prologue
  (lambda (constants-table fvars-table frst_sym_address)
    (let ((cont_label_ (label-marker "Cont_label__"))
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
     			(add-label-to-code "COMPARE_label")
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

;new impelementation
(define primitive-apply
  (lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'apply fvars-table))
		      (lbl-apply (label-marker "Lprim_apply") )
                      (lbl-closure_apply (label-marker "LPRIM_CLOSURE_apply") )
                      (lbl-end_apply (label-marker "LPRIM_end_apply") )
                      )
			(string-append
			"JUMP("lbl-closure_apply");" new-line
			lbl-apply":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP,SP);" new-line
			"MOV(R15,FPARG(-2)); \n"
			"MOV(R4,FP); \n"
			"SUB(R4,IMM(4)) \n; SUB(R4,FPARG(1)); \n"
			"	MOV(R3,FPARG(2));" new-line ;  R3 holds the function 
			"	MOV(R0,FPARG(3)); " new-line ;// R0 holds the list
			"	MOV(R7,IMM(0));" new-line

			"Lprim_apply_list_loop:" new-line
			"	CMP(R0,SOB_NIL);" new-line
			"	JUMP_EQ(Lend_apply_list_loop);" new-line
			"	MOV(R1,INDD(R0,1));" new-line
			"	PUSH(R1);" new-line
			"	MOV(R0,INDD(R0,2));" new-line
			"	ADD(R7,IMM(1));" new-line
			"	JUMP(Lprim_apply_list_loop);" new-line

			"Lend_apply_list_loop:" new-line
                        "       PUSH(SOB_NIL); \n"
			"	MOV(R5,SP);" new-line 
			"	SUB(R5,IMM(1));" new-line
			"MOV(R9,FP);\n"

			"Lreverse_params:" new-line
			"	CMP(R9,R5);" new-line
			"	JUMP_GE(Lapply_in_tp);" new-line
			"	MOV(R6,STACK(R5));" new-line
			"	MOV(STACK(R5),STACK(R9));" new-line
			"	MOV(STACK(R9),R6);" new-line
			"	SUB(R5,IMM(1));" new-line
			"	ADD(R9,IMM(1));" new-line
			"	JUMP(Lreverse_params);" new-line

			"Lapply_in_tp:" new-line
			"PUSH(R7+1);\n"
			"PUSH(INDD(R3,1));\n"
			"PUSH(FPARG(-1));\n"
			"MOV(R5,FP);\n"
		
			"Lapply_in_tp_loop:" new-line
				"	CMP(R5,SP);" new-line
				"	JUMP_EQ(Lend_prim_apply);" new-line
				"	MOV(STACK(R4),STACK(R5));" new-line
				"	ADD(R4,IMM(1));" new-line
				"	ADD(R5,IMM(1));" new-line
				"
				JUMP(Lapply_in_tp_loop);" new-line

			"Lend_prim_apply:" new-line
			"MOV(SP, R4); \n"
			"MOV(FP,R15); \n" 
			"	JUMPA(INDD(R3,2));" new-line
			"POP(FP);\n"
			"RETURN;\n"
			lbl-closure_apply":"  new-line
                        (generate-primitive-closure lbl-apply addr) new-line)))))

(define primitive-symbol-string
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'symbol->string fvars-table))
                      (lbl-symbol-string (label-marker "Lprim_symbolstringr") )
                      (lbl-closure_symbol-string (label-marker "LPRIM_CLOSURE_symbol_string") )
                      (lbl-end_symbol-string (label-marker "LPRIM_end_symbolstring") )
                      )
			(string-append
			"JUMP("lbl-closure_symbol-string");" new-line
			lbl-symbol-string":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			lbl-end_symbol-string":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_symbol-string":" new-line
                        (generate-primitive-closure lbl-symbol-string addr) new-line
			)))))

(define primitive-number
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'number? fvars-table))
                      (lbl-number (label-marker "Lprim_number") )
                      (lbl-closure_number (label-marker "LPRIM_CLOSURE_number") )
                      (lbl-end_number (label-marker "LPRIM_end_number") )
                      )
			(string-append
			"JUMP("lbl-closure_number");" new-line
			lbl-number":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			"	MOV(R1,FPARG(2));" new-line
			"	CMP(IND(R1),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-end_number");" new-line
			"	CMP(IND(R1),T_FRACTION);" new-line
			"	JUMP_EQ("lbl-end_number");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
		
			lbl-end_number":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_number":" new-line
                        (generate-primitive-closure lbl-number addr) new-line
			)))))

(define primitive-remainder
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'remainder fvars-table))
                      (lbl-remainder (label-marker "Lprim_remainder") )
                      (lbl-closure_remainder (label-marker "LPRIM_CLOSURE_remainder") )
                      (lbl-end_remainder (label-marker "LPRIM_end_remainder") )
                      )
			(string-append
			"JUMP("lbl-closure_remainder");" new-line
			lbl-remainder":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R1,FPARG(3));" new-line
			"	MOV(R0,INDD(R0,1)); " new-line
			"	MOV(R1,INDD(R1,1));" new-line
			"	REM(R0,R1);" new-line
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_INTEGER);" new-line
			"	DROP(1);" new-line
			lbl-end_remainder":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_remainder":" new-line
                        (generate-primitive-closure lbl-remainder addr) new-line
			)))))



(define primitive-rational
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'rational? fvars-table))
                      (lbl-rational (label-marker "Lprim_rational") )
                      (lbl-closure_rational (label-marker "LPRIM_CLOSURE_rational") )
                      (lbl-end_rational (label-marker "LPRIM_end_rational") )
                      )
			(string-append
			"JUMP("lbl-closure_rational");" new-line
			lbl-rational":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			"	MOV(R1,FPARG(2));" new-line
			"	CMP(IND(R1),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-end_rational");" new-line
			"	CMP(IND(R1),T_FRACTION);" new-line
			"	JUMP_EQ("lbl-end_rational");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			lbl-end_rational":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_rational":" new-line
                        (generate-primitive-closure lbl-rational addr) new-line
			)))))

(define primitive-numerator
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'numerator fvars-table))
                      (lbl-numerator (label-marker "Lprim_numerator") )
                      (lbl-closure_numerator (label-marker "LPRIM_CLOSURE_numerator") )
                     
                      )
			(string-append
			"JUMP("lbl-closure_numerator");" new-line
			lbl-numerator":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			
			
			"  	MOV(R0,FPARG(2));" new-line
			" 	MOV(R0,INDD(R0,1));"new-line
			" 	PUSH(R0);" new-line
			" 	CALL(MAKE_SOB_INTEGER);" new-line
			" 	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
						
			lbl-closure_numerator":" new-line
                        (generate-primitive-closure lbl-numerator addr) new-line
			)))))

(define primitive-denominator
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar 'denominator fvars-table))
                      (lbl-denominator (label-marker "Lprim_denominator") )
                      (IntegerArg (label-marker "L_integer") )
                      (EndLabel (label-marker "L_end_denom") )
                      (lbl-closure_denominator (label-marker "LPRIM_CLOSURE_denominator") )
                     
                      )
			(string-append
			"JUMP("lbl-closure_denominator");" new-line
			lbl-denominator":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"  	MOV(R0,FPARG(2));" new-line
			"CMP(IND(R0), T_INTEGER); \n"
			"JUMP_EQ("IntegerArg"); \n"
			
			" 	MOV(R0,INDD(R0,2));"new-line
			" 	PUSH(R0);" new-line
			" 	CALL(MAKE_SOB_INTEGER);" new-line
			"JUMP("EndLabel"); \n"
			
			IntegerArg":" new-line
			" 	MOV(R0,1);"new-line
			" 	PUSH(R0);" new-line
			" 	CALL(MAKE_SOB_INTEGER);" new-line
			EndLabel":" new-line
			" 	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_denominator":" new-line
                        (generate-primitive-closure lbl-denominator addr) new-line
			)))))

(define primitive-mul
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '* fvars-table))
                      (lbl-mul (label-marker "Lprim_mul") )
                      (lbl-closure_mul (label-marker "LPRIM_CLOSURE_mul") )
                      (lbl-mul-loop (label-marker "Lprimitive_minus_loop") )
                      (lbl-mul-intsToFracs (label-marker "Lprimitive_mul_intsToFracs") )
                      (lbl-mul-AfterUpdate (label-marker "Lprimitive_mul_AfterUpdate") )
                      (lbl-mul-end (label-marker "Lprimitive_mul_end") )     
                      )
			(string-append
			"JUMP("lbl-closure_mul");" new-line
			lbl-mul new-line":"
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"PUSH(R1);"new-line
			"PUSH(R2);"new-line
			"PUSH(R3);"new-line
			"PUSH(R4);"new-line
			"PUSH(R5);"new-line
			"PUSH(R6);"new-line
			"PUSH(R7);"new-line
			"  MOV(R1,IMM(1));" new-line
			"    MOV(R2,IMM(1));" new-line
			"    MOV(R5,IMM(0));" new-line	
			 lbl-mul-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line
			"	JUMP_EQ("lbl-mul-end");"new-line
			"	MOV(R6,FPARG(R5+2));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-mul-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,IMM(1)));"new-line
			"	MOV(R4,INDD(R6,IMM(2)));"new-line
			 lbl-mul-AfterUpdate":"  new-line
			"	MUL(R1,R3);"new-line
			"	MUL(R2,R4);"new-line
			"	INCR(R5);"new-line
			"	JUMP("lbl-mul-loop");"new-line
			lbl-mul-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-mul-AfterUpdate");"new-line
			lbl-mul-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line	
			      "POP(R7);"new-line
			      "POP(R6);"new-line
			      "POP(R5);"new-line
			      "POP(R4);"new-line
			      "POP(R3);"new-line
			      "POP(R2);"new-line
			      "POP(R1);"new-line
			      "	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_mul":" new-line
                        (generate-primitive-closure lbl-mul addr) new-line
			)))))

(define primitive-div
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '/ fvars-table))
                      (lbl-div (label-marker "Lprim_div")  )
                      (lbl-closure_div (label-marker "LPRIM_CLOSURE_div")  )
                      (lbl-div-loop (label-marker "Lprimitive_div_loop")  )
                      (lbl-div-intsToFracs (label-marker "Lprimitive_div_intsToFracs")  )
                      (lbl-div-AfterUpdate (label-marker "Lprimitive_div_AfterUpdate")  )
                      (lbl-div-One_arg (label-marker "Lprimitive_div_ONe_ARG")  )
                      (lbl-div-end (label-marker "Lprimitive_div_end")  )  
                      (lbl-div-Integer (label-marker "L_Integer")  )
                      )
			(string-append
			"JUMP("lbl-closure_div");" new-line
			lbl-div new-line":"
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R5,FPARG(2));" new-line
			"	MOV(R1,INDD(R5,1));" new-line
			"	MOV(R2,IMM(1));" new-line
			"	CMP(IND(R5),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-div-Integer");" new-line
			"	MOV(R2,INDD(R5,2));" new-line
			lbl-div-Integer":" new-line
			"	CMP(FPARG(1)-1,1);" new-line
			"	JUMP_EQ("lbl-div-One_arg");" new-line
			"	MOV(R5,1);" new-line
			 lbl-div-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line
			"	JUMP_EQ("lbl-div-end");"new-line
			"	MOV(R6,FPARG(R5+2));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-div-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,INDD(R6,2));"new-line
			 lbl-div-AfterUpdate":"  new-line
			"	MUL(R1,R4);"new-line
			"	MUL(R2,R3);"new-line
			"	INCR(R5);"new-line
			"	JUMP("lbl-div-loop");"new-line
			lbl-div-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-div-AfterUpdate");"new-line

			lbl-div-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line		
			"	POP(FP);" new-line
			"	RETURN;" new-line
			
			
			lbl-div-One_arg":"  new-line
			"CMP(IND(R5),T_FRACTION);" new-line
			"JUMP_NE(L_int);" new-line
			"MOV(R1,INDD(R5,2));"
			"MOV(R2,INDD(R5,1));"
			"JUMP("lbl-div-end");" new-line
			"L_int:" new-line
			"MOV(R1,IMM(1));" new-line
			"MOV(R2,INDD(R5,1));" new-line
			"JUMP("lbl-div-end");" new-line

			lbl-closure_div":" new-line
                        (generate-primitive-closure lbl-div addr) new-line
			)))))

(define primitive-minus
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '- fvars-table))
                      (lbl-minus (label-marker "Lprim_minus")  )
                      (lbl-closure_minus (label-marker "LPRIM_CLOSURE_minus")  )
                      (lbl-minus-loop (label-marker "Lprimitive_minus_loop")  )
                      (lbl-minus-intsToFracs (label-marker "Lprimitive_minus_intsToFracs")  )
                      (lbl-minus-AfterUpdate (label-marker "Lprimitive_minus_AfterUpdate")  )
                      (lbl-minus-One_arg (label-marker "Lprimitive_minus_ONe_ARG")  )
                      (lbl-minus-end (label-marker "Lprimitive_minus_end")  )
                      (lbl-minus-Integer (label-marker "L_IntegerR")  )
                      )
                    (string-append
			"JUMP("lbl-closure_minus");" new-line
			lbl-minus":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R5,FPARG(2));" new-line
			"	MOV(R1,INDD(R5,1));" new-line
			"	MOV(R2,IMM(1));" new-line
			"	CMP(IND(R5),T_INTEGER);" new-line
			"	JUMP_EQ("lbl-minus-Integer");" new-line
			"	MOV(R2,INDD(R5,2));" new-line
			lbl-minus-Integer":" new-line
			"	CMP(FPARG(1)-1,1);" new-line
			"	JUMP_EQ("lbl-minus-One_arg");" new-line
			"	MOV(R5,1);" new-line
			 lbl-minus-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line
			"	JUMP_EQ("lbl-minus-end");"new-line
			"	MOV(R6,FPARG(R5+2));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-minus-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,INDD(R6,2));"new-line
			 lbl-minus-AfterUpdate":"  new-line
			"	MUL(R1,R4);"new-line
			"	MUL(R4,R2);"new-line
			"	MUL(R3,R2);"new-line
			"	SUB(R1,R3);"new-line
			"	MOV(R2,R4);"new-line
			"	INCR(R5);"new-line
			
			"	JUMP("lbl-minus-loop");"new-line
			lbl-minus-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-minus-AfterUpdate");"new-line

			lbl-minus-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line		
			"	POP(FP);" new-line
			"	RETURN;" new-line
			
			
			lbl-minus-One_arg":"  new-line
			"CMP(IND(R5),T_FRACTION);" new-line
			"JUMP_NE(L_inttt);" new-line
			"MUL(R1,-1);" new-line
			"JUMP("lbl-minus-end");" new-line
			"L_inttt:" new-line
			"MUL(R1,-1);" new-line
			"MOV(R2,IMM(1));" new-line
			"JUMP("lbl-minus-end");" new-line
			
			lbl-closure_minus":" new-line
                        (generate-primitive-closure lbl-minus addr) new-line)))))

(define primitive-plus
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '+ fvars-table))
                      (lbl-plus (label-marker "Lprim_PLUS")  )
                      (lbl-closure_plus  (label-marker "LPRIM_CLOSURE_PLUS")  )
                      (lbl-plusl-loop (label-marker "Lprimitive_plusl_loop")  )
                      (lbl-plus-intsToFracs (label-marker "Lprimitive_plus_intsToFracs")  )
                      (lbl-plus-AfterUpdate (label-marker "Lprimitive_plus_AfterUpdate")  )
                      (lbl-plus-end (label-marker "Lprimitive_plus_end")  )     
                      )
		
			(string-append
			"JUMP("lbl-closure_plus");" new-line
			lbl-plus":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			
			
			"	MOV(R1,0);" new-line
			"	MOV(R2,1);" new-line
			"	MOV(R5,0);"new-line
			"	MOV(R8,R5);"new-line
			"	ADD(R8,2);"new-line
			 lbl-plusl-loop":" new-line
			"	CMP(R5,FPARG(1)-1);" new-line	
			"	JUMP_EQ("lbl-plus-end");"new-line
			"	MOV(R6,FPARG(R8));"new-line
			"	CMP(IND(R6),T_INTEGER);"new-line
			"	JUMP_EQ("lbl-plus-intsToFracs");"new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,INDD(R6,2));"new-line
			 lbl-plus-AfterUpdate":"  new-line
			"	MUL(R1,R4);"new-line
			"	MUL(R4,R2);"new-line
			"	MUL(R3,R2);"new-line
			"	ADD(R1,R3);"new-line
			"	MOV(R2,R4);"new-line
			"	INCR(R5);"new-line
			"	INCR(R8);"new-line
			"	JUMP("lbl-plusl-loop");"new-line
			lbl-plus-intsToFracs":"  new-line
			"	MOV(R3,INDD(R6,1));"new-line
			"	MOV(R4,IMM(1));"new-line
			"	JUMP("lbl-plus-AfterUpdate");"new-line

			lbl-plus-end":" new-line
			"	PUSH(R2);"new-line
			"	PUSH(R1);"new-line
			"	CALL(MAKE_SOB_FRACTION);"new-line
			"	DROP(2);"new-line		
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_plus":" new-line
                        (generate-primitive-closure lbl-plus addr) new-line
			)))))


(define primitive-numbers-equal
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '= fvars-table))
                      (lbl-numbers-equal (label-marker "Lprim_numbers_equal")  )
                      (lbl-closure_numbers-equal  (label-marker "Lmake_numbers_equal")  )
                      (lbl-numbers-equal-loop (label-marker "Lprimitive_numbersequal_loop")  )
                      (lbl-numbers-equalr-true (label-marker "Lprimitive_numbersequal_true")  )
                      (lbl-numbers-equal-false (label-marker "Lprimitive_numbersequal_false")  )
                      (lbl-numbers-equal-end (label-marker "Lprimitive_numbersequal_end")  )     
                      )
			(string-append
			"JUMP("lbl-closure_numbers-equal");" new-line
			lbl-numbers-equal":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"MOV(R1,FPARG(1)-1); // number of arguments"new-line			 			
			 "MOV(R7,SOB_BOOLEAN_TRUE);"new-line
			 "MOV(R2,IMM(1));" new-line
			 lbl-numbers-equal-loop":" new-line
			 "CMP(R1,R2);"new-line
			 "JUMP_EQ("lbl-numbers-equal-end");" new-line
			 "MOV(R3,FPARG(R2+1));" new-line
			 "MOV(R4,FPARG(R2+2));" new-line
			 "PUSH(R3);" new-line
			 "PUSH(R4);" new-line
			  "CALL(COMPARE_label);"
			"DROP(2);" new-line
			"CMP(R0,0);" new-line
			"JUMP_NE("lbl-numbers-equal-false");" new-line
			"INCR(R2);" new-line
			"JUMP("lbl-numbers-equal-loop");" new-line
			lbl-numbers-equal-false":" new-line
			"	MOV(R7,SOB_BOOLEAN_FALSE);" new-line
			lbl-numbers-equal-end":"
			new-line
			"  MOV(R0,R7);"new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_numbers-equal":" new-line
                        (generate-primitive-closure lbl-numbers-equal addr) new-line
			)))))

(define primitive-smaller
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '< fvars-table))
                      (lbl-smaller (label-marker "Lprim_smaller")  )
                      (lbl-closure_smaller (label-marker "Lmake_smaller")  )
                      (lbl-smaller-loop (label-marker "Lprimitive_smaller_loop")  )
                      (lbl-smaller-true (label-marker "Lprimitive_smaller_true")  )
                      (lbl-smaller-false (label-marker "Lprimitive_smaller_false")  )
                      (lbl-smaller-end (label-marker "Lprimitive_smaller_end")  )
                      )
                    (string-append
			"JUMP("lbl-closure_smaller");" new-line
			lbl-smaller":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			 "MOV(R1,FPARG(1)-1); // number of arguments"new-line			
			 "MOV(R7,SOB_BOOLEAN_TRUE);"new-line
			 "MOV(R2,IMM(1));" new-line
			 lbl-smaller-loop":" new-line
			 "CMP(R1,R2);"new-line
			 "JUMP_EQ("lbl-smaller-end");" new-line
			 "MOV(R3,FPARG(R2+1));" new-line
			 "MOV(R4,FPARG(R2+2));" new-line
			 "PUSH(R3);" new-line
			 "PUSH(R4);" new-line
			  "CALL(COMPARE_label);"
			"DROP(2);" new-line
			"CMP(R0,-1);" new-line
			"JUMP_NE("lbl-smaller-false");" new-line
			"INCR(R2);" new-line
			"JUMP("lbl-smaller-loop");" new-line
			lbl-smaller-false":" new-line
			"	MOV(R7,SOB_BOOLEAN_FALSE);" new-line
			lbl-smaller-end":" new-line
			"  MOV(R0,R7);"new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_smaller":" new-line
                        (generate-primitive-closure lbl-smaller addr) new-line
			)))))
	
(define primitive-bigger
(lambda (constants-table fvars-table frst_sym_address)
  (lambda ()
		(let 
		     ((addr (search-fvar '> fvars-table))
                      (lbl-bigger (label-marker "Lprim_bigger")  )
                      (lbl-closure_bigger (label-marker "Lmake_bigger")  )
                      (lbl-bigger-loop (label-marker "Lprimitive_bigger_loop")  )
                      (lbl-bigger-true (label-marker "Lprimitive_bigger_true")  )
                      (lbl-bigger-false (label-marker "Lprimitive_bigger_false")  )
                      (lbl-bigger-end (label-marker "Lprimitive_bigger_end")  ))
			(string-append
			"JUMP("lbl-closure_bigger");" new-line
			lbl-bigger":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			 "MOV(R1,FPARG(1)-1); // number of arguments"new-line			 			
			 "MOV(R7,SOB_BOOLEAN_TRUE);"new-line
			 "MOV(R2,IMM(1));" new-line
			 lbl-bigger-loop":" new-line
			 "CMP(R1,R2);"new-line
			 "JUMP_EQ("lbl-bigger-end");" new-line
			 "MOV(R3,FPARG(R2+1));" new-line
			 "MOV(R4,FPARG(R2+2));" new-line
			 "PUSH(R3);" new-line
			 "PUSH(R4);" new-line
			  "CALL(COMPARE_label);"
			"DROP(2);" new-line
			"CMP(R0,1);" new-line
			"JUMP_NE("lbl-bigger-false");" new-line
			"INCR(R2);" new-line
			"JUMP("lbl-bigger-loop");" new-line
			lbl-bigger-false":" new-line
			"	MOV(R7,SOB_BOOLEAN_FALSE);" new-line
			lbl-bigger-end":" new-line
			"  MOV(R0,R7);"new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure_bigger":" new-line
                        (generate-primitive-closure lbl-bigger addr) new-line
			)))))

(define primtive-symbol->string
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'symbol->string fvars-table))
                      (lbl-symbol->string (label-marker "Lprim_symbol_string")  )
                      (lbl-closure_symbol->string (label-marker "Lmake_symbol_string")  )
                      )
		(string-append
			"JUMP("lbl-closure_symbol->string");" new-line
			lbl-symbol->string":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure_symbol->string":" new-line
                        (generate-primitive-closure lbl-symbol->string addr) new-line
			)))))

(define prim-cons
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'cons fvars-table))
                      (lbl-cons (label-marker "Lprim_cons")  )
                      (lbl-closure_cons (label-marker "Lmake_cons")  )
                      )
		(string-append
			"JUMP("lbl-closure_cons");" new-line
			lbl-cons":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	PUSH(FPARG(3));" new-line ;get arg2
			"	PUSH(FPARG(2));" new-line ;get arg1
			"	CALL(MAKE_SOB_PAIR);" new-line
			"	DROP(2);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure_cons":" new-line
                        (generate-primitive-closure lbl-cons addr) new-line
			)))))

(define primitive-eq
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'eq? fvars-table))
                      (lbl-eq (label-marker "Lprim_eq")  )
                      (lbl-closure-eq (label-marker "Lmake_eq")  )
                      (lbl-eq-cmp-addr (label-marker "Leq_cmp_addr"))
                      (lbl-eq-cmp-val (label-marker "Leq_cmp_val"))
                      (lbl-is-eq (label-marker "Lis_eq"))
                      (lbl-not-eq (label-marker "Lnot_eq"))
                      (Lbl-end-eq (label-marker "Lend_eq")  )
                      (lbl-is-eq-Fraction (label-marker "Leq_fraction")  )
                      (lbl-is-eq-Symbol (label-marker "Leq_sym")  )
                      )
		(string-append
                        "/* Compares two arguments
                         If they are in {T_PAIR, T_STRING, T_VECTOR, T_NIL, T_VOID, T_BOOL} we only compare addresses
                         If they are closures, we compare each of their 3 fields to make sure they are all equal
                         Otherwise they are integers or chars so we compare their values and then their addresses */ " new-line		
			"JUMP("lbl-closure-eq");" new-line
			lbl-eq":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));  // Move the first arg to R0" new-line
			"	MOV(R2,FPARG(3)); // Move the second argument to R1" new-line
			"	MOV(R5,SOB_BOOLEAN_TRUE);" new-line
			"	CMP(INDD(R1,0),INDD(R2,0)); // Compare the types" new-line
			"	JUMP_NE("lbl-not-eq");" new-line
			"	CMP(INDD(R1,0),T_FRACTION);" new-line
			"	JUMP_EQ("lbl-is-eq-Fraction");" new-line
			"	CMP(INDD(R1,0),T_NIL);" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"	CMP(INDD(R1,0),T_VOID);" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"	CMP(INDD(R1,0),T_SYMBOL);" new-line
			"	JUMP_EQ("lbl-is-eq-Symbol");" new-line
                        "	CMP(INDD(R1,0),T_STRING);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
			"	CMP(INDD(R1,0),T_PAIR);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
			"	CMP(INDD(R1,0),T_VECTOR);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
			"	CMP(INDD(R1,0),T_BOOL);" new-line
			"	JUMP_EQ("lbl-eq-cmp-addr");" new-line
                        "	CMP(INDD(R1,0),T_CLOSURE); // Checks if both args are closures" new-line
			"	JUMP_NE("lbl-eq-cmp-val");" new-line
                        "       CMP(INDD(R1,0),INDD(R2,0)); // Compares the 3 fields of both closures" new-line
                        "       JUMP_NE("lbl-not-eq"); " new-line
                        "       CMP(INDD(R1,1),INDD(R2,1));" new-line
                        "       JUMP_NE("lbl-not-eq"); " new-line
                        "       CMP(INDD(R1,2),INDD(R2,2));" new-line
                        "       JUMP_NE("lbl-not-eq"); " new-line
			"       JUMP("Lbl-end-eq");" new-line
			lbl-eq-cmp-addr":" new-line
			"	CMP(R1,R2);" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"	JUMP("lbl-not-eq");" new-line
			lbl-eq-cmp-val":" new-line
			"	CMP(INDD(R2,1),INDD(R1,1));" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"JUMP("lbl-not-eq");\n"
			lbl-is-eq-Fraction":"  new-line
			"	CMP(INDD(R2,1),INDD(R1,1));" new-line
			"	JUMP_NE("lbl-not-eq");" new-line
			"	CMP(INDD(R2,2),INDD(R1,2));" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			"JUMP("lbl-not-eq");\n"
			lbl-is-eq-Symbol":"  new-line
			"	CMP(INDD(R2,1),INDD(R1,1));" new-line
			"	JUMP_EQ("Lbl-end-eq");" new-line
			lbl-not-eq":" new-line
			"	MOV(R5,SOB_BOOLEAN_FALSE);" new-line
			Lbl-end-eq":" new-line
			"MOV(R0,R5);\n"
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure-eq":" new-line
                        (generate-primitive-closure lbl-eq addr) new-line
			)))))

(define primitive-vector
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector fvars-table))
                      (lbl-vector (label-marker "Lprim_vector")  )
                      (lbl-closure_vector (label-marker "Lmake_vector")  )
                      (Lbl_vector_loop (label-marker "Lvector_loop")  )
                      (Lbl_vector_end (label-marker "Lvector_end")  )
                      )
		(string-append
			"JUMP("lbl-closure_vector");" new-line
			lbl-vector":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
                        "       MOV(R1, IMM(0))     "
			"	MOV(R2,FPARG(1)-1); " new-line  
			"	MOV(R3,R1); " new-line
			"	ADD(R3,IMM(2)); " new-line
			Lbl_vector_loop":" new-line
			"       CMP(R2 ,R1);" new-line  
			"	JUMP_EQ("Lbl_vector_end");" new-line
			"       PUSH(FPARG(R3)) ;" new-line
			"       ADD(R3, IMM(1)); " new-line
			"       ADD(R1, IMM(1)); " new-line
			"	JUMP("Lbl_vector_loop");" new-line
			Lbl_vector_end":" new-line
			"	PUSH(R2);"	new-line
			"	CALL(MAKE_SOB_VECTOR);" new-line
			"	ADD(R2,IMM(1));" new-line
			"	DROP(R2);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			
			lbl-closure_vector":" new-line
                        (generate-primitive-closure lbl-vector addr) new-line
			
			))))	)			
		
(define primitive-make-string
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'make-string fvars-table))
                      (lbl-make-string (label-marker "Lprim_maketring")  )
                      (lbl-closure-make-string (label-marker "Lmake_make_string")  )
                      (Lbl_make-string_one_param (label-marker "Lmakestring_one_param")  )
                      (Lbl_make-string_loop (label-marker "Lmakestring_loop")  )
                      (Lbl_make-string_end (label-marker "Lmakestring_end")  )
                      )
		(string-append
			"JUMP("lbl-closure-make-string");" new-line
			lbl-make-string":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"MOV(R2,0); \n" 
			"	MOV(R3,IMM(0));" new-line 
			"	MOV(R1,FPARG(2));" new-line ;(len)
			"	MOV(R1,INDD(R1,1));" new-line
			"MOV(R5, FPARG(1)-1); \n"
			"CMP(R5, 1); \n"
			"JUMP_EQ("Lbl_make-string_one_param"); \n"
			"	MOV(R2,FPARG(3)); " new-line ;(value)
			Lbl_make-string_loop":" new-line 
			"	CMP(R3,R1);" new-line ; 
			"	JUMP_EQ("Lbl_make-string_end");" new-line
			"	PUSH(INDD(R2,1));" new-line
			"	ADD(R3,IMM(1));" new-line
			"	JUMP("Lbl_make-string_loop");" new-line
			Lbl_make-string_one_param":" new-line 
			"	CMP(R3,R1);" new-line ; 
			"	JUMP_EQ("Lbl_make-string_end");" new-line
			"	PUSH(R2);" new-line
			"	ADD(R3,IMM(1));" new-line
			"	JUMP("Lbl_make-string_one_param");" new-line
			Lbl_make-string_end":" new-line
			"	PUSH(R1);"	new-line
			"	CALL(MAKE_SOB_STRING);" new-line
			"	ADD(R1,IMM(1));" new-line
			"	DROP(R1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-make-string":" new-line
                        (generate-primitive-closure lbl-make-string addr) new-line
			)))))			

(define primitive-make-vector
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'make-vector fvars-table))
                      (lbl-make-vector (label-marker "Lprim_make_vector")  )
                      (lbl-closure-make-vector (label-marker "Lmake_make_vector")  )
                      
                      (Lbl_vec_loop (label-marker "Lvec_loop")  )
                      (Lbl_vec_end (label-marker "Lvec_end")  )
                      )
		(string-append
			"JUMP("lbl-closure-make-vector");" new-line
			lbl-make-vector":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"PUSH(2);\n"
			"CALL(MALLOC);\n"
			"DROP(1);\n"
			"MOV(IND(R0),T_INTEGER); \n"
			"MOV(INDD(R0,1),IMM(0)); \n"
			"MOV(R2,R0); \n" 
			"	MOV(R3,IMM(0));" new-line 
			"	MOV(R1,FPARG(2));" new-line ;(len)
			"	MOV(R1,INDD(R1,1));" new-line
			"MOV(R5, FPARG(1)-1); \n"
			"CMP(R5, 1); \n"
			"JUMP_EQ("Lbl_vec_loop"); \n"
			"	MOV(R2,FPARG(3)); " new-line ;(value)
			Lbl_vec_loop":" new-line 
			"	CMP(R3,R1);" new-line ; 
			"	JUMP_EQ("Lbl_vec_end");" new-line
			"	PUSH(R2);" new-line
			"	ADD(R3,IMM(1));" new-line
			"	JUMP("Lbl_vec_loop");" new-line
			Lbl_vec_end":" new-line
			"	PUSH(R1);"	new-line
			"	CALL(MAKE_SOB_VECTOR);" new-line
			"	ADD(R1,IMM(1));" new-line
			"	DROP(R1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-make-vector":" new-line
                        (generate-primitive-closure lbl-make-vector addr) new-line
			)))))		

(define primitive-string-set
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-set! fvars-table))
                      (lbl-string-set (label-marker "Lprim_string_set")  )
                      (lbl-closure-string-set (label-marker "Lmake_string_set")  ))
		(string-append
			"JUMP("lbl-closure-string-set");" new-line
			lbl-string-set":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line  ; The string param
			"	MOV(R1,FPARG(3));" new-line  ; index param
			"	MOV(R2,FPARG(4));" new-line  ; char param
			"	MOV(R2,INDD(R2,1));" new-line
			"	MOV(R1,INDD(R1,1));" new-line
			"	ADD(R1,IMM(2));" new-line
			"	MOV(INDD(R0,R1),R2);" new-line  ; Moving the char to the given index
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-string-set":" new-line
                        (generate-primitive-closure lbl-string-set addr) new-line
	)))))
	
(define primitive-vector-set
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-set! fvars-table))
                      (lbl-vector-set (label-marker "Lprim_vector_set")  )
                      (lbl-closure-vector-set (label-marker "Lmake_vector_set")  ))
		(string-append
			"JUMP("lbl-closure-vector-set");" new-line
			lbl-vector-set":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line  ; The vector param
			"	MOV(R1,FPARG(3));" new-line  ; index param
			"	MOV(R2,FPARG(4));" new-line  ; obj param
			"	MOV(R1,INDD(R1,1));" new-line
			"	ADD(R1,IMM(2));" new-line
			"	MOV(INDD(R0,R1),R2);" new-line  ; Moving the obj to the given index
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-vector-set":" new-line
                        (generate-primitive-closure lbl-vector-set addr) new-line
	)))))
			
(define primitive-vector-length
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-length fvars-table))
                      (lbl-vector-length (label-marker "Lprim_vector_length")  )
                      (lbl-closure-vector-length (label-marker "Lmake_vector_length")  ))
		(string-append
			"JUMP("lbl-closure-vector-length");" new-line
			lbl-vector-length":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_INTEGER);" new-line
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure-vector-length":" new-line
                        (generate-primitive-closure lbl-vector-length addr) new-line

			)))))		
			
(define primitive-vector-ref
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector-ref fvars-table))
                      (lbl-vector-ref (label-marker "Lprim_vector_ref")  )
                      (lbl-closure-vector-ref (label-marker "Lmake_vector_ref")  ))
		(string-append
			"JUMP("lbl-closure-vector-ref");" new-line
			lbl-vector-ref":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));" new-line ; vector arg
			"	MOV(R2,FPARG(3));" new-line      ; Int object
			"	MOV(R5,INDD(R2,1));" new-line ; int-ref to the relevant index 
			"	ADD(R5,IMM(2));" new-line
			"	MOV(R0,INDD(R1,R5));" new-line ; get the value in the index
			"	POP(FP);" new-line
			"	RETURN;" new-line

			lbl-closure-vector-ref":" new-line
                        (generate-primitive-closure lbl-vector-ref addr) new-line
			)))))
			
			
			
			
(define primitive-str-length
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-length fvars-table))
                      (lbl-str-length (label-marker "Lprim_str_length")  )
                      (lbl-closure-str-length (label-marker "Lmake_str_length")  ))
		(string-append
			"JUMP("lbl-closure-str-length");" new-line
			lbl-str-length":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_INTEGER);" new-line
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
                        lbl-closure-str-length":" new-line
                        (generate-primitive-closure lbl-str-length addr) new-line
                        )))))
			

(define primitive-string-ref
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string-ref fvars-table))
                      (lbl-string-ref (label-marker "Lprim_string_ref")  )
                      (lbl-closure-string-ref (label-marker "Lmake_string_ref")  ))
		(string-append
			"JUMP("lbl-closure-string-ref");" new-line
			lbl-string-ref":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line ; string arg
			"	MOV(R1,FPARG(3));" new-line      ; Int object
			"	MOV(R1,INDD(R1,1));" new-line ; int-ref to the relevant index 
			"	ADD(R1,IMM(2));" new-line
			"	MOV(R0,INDD(R0,R1));" new-line ; get the value in the index
			"	PUSH(R0);" new-line
			"	CALL(MAKE_SOB_CHAR);" new-line
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-string-ref":" new-line
                        (generate-primitive-closure lbl-string-ref addr) new-line
			))))	)			

(define primtive-set-cdr!
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'set-cdr! fvars-table))
                      (lbl-set-cdr! (label-marker "Lprim_setcdr")  )
                      (lbl-closure-set-cdr! (label-marker "Lmake_set_cdr")  ))
		(string-append
			"JUMP("lbl-closure-set-cdr!");" new-line
			lbl-set-cdr!":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));" new-line ;pair
			"	MOV(R2,FPARG(3));" new-line ;obj
			"	MOV(INDD(R1,2),R2);" new-line
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-set-cdr!":" new-line
                        (generate-primitive-closure lbl-set-cdr! addr) new-line
			
			)))))
			
(define primtive-set-car!
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'set-car! fvars-table))
                      (lbl-set-car! (label-marker "Lprim_set_car")  )
                      (lbl-closure-set-car!(label-marker "Lmake_set_car")  ))
		(string-append
			"JUMP("lbl-closure-set-car!");" new-line
			lbl-set-car!":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R1,FPARG(2));" new-line ;pair
			"	MOV(R2,FPARG(3));" new-line ;obj
			"	MOV(INDD(R1,1),R2);" new-line
			"	MOV(R0,SOB_VOID);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-set-car!":" new-line
                        (generate-primitive-closure lbl-set-car! addr) new-line
			
			)))))
			
(define primitive-procedure?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'procedure? fvars-table))
                      (lbl-procedure? (label-marker "Lprim_procedure")  )
                      (lbl-closure-procedure? (label-marker "Lmake_procedure")  )
                     (lbl-is-procedure (label-marker "L_is_procedure")  )
                     (lbl-end-procedure (label-marker "L_end_procedure")  ))
		(string-append
			"JUMP("lbl-closure-procedure?");" new-line
			lbl-procedure?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_CLOSURE);" new-line
			"	JUMP_EQ("lbl-is-procedure");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-procedure");" new-line
			lbl-is-procedure":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-procedure":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-procedure?":" new-line
                (generate-primitive-closure lbl-procedure? addr) new-line
		)))))
			
			
(define primitive-pair?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'pair? fvars-table))
                      (lbl-pair? (label-marker "Lprim_pair")  )
                      (lbl-closure-pair? (label-marker "Lmake_pair")  )
                     (lbl-is-pair (label-marker "L_is_pair")  )
                     (lbl-end-pair (label-marker "L_end_pair")  ))
		(string-append
			"JUMP("lbl-closure-pair?");" new-line
			lbl-pair?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_PAIR);" new-line
			"	JUMP_EQ("lbl-is-pair");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-pair");" new-line
			lbl-is-pair":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-pair":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-pair?":" new-line
                (generate-primitive-closure lbl-pair? addr) new-line
		)))))
			
(define primitive-symbol?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'symbol? fvars-table))
                      (lbl-symbol? (label-marker "Lprim_symbol")  )
                      (lbl-closure-symbol? (label-marker "Lmake_symbol")  )
                     (lbl-is-symbol (label-marker "L_is_symbol")  )
                     (lbl-end-symbol (label-marker "L_end_symbol")  ))
		(string-append
			"JUMP("lbl-closure-symbol?");" new-line
			lbl-symbol?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_SYMBOL);" new-line
			"	JUMP_EQ("lbl-is-symbol");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-symbol");" new-line
			lbl-is-symbol":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-symbol":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-symbol?":" new-line
                (generate-primitive-closure lbl-symbol? addr) new-line
		)))))
;; 		
(define primitive-string?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string? fvars-table))
                      (lbl-string? (label-marker "Lprim_string")  )
                      (lbl-closure-string? (label-marker "Lmake_string")  )
                     (lbl-is-string (label-marker "L_is_string")  )
                     (lbl-end-string (label-marker "L_end_string")  ))
		(string-append
			"JUMP("lbl-closure-string?");" new-line
			lbl-string?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_STRING);" new-line
			"	JUMP_EQ("lbl-is-string");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-string");" new-line
			lbl-is-string":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-string":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-string?":" new-line
                (generate-primitive-closure lbl-string? addr) new-line
		)))))

(define primitive-zero?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'zero? fvars-table))
                      (lbl-zero? (label-marker "Lprim_zero")  )
                      (lbl-closure-zero? (label-marker "Lmake_zero")  )
                     (lbl-is-zero? (label-marker "L_is_zero")  )
                     (lbl-end-zero (label-marker "L_end_zero")  ))
		(string-append
			"JUMP("lbl-closure-zero?");" new-line
			lbl-zero?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,1));" new-line
			"	CMP(R0,IMM(0));" new-line
			"	JUMP_EQ("lbl-is-zero?");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-zero");" new-line
			lbl-is-zero?":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-zero":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-zero?":" new-line
                (generate-primitive-closure lbl-zero? addr) new-line
		)))))
		
(define primitive-vector?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'vector? fvars-table))
                      (lbl-vector? (label-marker "Lprim_vector")  )
                      (lbl-closure-vector? (label-marker "Lmake_vector")  )
                     (lbl-is-vector (label-marker "L_is_vector")  )
                     (lbl-end-vector (label-marker "L_end_vector")  ))
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
                      (lbl-null? (label-marker "Lprim_null")  )
                      (lbl-closure-null? (label-marker "Lmake_null")  )
                     (lbl-is-null (label-marker "L_is_null")  )
                     (lbl-end-null (label-marker "L_end_null")  ))
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
                      (lbl-char? (label-marker "Lprim_char")  )
                      (lbl-closure-char? (label-marker "Lmake_char")  )
                     (lbl-is-char (label-marker "L_is_char")  )
                     (lbl-end-char (label-marker "L_end_char")  ))
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
                      (lbl-integer? (label-marker "Lprim_integer")  )
                      (lbl-closure-integer? (label-marker "Lmake_integer")  )
                     (lbl-is-integer (label-marker "L_is_integer")  )
                     (lbl-end-integer (label-marker "L_end_integer")  ))
		(string-append
			"JUMP("lbl-closure-integer?");" new-line
			lbl-integer?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_INTEGER);" new-line
			"	JUMP_EQ("lbl-is-integer");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-integer");" new-line
			lbl-is-integer":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-integer":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-integer?":" new-line
                (generate-primitive-closure lbl-integer? addr) new-line
		)))))
		
(define primitive-boolean?
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'boolean? fvars-table))
                      (lbl-boolean? (label-marker "Lprim_boolean")  )
                      (lbl-closure-boolean? (label-marker "Lmake_boolean")  )
                     (lbl-is-boolean (label-marker "L_is_boolean")  )
                     (lbl-end-boolean (label-marker "L_end_boolean")  ))
		(string-append
			"JUMP("lbl-closure-boolean?");" new-line
			lbl-boolean?":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line
		
			"	MOV(R0,INDD(R0,0));" new-line
			"	CMP(R0,T_BOOL);" new-line
			"	JUMP_EQ("lbl-is-boolean");" new-line
			"	MOV(R0,SOB_BOOLEAN_FALSE);" new-line
			"	JUMP("lbl-end-boolean");" new-line
			lbl-is-boolean":" new-line
			"	MOV(R0,SOB_BOOLEAN_TRUE);" new-line
			lbl-end-boolean":" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-boolean?":" new-line
                (generate-primitive-closure lbl-boolean? addr) new-line
		)))))

(define primitive-char->integer
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'char->integer fvars-table))
                      (lbl-char->integer (label-marker "Lprim_char_integer")  )
                      (lbl-closure-char->integer (label-marker "Lmake_char_integer")  ))
		(string-append
			"JUMP("lbl-closure-char->integer");" new-line
			lbl-char->integer":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2)); // char" new-line ; param char
			"	MOV(R0,INDD(R0,1));" new-line ;char value
			"	PUSH(R0);" new-line 
			"	CALL(MAKE_SOB_INTEGER);" new-line 
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-char->integer":" new-line
                (generate-primitive-closure lbl-char->integer addr) new-line
		
		)))))
			
(define primitive-integer->char
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'integer->char fvars-table))
                      (lbl-integer->char (label-marker "Lprim_integer_char")  )
                      (lbl-closure-integer->char (label-marker "Lmake_integer_char")  ))
		(string-append
			"JUMP("lbl-closure-integer->char");" new-line
			lbl-integer->char":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2)); // char" new-line ; param char
			"	MOV(R0,INDD(R0,1));" new-line ;char value
			"	PUSH(R0);" new-line 
			"	CALL(MAKE_SOB_CHAR);" new-line 
			"	DROP(1);" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
		lbl-closure-integer->char":" new-line
                (generate-primitive-closure lbl-integer->char addr) new-line
		
		)))))
		
(define primitive-car
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'car fvars-table))
                      (lbl-car (label-marker "Lprim_car")  )
                      (lbl-closure-car (label-marker "Lmake_primitive_car")  ))
                      
		(string-append
			"JUMP("lbl-closure-car");" new-line
			lbl-car":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line 
			"	MOV(R0,INDD(R0,1));" new-line
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-car":" new-line
			(generate-primitive-closure lbl-car addr) new-line
			)))))
			
(define primitive-cdr
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'cdr fvars-table))
                      (lbl-cdr (label-marker "Lprim_cdr")  )
                      (lbl-closure-cdr (label-marker "Lmake_primitive_cdr")  ))
                      
		(string-append
			"JUMP("lbl-closure-cdr");" new-line
			lbl-cdr":" new-line
			"	PUSH(FP);" new-line
			"	MOV(FP, SP);" new-line
			"	MOV(R0,FPARG(2));" new-line 
			"	MOV(R0,INDD(R0,2));" new-line 
			"	POP(FP);" new-line
			"	RETURN;" new-line
			lbl-closure-cdr":" new-line
			(generate-primitive-closure lbl-cdr addr) new-line
			)))))

(define prim-string-to-symbol
(lambda (constants-table fvars-table frst_sym_address)
	(lambda ()
		(let ((addr (search-fvar 'string->symbol fvars-table))
                      (lbl-str->sym (label-marker "Lprim_str_sym")  )
                      (lbl-closure-str->sym (label-marker "Lmake_prim_str_sym")  )
                      (lbl-done (label-marker "L_str_sym_done")  )
                      (lbl-loop (label-marker "L_str_sym_loop"))
                      (lbl-new-sym (label-marker "L_str_sym_new_sym"))
                      )
                      
		(string-append
			"JUMP("lbl-closure-str->sym");" new-line
			lbl-str->sym":" new-line
			"  PUSH(FP);" new-line
                        "  MOV(FP,SP);" new-line
                        "  PUSH(R11);" new-line
                        "  PUSH(R12);" new-line
                        "  PUSH(R13);" new-line
                        "  PUSH(R14);" new-line
                        "  MOV(R12, FPARG(2));" new-line ; our param --> string
                        "  MOV(R11,"(number->string frst_sym_address)");" new-line; first real symbol
                        lbl-loop":" new-line
                        "  CMP(R11,IMM(-1)); " new-line ; to know that we are not at the end of the symbol table
                        "  JUMP_EQ("lbl-new-sym"); " new-line
                        "  MOV(R13,INDD(R11,1)); " new-line ; if the curr symbol exists -> point r13 to the string address
                        "  MOV(R0,R11); " new-line ; r11 = r0 = curr symbol {to return it in case we have match}
			  ; comapring the strings  
			"      
			MOV(R15,SOB_BOOLEAN_FALSE); \n
			CMP(INDD(R13,1),INDD(R12,1)); \n
			JUMP_NE(L_compare_strings_finish); \n
			MOV(R3,INDD(R13,1)); \n
			ADD(R3,2); \n
			MOV(R4,2); \n
			L_compare_strings_loop: \n
			CMP(R4,R3);  \n
			JUMP_EQ(L_compare_strings_true); \n
			CMP(INDD(R13,R4),INDD(R12,R4));  \n
			JUMP_NE(L_compare_strings_finish); \n
			INCR(R4); \n
			JUMP(L_compare_strings_loop); \n
			L_compare_strings_true: \n
			MOV(R15,SOB_BOOLEAN_TRUE); \n 
			L_compare_strings_finish: \n"
		"CMP(R15,SOB_BOOLEAN_TRUE);\n"
                        "  JUMP_EQ("lbl-done"); " new-line
                        "  MOV(R14, R11); " new-line
                        "  MOV(R11, INDD(R11,2)); " new-line
                        "  JUMP("lbl-loop");" new-line
       
                        lbl-new-sym":" new-line
                        "  PUSH(IMM(3));" new-line ;allocating for new sym
                        "  CALL(MALLOC);" new-line
                        "  DROP(1);" new-line
                        "  MOV(INDD(R14,2),IMM(R0));" new-line ; update the prev pointer to point to the  new symbol
                        "  MOV(IND(R0), T_SYMBOL);" new-line ;type
                        "  MOV(INDD(R0,1), IMM(R12));" new-line ;pointer to the string
                        "  MOV(INDD(R0,2), IMM(-1));" new-line ;point to the next -1
                                                     
                        lbl-done":" 
                        new-line
                        "  POP(R14);" new-line
                        "  POP(R13);" new-line
                        "  POP(R12);" new-line
                        "  POP(R11);" new-line
                        "  POP(FP);" new-line
                        "  RETURN;" new-line

			lbl-closure-str->sym":" new-line
			(generate-primitive-closure lbl-str->sym addr) new-line
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
       "  long mem["(number->string num-of-constansts)"] = {"(create-cs const-tab_)"}; " new-line
       "  memcpy(&ADDR("(number->string first-address)"), mem, sizeof(mem)); " new-line
         
       "MOV(IND(0),"(number->string last-address)");" new-line
       "  /* end of initialization */" new-line
       ))))

(define epilogue
  (string-append
   "  /* Stopping the program...   */ " new-line
    program-end-label ":" 
   new-line
     "STOP_MACHINE; "
   new-line
   "  return 0;" 
   new-line
   "} " 
   new-line))

(define gen-epilogue-sexpr
  (lambda (lbl) 
  (let ((L_epilogueSexpr (label-marker lbl))
	(DOne_label (label-marker "Done__lbl"))
  )
      (string-append
       L_epilogueSexpr":"
       "  /*	print the content of R0 */" new-line
       "  CMP(R0, SOB_VOID);" new-line
       "  JUMP_EQ("DOne_label");" new-line
       "  PUSH(R0);" new-line
       "  CALL(WRITE_SOB);" new-line 
       "  CALL(NEWLINE);" new-line
       "  DROP(1);" new-line
       DOne_label":" new-line
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