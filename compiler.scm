
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

; ####################################### Ass2 starts here #######################################

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

; ####################################### Eliminate Nested Defines #######################################

(define first-part
    (lambda (begining ending) begining))
    
(define second-part
    (lambda (begining ending) ending))
    
(define perform-define-split
    (lambda (var func)
        (if (not (null? var))
            (perform-define-split
                (cdr var)
                (lambda (expr1 expr2)
                        (if (eq? (caar var) 'def)
                            (func (cons (car var) expr1) expr2)
                            (if (eq? (caar var) 'seq)
			        (perform-define-split
                                    (cadar var)
                                    (lambda (expr3 expr4)
                                            (func (append expr3 expr1)
                                                  (append expr4 expr2))))
                                (func expr1 (cons (car var) expr2))))))
            (func '() '()))))

(define handle-sequence
    (lambda (expr1 expr2)
        `(seq (,@(map (lambda (a) `(set ,(cadr a) ,(eliminate-nested-defines (caddr a)))) expr1)
               ,@(map eliminate-nested-defines expr2)))))
            
(define eliminate-inner-lambda-nested-defines
    (lambda (content)
        (let ((expr1 (perform-define-split content first-part))
              (expr2 (perform-define-split content second-part)))
             (if (not (null? expr1))
                `((applic (lambda-simple ,(map cadadr expr1) ,(handle-sequence expr1 expr2)) ,(map (lambda (a) '(const #f)) expr1)))
                (map eliminate-nested-defines content)))))
            
(define eliminate-nested-defines
    (lambda (expr)
        (let
            ((first-expr (car expr))
             (rest-expr (cdr expr)))
            (cond ((eq? first-expr 'seq) (map eliminate-nested-defines (car rest-expr)))
                  ((eq? first-expr 'def) `(def ,(car rest-expr) ,(eliminate-nested-defines (cadr rest-expr))))
                  ((eq? first-expr 'lambda-simple) `(lambda-simple ,(car rest-expr) ,@(eliminate-inner-lambda-nested-defines (cdr rest-expr))))
                  ((eq? first-expr 'lambda-var) `(lambda-var ,(car rest-expr) ,@(eliminate-inner-lambda-nested-defines (cdr rest-expr))))
                  ((eq? first-expr 'lambda-opt) `(lambda-opt ,(car rest-expr) ,(cadr rest-expr) ,@(eliminate-inner-lambda-nested-defines (cddr rest-expr))))
                  (else expr)))))

; ####################################### Boxing of Variables #######################################

(define box-set
    (lambda (x)
        x))

; ####################################### Removing Redundant Applications #######################################

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

(define pe->lex-pe
  (lambda (expr)
    (cond ((null-or-not-pair? expr) expr)
          ((tag-var? expr) `(fvar ,@(cdr expr)))
          ((lambda-expr? expr) (lex-pe-lambda expr))
          (else `(,(pe->lex-pe (car expr)) ,@(pe->lex-pe (cdr expr)))))
    ))

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
