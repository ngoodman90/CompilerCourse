#lang racket



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
        (*caten 2) *star                            ; ( *// InfixMulDiv )*

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
            (cond ((or (eq? first-expr 'const) (eq? first-expr 'var)) expr)
                  ((eq? first-expr 'seq) (map eliminate-nested-defines (car rest-expr)))
                  ((eq? first-expr 'def) `(def ,(car rest-expr) ,(eliminate-nested-defines (cadr rest-expr))))
                  ((eq? first-expr 'lambda-simple) `(lambda-simple ,(car rest-expr) ,@(eliminate-inner-lambda-nested-defines (cdr rest-expr))))
                  ((eq? first-expr 'lambda-var) `(lambda-var ,(car rest-expr) ,@(eliminate-inner-lambda-nested-defines (cdr rest-expr))))
                  ((eq? first-expr 'lambda-opt) `(lambda-opt ,(car rest-expr) ,(cadr rest-expr) ,@(eliminate-inner-lambda-nested-defines (cddr rest-expr))))
                  ((eq? first-expr 'applic) `(applic ,@(map eliminate-nested-defines rest-expr)))
                  ((list? first-expr) (list* (eliminate-nested-defines first-expr) (map eliminate-nested-defines rest-expr)))
                  (else expr)))))

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
         
(define box-set
    (lambda (expr)
        (cond   ((not-pair-or-null? expr) expr)                                                                                 ; do nothing
                ((and (equal? 'lambda-simple (car expr)) (< 2 (length expr)) (not (null? (car (lambda-simple-boxing expr)))))   ; lambda-simple
                 (begin (set! boxed-variables (lambda-simple-boxing expr))
                        `(,(car expr) ,(cadr expr) (seq (,@(car boxed-variables) ,@(lambda-box-set boxed-variables))))))
                ((and (equal? 'lambda-opt (car expr)) (< 2 (length expr)) (not (null? (car (lambda-opt-boxing expr)))))         ; lambda-opt
                 (begin (set! boxed-variables (lambda-opt-boxing expr)) 
                        `(,(car expr) ,(cadr expr) ,(caddr expr) (seq (,@(car boxed-variables) ,@(lambda-box-set boxed-variables))))))
                ((and (equal? 'lambda-var (car expr)) (< 2 (length expr)) (not (null? (car (lambda-var-boxing expr)))))         ; lambda-var
                 (begin (set! boxed-variables (lambda-var-boxing expr))
                        `(,(car expr) ,(cadr expr) (seq (,@(car boxed-variables) ,@(lambda-box-set boxed-variables))))))
                (else `(,(box-set (car expr)) ,@(box-set (cdr expr)))))))                                                       ; everything else

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




; ######################################### PROJECT #################################################

; ######################################## HELPER FUNCTIONS #########################################

(define code "")

(define boolean->string
  (lambda (bool)
    (if bool "1" "0")))

(define add-to-code
  (lambda (code-section)
    (set! code (string-append code code-section))))

(define add-line-to-code
  (lambda (line)
      (string-append line ";" (string #\newline))))

(define add-label-to-code
  (lambda (label)
    (set! code (string-append code label ":" (string #\newline)))))

(define my-map-helper
  (lambda (proc lst1 lst2)
    (if (null? lst1) lst2 
                     (my-map-helper proc (append lst2 (list (proc (car lst1)))) (cdr lst1)))))

(define my-map
  (lambda (proc lst)
    (my-map helper proc lst '())))


; Arithmetic instruction


(define ADD
  (lambda (dest stc)
    (string-append "ADD(" dest "," src ")")))

(define DECR
  (lambda (dest)
    (string-append "DECR(" dest ")")))

(define DIV
  (lambda (dest src)
    (string-append "DIV(" dest "," src ")")))

(define INCR
  (lambda (dest src)
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
    (string-append ("MOV(" dest "," src ")"))))


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

(define FPARG
  (lambda (n)
    (string-append "FPARG(" n ")")))


; system.lib

(define MALLOC
  (lambda (size dest)
    (add-line-to-code (PUSH size))
    (add-line-to-code (CALL "MALLOC"))
    (add-line-to-code (DROP "1"))
    (add-line-to-code (MOV dest "RO"))))


; Labels


(define label-index 0)

(define increment-label-index
  (set! label-index (+ 1 label-index)))

(define give-label-index
  (lambda (label-name)
    (begin (increment-label-index) (string-append label-name (number->String label-index)))))




; Loop

(define gen-loop
  (lambda (iter instruction)
    (let ((loop-start (give-label-index "LOOP_START_"))
         ((loop-exit (give-label-index "LOOP_EXIT_"))))
    (add-line-to-code (PUSH "R8"))
    (add-line-to-code (MOV "R8" iter))
    (add-label-to-code loop-start)
    (add-line-to-code (CMP "R8" "0"))
    (add-line-to-code (JUMP_EQ loop-exit))
    (add-line-to-code (PUSH "R8"))
    (instruction)
    (add-line-to-code (POP "R8"))
    (add-line-to-code (DECR "R8"))
    (add-line-to-code (JUMP loop-start))
    (add-label-to-code loop-exit)
    (add-line-to-code (POP "R8")))))







; ######################################### TABLES ##################################################


;initial values
(define global-table (list))
(define fvar-index 0)

(define add-fvar-to-global-table
    (lambda (fvar)
        (set! global-table (cons (list fvar-index fvar) global-table))))

(define increment-fvar-index
    (lambda ()
        (set! fvar-index (+ 1 fvar-index))))

(define find-fvars
    (lambda (code)
        (cond ((null-or-not-pair? code) '())
                    ((equal? (car code) 'fvar)
                        (begin (add-fvar-to-global-table (cadr code))
                                     (increment-fvar-index)))
                    (else (begin (find-fvars (car code)) (find-fvars (cdr code))))
    )))

(define sort-table-by-index
  (lambda (table)
    (sort (lambda (index1 index2) 
      (< (car index1) (car index2)) table))))

(define make-global-table
  (lambda (code)
    (begin (find-fvars code)
           (set! global-table (sort-table-by-index global-table))
           (MALLOC (number->string fvar-index) "R9")
  )))

(define add-rt-fvars
  (lambda (rt-fvars)
    (begin (map (lambda (expr)
              (begin (add-fvar-to-global-table expr)
                     (increment-fvar-index))) rt-fvars)
           (set! global-table (sort-table-by-index global-table)))))

(define lookup-fvar
  (lambda (index table)
    (number->string (cond ((null? table) "ERROR: KEY NOT FOUND IN GLOBAL TABLE")
                          ((equal? index (cadar table)) (caar table))
                          (else (lookup-fvar index (cdr table)))))))

(define gen-fvar
  (lambda (pe)
    (add-line-to-code (PUSH "R1"))
    (add-line-to-code (MOV "R1" (lookup-fvar (cadr pe))))
    (add-line-to-code (ADD "R1" "R9"))
    (add-line-to-code (MOV "R0" (IND "R1")))
    (add-line-to-code (POP "R1"))))

(define set-fvar
  (lambda (pe)
    (code-gen (caddr pe))
    (add-line-to-code (PUSH "R1"))
    (add-line-to-code (MOV "R1" (lookup-fvar (cadr (cadr pe)))))
    (add-line-to-code (ADD "R1" "R9"))
    (add-line-to-code (MOV (IND "R1") "R0"))
    (add-line-to-code (POP "R1"))
    (add-line-to-code (MOV "R0" "T_VOID"))))

(define gen-box-get-fvar
  (lambda (pe)
    (add-line-to-code (PUSH "R1"))
    (add-line-to-code (MOV "R1" (lookup-fvar (cadr pe))))
    (add-line-to-code (ADD "R1" "R9"))
    (add-line-to-code (MOV "R0" (IND "R1")))
    (add-line-to-code (POP "R1"))
    (add-line-to-code (MOV "R0" (IND "R0")))))

(define def-fvar
  (lambda (pe)
    (code-gen (caddr pe))
    (add-line-to-code (PUSH "R1"))
    (add-line-to-code (MOV "R1" (lookup-fvar (cadr (cadr pe)))))
    (add-line-to-code (ADD "R1" "R9"))
    (add-line-to-code (MOV (IND "R1") "R0"))
    (add-line-to-code (MOV "R0" "T_VOID"))
    (add-line-to-code (POP "R1"))))





; ########################################### CODE-GEN ###################################################


(define code-gen 1)

(define gen-make-sob-void
  (lambda ()
    (add-line-to-code (CALL "MAKE_SOB_VOID"))))

(define gen-make-sob-integer
  (lambda (int)
    (begin (add-line-to-code (PUSH (IMM  (number->string (car int)))))
           (add-line-to-code (CALL "MAKE_SOB_INTEGER"))
           (add-line-to-code (DROP "1")))))

(define gen-make-sob-fraction
  (lambda (frac)
    (begin (add-line-to-code (PUSH (IMM  (number->String (cadr frac)))))
           (add-line-to-code (PUSH (IMM  (number->String (car frac)))))
           (add-line-to-code (CALL "MAKE_SOB_FRACTION")))))

(define gen-make-sob-pair
  (lambda (pair)
    (begin (add-line-to-code (PUSH (IMM  (number->String (cadr pair)))))
           (add-line-to-code (PUSH (IMM  (number->String (car pair)))))
           (add-line-to-code (CALL "MAKE_SOB_PAIR")))))

(define gen-make-sob-vector
  (lambda (vec)
    (let ((reversed-vec (reverse vec)))
    (begin  (map-in-order (lambda (vec-element) 
                            (add-line-to-code (PUSH (IMM  (number->String vec-element))))) 
                              (cdr reversed-vec))
            (add-line-to-code (PUSH (IMM (number->String (car reversed)))))
            (add-line-to-code (CALL "MAKE_SOB_VECTOR"))))))

(define gen-make-sob-char
  (lambda (c)
    (begin (add-line-to-code (PUSH (IMM  (number->String (char->integer (car c))))))
           (add-line-to-code (CALL "MAKE_SOB_CHAR")))))

(define gen-make-sob-string
  (lambda (str)
    (let ((reversed-str (reverse-str)))
      (begin (my-map (lambda (c) 
          (add-line-to-code (PUSH (IMM (number->String (char->integer c)))))) (cdr reversed-str))
             (add-line-to-code (PUSH (IMM (number->String (car reversed-str)))))
             (add-line-to-code (CALL "MAKE_SOB_STRING"))))))

(define gen-make-sob-symbol
  (lambda (sym)
    (begin (add-line-to-code (PUSH (IMM (number->String (car sym)))))
           (add-line-to-code (CALL "MAKE_SOB_SYMBOL"))
           (add-line-to-code (DROP "1")))))

(define gen-make-sob-bool
  (lambda (bool)
    (let ((bool-val (if (car bool) 1 0)))
        (begin (add-line-to-code (PUSH (IMM (number->String bool-val))))
               (add-line-to-code (CALL "MAKE_SOB_BOOL"))
               (add-line-to-code (DROP "1"))))))

(define gen-make-sob-nil
  (lambda ()
    (add-line-to-code (CALL "MAKE_SOB_NIL"))))

(define gen-seq
  (lambda (pe)
    (my-map code-gen (cadr pe))))

(define gen-if3
  (lambda (pe)
    (let ((if-exit (give-label-index "L_if3_exit_"))
         ((if-else (give-label-index "L_if3_else_"))))
     (begin (code-gen (cadr pe))
      TODO CHANGE THIS ONCE CONST IS COMPLETE
            (add-line-to-code (CMP "R0" (number->String (const-lookup #f const-table))))
            (add-line-to-code (JUMP_EQ if-else))
            (code-gen (caddr pe))
            (add-line-to-code (JUMP if-exit))
            (add-label-to-code if-else)
            (code-gen (caddr (cdr pe)))
            (add-label-to-code if-exit)))))

(define gen-or
  (lambda (pe)
    (begin (define or-exit (give-label-index "L_or_exit_"))
           (my-map (lambda (predicate)
                      (begin (code-gen predicate)
                        TODO CHANGE ONCE CONST TABLE IS COMPLETE
                             (add-line-to-code (CMP "R0" (number->String (const-lookup #f const-table))))
                             (add-line-to-code (JUMP_NE or-exit)))) (cadr pe))
           (add-label-to-code or-exit))))

(define gen-pvar
  (lambda (pe)
    (add-line-to-code (MOV "RO" (FPARG (number->string (+ 2 (caddr pe))))))))

(define gen-bvar
  (lambda (pe)
    (add-line-to-code (MOV "R0" (FPARG "0")))
    (add-line-to-code (MOV "R0" (INDD "R0" (number->string (caddr pe)))))
    (add-line-to-code (MOV "R0" (INDD "R0" (number->string (cadddr pe)))))))

(define gen-applic-tc
  (lambda (pe)
    (let ((fixed_sp (give-label-index "FIXED_SP_"))
          (caddr-len (number->string (length (caddr pe)))))
    (begin (add-line-to-code (CMP (FPARG "1") caddr-len))
           (add-line-to-code (JUMP_LE fixed_sp))         
           (add-label-to-code fixed_sp)
           (my-map (lambda (expr)
                    (begin (code-gen expr)
                           (add-line-to-code (push "R0"))))
              (reverse (caddr pe)))
           (add-line-to-code (PUSH (IMM (number->string (length (caddr pe))))))
           (code-gen (cadr pe))
           (add-line-to-code (CMP (INDD "R0" "0") "T_CLOSURE"))
           (add-line-to-code (JUMP_NE "NOT_CLOSURE"))
           (add-line-to-code (string-append (PUSH (INDD "R0" "1")) "/*env*/"))
           (add-line-to-code (PUSH (FPARG "-1")))
           (add-line-to-code (MOV "R10" (FPARG "-2")))
           (add-line-to-code (MOV "R11" (number->string (length (caddr pe)))))
           (add-line-to-code (ADD "R11" "3"))
           (add-line-to-code (MOV "R12" "SP"))
           (add-line-to-code (SUB "R12" "3"))
           (add-line-to-code (SUB "R12" (number->string (length (caddr pe)))))
           (add-line-to-code (MOV "R13" "FP"))
           (add-line-to-code (SUB "R13" "4"))
           (add-line-to-code (SUB "R13" (FPARG "1")))
           (gen-loop "R11"
             (lambda () (begin (add-line-to-code (MOV "R14" ( "R12")))                               
                               (add-line-to-code (MOV (STACK "R13") "R14"))
                               (add-line-to-code (INCR "R12"))
                               (add-line-to-code (INCR "R13")))))
           (add-line-to-code (MOV "FP" "R1"))
           (add-line-to-code (MOV "SP" "R13"))
           (add-line-to-code (JUMP (string-append "*" (INDD "R0" "2"))))
           ))))


(define gen-lambda
  (lambda (pe)
    (cond ((lambda-simple? pe) (gen-lambda-simple pe))
          ((lambda-var? pe) (gen-lambda-var pe))
          ((lambda-opt? pe) (gen-lambda-opt pe)))
    ))

(define code-gen
  (lambda (pe)
    (cond ((null-or-not-pair? pe) "")
          ((equal? 'or (car pe)) (gen-or pe));ALMOST DONE - missing const table functions
          ((equal? 'if3 (car pe)) (gen-if3 pe));ALMOST DONE - missing const table functions
          ((equal? 'pvar (car pe)) (gen-pvar pe));ALMOST DONE - no set or set box
          ((equal? 'bvar (car pe)) (gen-bvar pe));ALMOST DONE - no gen-set or gen-set-box
          ((equal? 'def (car pe)) (gen-def pe))
          ((equal? 'box-get (car pe)) (gen-box-get pe))
          ((equal? 'applic (car pe)) (gen-applic pe))
          ((equal? 'tc-applic (car pe)) (gen-tc-applic pe));DONE
          ((equal? 'const (car pe)) (gen-const pe))
          ((equal? 'set (car pe)) (gen-set pe))
          ((equal? 'seq (car pe)) (gen-seq pe));DONE
          ((lambda-expr? pe) (gen-lambda pe))
          (else (begin (code-gen (car pe)) (code-gen (cdr-pe))))
    )))
