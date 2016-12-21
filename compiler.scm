
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

;############################## Assignment 2 ##############################


;reserved words
(define reserved-words
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

; variable predicate
(define var?
  (lambda (x)
  (and (symbol? x)
  (not (member x reserved-words)))))


(define parse
  (let ((run
  (compose-patterns
    ;variables 3.1.2
    (pattern-rule
        `(? 'key var?)
        (lambda (key)
            '(var key)));??????????
    ;if with 2 args
    (pattern-rule 
      `(if ,(? 'test) ,(? 'then))
       (lambda (test then)
      `(if3 ,(run test) ,(run then) (const ,(void)))))
    ;if with 3 args
    (pattern-rule 
      `(if ,(? 'test) ,(? 'then) ,(? 'else))
       (lambda (test then else)
      `(if3 ,(run test) ,(run then) ,(run else))))
    ;or 3.1.4
    (pattern-rule
        '(or ,(? 'arg1) . , (? 'other-args))
        (lambda (arg1 other-args)
        (run `(begin ,arg1 ,@other-args))))
    ;let* with no defined bindings
    (pattern-rule
      `(let* () ,(? 'body) . ,(? 'other-bodies))
      (lambda (body other-bodies)
      (run `(begin ,body ,@other-bodies))))
    ;let*
    (pattern-rule
      `(let* ((,(? 'key var?) ,(? 'value)) . ,(? 'other-bindings)) ,(? 'body))
      (lambda (key value other-bindings body)
      `(let (,(run key) ,(run value))
      ,(run (list 'let* other-bindings body)))))
    ;MIT define 3.1.16
    (pattern-rule
      `(define (,(? 'define-first-var  var?) . ,(? 'other-vars)) ,(? 'expr))
      (lambda (define-first-var other-vars expr)
      	(run (list 'define define-first-var '( 'lambda '( other-vars ') expr)))))
    ;regular define 3.1.16
    (pattern-rule
      `(define ,(? 'define-var var?) ,(? 'expr))
    (lambda (define-var expr)
      `(define '(var define-var) ,(run expr))))
    ;assignments 3.1.17
    (pattern-rule
      `(set! ,(? 'set-var var?) ,(? 'expr))
    (lambda (set-var expr)
      `(set '(var set-var) ,(run expr))))
  (lambda (sexpr)
  (run sexpr (lambda () '(this is what happens when the tag
  parser fails to match the input))))))

