
(load "pc.scm")

;digits
(define <digit-0-9>
  (range #\0 #\9))

;digits without 0
(define <digit-1-9>
  (range #\1 #\9))

(define <a-z>
  (range #\a #\z))
  
(define <A-Z>
  (range #\A #\Z))

;make string meta char
(define ^<MetaChar>
  (lambda (str ch)
    (new (*parser (word-ci str))
	  (*pack (lambda (_) ch))
  done)))

(define <Boolean>
  (new (*parser (^<MetaChar> "#t" #t))
       (*parser (^<MetaChar> "#f" #f))

       (*disj 2)
  done))
  
; ##################################### Char #####################################

(define <CharPrefix>
  (new (*parser (word-ci "#\\"))
  done))  

(define <VisibleSimpleChar>
  (new (*parser <any-char>)
       (*guard (lambda (n) (> (char->integer n) 32))) ; no space
;;        (*pack-with
;; 	(lambda (n)
;; 	  n))
  done))
  
(define <NamedChar>
  (new (*parser (^<MetaChar> " " #\space))
       (*parser (^<MetaChar> "\\n" #\newline))
       (*parser (^<MetaChar> "\\r" #\return))
       (*parser (^<MetaChar> "\\t" #\tab))
       (*parser (^<MetaChar> "\\f" #\page)) ; formfeed
       (*parser (^<MetaChar> "lambda" (integer->char 955)))
       (*parser (^<MetaChar> "alef" (integer->char 1488)))
       (*parser (^<MetaChar> "bismillah" (integer->char 65021)))
       (*parser (^<MetaChar> "smiley" (integer->char 9786)))
;;        (*parser (^<MetaChar> "" #\nul)) ; TODO change so it will work

       (*disj 9)
  done))
  
(define <HexChar>
  (new (*parser (range #\0 #\9))
       (*parser (range #\a #\f))
       
       (*disj 2)
  done))

(define <HexUnicodeChar>
  (new (*parser (char #\x))
       (*parser <HexChar>) *plus
       
       (*caten 2)
       (*pack-with
	(lambda (x chars)
	  (list->string chars)))
  done))

(define <Char>
  (new (*parser <CharPrefix>)
       (*parser (char #\())
       
       (*parser <HexUnicodeChar>)
       (*parser <NamedChar>)
       (*parser <VisibleSimpleChar>)
       (*disj 3)
       
       (*parser (char #\)))
       
       (*caten 4)
       (*pack-with
	(lambda (prefix open ch close)
	  ch))
  done))
  
; ##################################### Number #####################################

(define <Natural>
  (new (*parser (char #\0))
       (*pack (lambda (_) 0))

       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
 (lambda (a s)
   (string->number
    (list->string
     `(,a ,@s)))))

       (*disj 2)
  done))
  
(define <Integer>
  (new (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
         (lambda (++ n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
         (lambda (-- n) (- n)))

       (*parser <Natural>)

       (*disj 3)

  done))

(define <Fraction>
  (new (*parser <Integer>)
       (*parser (char #\/))
       (*parser <Natural>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
 (lambda (num div den)
   (/ num den)))
  done))
  
(define <Number>
  (new (*parser <Integer>)
       (*parser <Fraction>)
       (*disj 2)
  done))
  
; ##################################### String #####################################

(define <StringHexChar>
  (new (*parser (word-ci "\\x"))
       (*parser <HexChar>) *star
       
       (*caten 2)
        (*pack-with
	  (lambda (x ch) (list->string ch)))
  done))
  
(define <StringLiteralChar>
  (new (*parser <any-char>)
       (*guard (lambda (n) (> (char->integer n) 92))) ; no backslash (\)
  done))

(define <StringMetaChar>
  (new (*parser (word-ci "\\\\"))
       (*parser (word-ci "\\\""))
       (*parser (word-ci "\\n"))
       (*parser (word-ci "\\r"))
       (*parser (word-ci "\\t"))
       (*parser (word-ci "\\f"))
;;        (*parser (^<MetaChar> "\\n" #\newline))
;;        (*parser (^<MetaChar> "\\r" #\return))
;;        (*parser (^<MetaChar> "\\t" #\tab))
;;        (*parser (^<MetaChar> "\\f" #\page))

       (*disj 6)
;;        (*pack-with
;; 	(lambda (n)
;; 	  (list->string n)))
  done))

(define <StringChar>
  (new (*parser <StringHexChar>)
       (*parser <StringMetaChar>)
	(*pack (lambda (n) (list->string n)))
       (*parser <StringLiteralChar>)
       
       (*disj 3)
  done))

(define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
	(lambda (open-delim chars close-delim)
	  (list->string chars)))
  done))
  
; ##################################### Symbol #####################################

(define <SymbolChar>
  (new (*parser <digit-0-9>)
       (*parser <a-z>)
       (*parser <A-Z>)
       (*parser (char #\!))
       (*parser (char #\$))
       (*parser (char #\^))
       (*parser (char #\*))
       (*parser (char #\-))
       (*parser (char #\_))
       (*parser (char #\=))
       (*parser (char #\+))
       (*parser (char #\<))
       (*parser (char #\>))
       (*parser (char #\?))
       (*parser (char #\/))
       
       (*disj 15)
  done))

(define <Symbol>
  (new (*parser <SymbolChar>) *plus
;; 	(*pack (lambda (n) n))
;;        (*caten 2)
;;        (*pack-with
;; 	(lambda (a b) `(,@(list ,@a ,@b))))
  done))

; ##################################### InfixExtension #####################################

(define <InfixPrefixExtensionPrefix>
  (new (*parser (word-ci "##"))
       (*parser (word-ci "#%"))
       
       (*disj 2)
  done))

(define <PowerSymbol>
  (new (*parser (^<MetaChar> "**" #\^))
       (*parser (char #\^))
       
       (*disj 2)
  done))
  
(define <action-symbol>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*parser <PowerSymbol>)
       (*parser (char #\*))
       (*parser (char #\/))
       (*disj 5)
  done))
       
(define <InfixSymbol>
  (new (*parser <SymbolChar>)
       (*parser <action-symbol>)
       *diff
       (*pack
	(lambda (a) a))
	*plus
       (*pack
	(lambda (a) (list->string a)))
  done))
  
(define <InfixNeg>
  (new (*parser (char #\-))
;; 	(*pack (lambda (a) -))
       (*delayed (lambda () <InfixNumberOrSymbol>))
       (*caten 2)
       (*pack-with
	(lambda (neg num)
	  `(- ,num)))
  done))
  
(define <InfixNumberOrSymbol>
  (new (*parser <InfixNeg>)
       (*parser <Number>)
       (*parser <InfixSymbol>)
       (*disj 3)
  done))
       
(define power
  (lambda (num pow)
     `(^ ,num ,pow)))

(define power-list
  (lambda (lst)
    (let ((head (car lst))
	 (tail (cdr lst)))
      (if (not (null? (cdr tail)))
	(power head (power-list tail))
	(power head (car tail))))))

; Power
(define <InfixPowerList>
  (new (*parser <InfixNumberOrSymbol>)
       (*parser <PowerSymbol>)
       (*caten 2)
       (*pack-with
	(lambda (n _^) n)) *plus
       
       (*parser <InfixNumberOrSymbol>)
       ; todo maybe add here [] and ()
       (*caten 2)
       (*pack-with
	(lambda (a b)
	  (power-list (reverse (list* b (reverse a))))))
	  
       (*parser <InfixNumberOrSymbol>)
       (*disj 2)
  done))

; Multiplication & Division
(define <InfixMulOrDiv>
  (new (*parser <InfixPowerList>)
;;        (*parser <InfixPowerList>) ; gets 1+1 but not 1*1
       
       (*parser (char #\*))
	(*pack (lambda (a) 1))
       (*parser (char #\/))
	(*pack (lambda (a) 2))
       (*disj 2)
       
       (*delayed (lambda () <InfixMulOrDiv>))
       (*caten 3)
       (*pack-with
	(lambda (num action expression)
	  (if (= action 1)
	    `(* ,num ,expression)
	    `(/ ,num ,expression))))
	  
       (*parser <InfixPowerList>)
       (*disj 2)
  done))

; Addition & Subtraction
(define <InfixAddOrSub>
  (new (*parser <InfixMulOrDiv>)
       
       (*parser (char #\+))
	(*pack (lambda (a) 1))
       (*parser (char #\-))
	(*pack (lambda (a) 2))
       (*disj 2)
       
       (*delayed (lambda () <InfixAddOrSub>))
       (*caten 3)
       (*pack-with
	(lambda (num action expression)
	  (if (= action 1)
	    `(+ ,num ,expression)
	    `(- ,num ,expression))))
	  
       (*parser <InfixMulOrDiv>)
       (*disj 2)
  done))
  
;; (define <InfixFunctionContinued>
;;   (new (*parser (char #\,))
;;        (*delayed (lambda () <InfixExpression>))
;;        (*caten 2)
;; ;;        (*pack-with (lambda (a b)
;; ;; 	(list a b)))
;;   done))
  
;; (define <yuval>
;;   (new (*parser <InfixFunctionContinued>)
;;        (*pack (lambda (a) `(,@a)))
;;   done))
  
(define <InfixExpressionContinued>
  (new (*parser (char #\[))					; Array
       (*delayed (lambda () <InfixExpression>))
       (*parser (char #\]))
       (*caten 3)
       (*pack-with
	(lambda (a b c)
	  `(,a ,@b ,c)))
       
       (*parser (char #\())					; Function
       
       (*delayed (lambda () <InfixExpression>)) ; first arguement
       (*parser (char #\,))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       (*pack-with
	(lambda (a b) `(,a ,b)))
       (*pack
	(lambda (a) `(,@a))) *star ; rest of the arguements
       (*caten 2) ; 1 or many arguements
       (*pack-with
	(lambda (a b) `(,a ,@b)))
       (*parser <epsilon>)
       (*disj 2) ; 0,1 or many arguements
       
       (*parser (char #\)))
       (*caten 3)
       (*pack-with
	(lambda (a b c)
	  b))
       
       (*disj 2)						; Array / Function
  done))
  
(define <InfixExpression>
  (new (*parser <InfixAddOrSub>)				; beginning of every InfixExpression
       
       (*parser <InfixExpressionContinued>)
       
       (*caten 2)
       
       (*parser <InfixAddOrSub>)				; ending of every InfixExpression
       (*disj 2)
  done))
  
(define <InfixExtension>
  (new (*parser <InfixPrefixExtensionPrefix>)
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with
	(lambda (prefix e) e))
  done))
 
;; ;; (define <InfixArrayGet>
;; ;;   (new (*parser <InfixExpression>)
;; ;;        (*parser (char #\[))
;; ;;        (*parser <InfixExpression>)
;; ;;        (*parser (char #\]))
;; ;;        (*caten 4)
;; ;;   done))
;; ;;   
;; ;; (define <InfixFuncall>
;; ;;   (new (*parser <InfixExpression>)
;; ;;        (*parser (char #\())
;; ;;        (*parser <InfixArgList>)
;; ;;        (*parser (char #\)))
;; ;;        (*caten 4)
;; ;;   done))
;; ;; 
;; ;; (define <InfixArgList>
;; ;;   (new (*parser <InfixExpression>)
;; ;;        (*parser (word-ci "(,"))
;; ;;        (*parser <InfixExpression>)
;; ;;        (*parser (char #\))) *star ;TODO check where the star should be: ⟨InfixArgList⟩::=⟨InfixExpression⟩ (, ⟨InfixExpression⟩) ∗ | ε
;; ;;        (*parser <epsilon>)
;; ;;        (*caten 4)
;; ;;   done))
;; ;;   
;; ;; (define <InfixParen>
;; ;;   (new (*parser (char #\())
;; ;;        (*parser <InfixExpression>)
;; ;;        (*parser (char #\())
;; ;;        (*caten 3)
;; ;;   done))
;; ;;   
;; ;; (define <InfixSexprEscape>
;; ;;   (new (*parser <InfixPrefixExtensionPrefix>)
;; ;;        (*parser <Sexpr>)
;; ;;        (*caten 2)
;; ;;   done))

; ##################################### Sexpr #####################################

(define <Sexpr>
  (new (*parser <Boolean>)
       (*parser <Char>)
       (*parser <Number>)
       (*parser <String>)
       (*parser <Symbol>)
       (*delayed (lambda () <ProperList>))
       (*delayed (lambda () <ImproperList>))
       (*delayed (lambda () <Vector>))
       (*delayed (lambda () <Quoted>))
       (*delayed (lambda () <QuasiQuoted>))
       (*delayed (lambda () <Unquoted>))
       (*delayed (lambda () <UnquotedAndSpliced>))
       (*parser <InfixExtension>)
       (*disj 13)
  done))
  
; ##################################### ProperList #####################################

(define <ProperList>
  (new (*parser (char #\())
       (*parser <Sexpr>)
       (*parser (char #\)))
       (*caten 3)
       (*pack-with
	(lambda (a s b)
	  `(,s)))
  done))

; ##################################### ImproperList #####################################

(define <ImproperList>
  (new (*parser (char #\())
       (*parser <Sexpr>)
       (*parser <Sexpr>) *star
       (*parser (char #\.))
       (*parser <Sexpr>)
       (*parser (char #\)))
       (*caten 6)
  done))

; ##################################### Vector #####################################

(define <Vector>
  (new (*parser (word-ci "#("))
       (*parser <Sexpr>) *star
       (*parser (char #\)))
       (*caten 3)
  done))

; ##################################### Quoted #####################################

(define <Quoted>
  (new (*parser (char #\'))
       (*parser <Sexpr>)
       (*caten 2)
       (*pack-with
	(lambda (tag e)
	  `(,tag ,e)))
  done))

; ##################################### QuasiQuoted #####################################

(define <QuasiQuoted>
  (new (*parser (char #\`))
       (*parser <Sexpr>)
       (*caten 2)
       (*pack-with
	(lambda (tag e)
	  `(,tag ,e)))
  done))

; ##################################### Unquoted #####################################

(define <Unquoted>
  (new (*parser (char #\,))
       (*parser <Sexpr>)
       (*caten 2)
       (*pack-with
	(lambda (tag e)
	  `(,tag ,e)))
  done))

; ##################################### UnquoteAndSpliced #####################################

(define <UnquotedAndSpliced>
  (new (*parser (word-ci ",@"))
       (*parser <Sexpr>)
       (*caten 2)
       (*pack-with
	(lambda (tag e)
	  `(,(list->string tag) ,e)))
  done))