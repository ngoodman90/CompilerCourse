
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
  
(define <Comment>
  (new (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
	    
       (*parser (char #\;))
       
       (*parser (const
	(lambda (c)
	  (if (>= (char->integer c) 0)
	    #t #f))))
       (*guard (lambda (n) (not (= (char->integer n) 92)))) *star ; no space or less characters
       (*pack (lambda (a) (list->string a)))
       
       (*parser (word-ci "\\n"))
       (*pack (lambda (a) (list->string a)))
       (*parser <end-of-input>)
       (*pack (lambda (a) ""))
       (*disj 2)
       
       (*caten 4)
       (*pack-with (lambda (a b c d) (list->string (list* b (string->list c)))))
;;        `(,b ,c ,d)))
  done))

(define <Boolean>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (^<MetaChar> "#t" #t))
;;        (*pack (lambda (a) `(,@a)))
       (*parser (^<MetaChar> "#f" #f))
;;        (*pack (lambda (a) `(,@a)))

       (*disj 2)
       
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))
  
; ##################################### Char #####################################

(define <CharPrefix>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (word-ci "#\\"))
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))  

(define <VisibleSimpleChar>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser <any-char>)
       (*guard (lambda (n) (> (char->integer n) 32))) ; no space or less characters
       
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
;;        (*pack-with
;; 	(lambda (n)
;; 	  n))
  done))
  
(define <NamedChar>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (^<MetaChar> " " #\space))
       (*parser (^<MetaChar> "\\n" #\newline))
       (*parser (^<MetaChar> "\\r" #\return))
       (*parser (^<MetaChar> "\\t" #\tab))
       (*parser (^<MetaChar> "\\f" #\page)) ; formfeed
       (*parser (^<MetaChar> "lambda" (integer->char 955)))
       (*parser (^<MetaChar> "alef" (integer->char 1488)))
       (*parser (^<MetaChar> "bismillah" (integer->char 65021)))
       (*parser (^<MetaChar> "smiley" (integer->char 9786)))
       (*parser (^<MetaChar> "" #\nul)) ; TODO change so it will work

       (*disj 10)
       
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))
  
(define <HexChar>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (range #\0 #\9))
       (*parser (range #\a #\f))
       
       (*disj 2)
       
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))

(define <HexUnicodeChar>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (char #\x))
       (*parser <HexChar>) *plus
       
       (*caten 3)
       (*pack-with
	(lambda (s x chars)
	  (list->string chars)))
	  
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
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
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (char #\0))
       (*caten 2)
       (*pack-with (lambda (a b) 0))

       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser <digit-1-9>)
       (*caten 2)
       (*pack-with (lambda (s n) n)) *plus
       (*pack (lambda (a) (string->number (list->string `(,@a)))))

       (*disj 2)
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))
  
(define <Integer>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
	    
       (*parser (char #\+))
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
       (*caten 2)
       (*pack-with (lambda (s n) n))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
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
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (word-ci "\\x"))
       (*parser <HexChar>) *star
       
       (*caten 3)
        (*pack-with
	  (lambda (s x ch) (list->string ch)))
	  
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))
  
(define <StringLiteralChar>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser <any-char>)
       (*guard (lambda (n) (> (char->integer n) 92))) ; no backslash (\)
       
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))

(define <StringMetaChar>
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (word-ci "\\\\"))
       (*parser (word-ci "\\\""))
       (*parser (word-ci "\\n"))
       (*parser (word-ci "\\r"))
       (*parser (word-ci "\\t"))
       (*parser (word-ci "\\f"))

       (*disj 6)
       
       (*caten 2)
       (*pack-with (lambda (a b) (list->string b)))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
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
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser <digit-0-9>)
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
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
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
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (char #\+))
       (*parser (char #\-))
       (*parser <PowerSymbol>)
       (*parser (char #\*))
       (*parser (char #\/))
       (*disj 5)
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
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
  (new (*parser <Comment>) *star			; allowing COMMENTS
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       (*parser (char #\-))
;; 	(*pack (lambda (a) -))
       (*delayed (lambda () <InfixNumberOrSymbol>))
       (*caten 3)
       (*pack-with
	(lambda (s neg num)
	  `(- ,num)))
	  
       (*caten 2)
       (*pack-with (lambda (a b) b))			; handle added COMMENT
  done))
  
;; (define <InfixNegWithSpaces>
;;   (new (*parser (const (is-white-space))) *star
;;        (*parser (char #\-))
;; ;; 	(*pack (lambda (a) -))
;;        (*delayed (lambda () <InfixNumberOrSymbol>))
;;        (*caten 3)
;;        (*pack-with
;; 	(lambda (s neg num)
;; 	  `(- ,num)))
;;   done))

  
(define <InfixNumberOrSymbol>
  (new (*parser <InfixNeg>)
       (*parser <Number>)
       
       (*parser <InfixSymbol>)
       
       (*parser <InfixExpressionContinued>)
       (*parser <epsilon>)
       (*disj 2)
       
       (*caten 2)				; allowing a symbol to continue to an Array or a Function
       
       (*disj 3)				; Neg / Number / (Symbol / Array / Function)
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
	  `(,a ,b ,c)))
       
       (*parser (char #\())					; Function
       
       (*delayed (lambda () <InfixExpression>)) ; first arguement
       
       (*parser (char #\,))
       (*delayed (lambda () <InfixExpression>))
       (*caten 2)
       (*pack-with
	(lambda (a b) `(,a ,@b)))
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
  (new (*parser <Comment>) *star			; allowing COMMENTS
       
       (*parser (const
	(lambda (c)
	  (if (<= (char->integer c) 32)
	    #t #f)))) *star				; allowing WHITE-SPACES
       
       (*parser <Boolean>)
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
       
       (*caten 2)
       (*pack-with (lambda (a b) b))
       
       (*parser <Comment>) *star			; allowing COMMENTS
       
       (*caten 3)
       (*pack-with (lambda (a b c) b))			; handle added COMMENT
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