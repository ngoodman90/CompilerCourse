
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
       
;;        (*parser (char #\t))
;;        (*parser (char #\f))
       (*disj 2)
       
;;        (*caten 2)
;;        (*pack-with
;; 	(lambda (ht pred)
;; 	 (list->string
;; 	 '(,ht ,@pred))))
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
	  chars))
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
;;        (*pack-with
;; 	(lambda (n) (list->string n)))
  done))

; ##################################### InfixExtension #####################################

;; ;; (define <InfixPrefixExtensionPrefix>
;; ;;   (new (*parser (word-ci "##"))
;; ;;        (*parser (word-ci "#%"))
;; ;;        (*disj 2)
;; ;;   done))
;;   
;; ;; (define <InfixExtension>
;; ;;   (new (*parser <InfixPrefixExtensionPrefix>)
;; ;;        (*parser <InfixExpression>)
;; ;;        (*caten 2)
;; ;;   done))
;;   


;; ;; 	
;PowerSymbol
(define <PowerSymbol>
  (new (*parser (char #\*))
       (*parser (char #\*))
       (*caten 2)
       
       (*parser (char #\^))
       
       (*disj 2)
  done))

;; (define <InfixAdd>
;;   (new (*parser <Number>)
;;        (*parser (char #\+))
;; ;;        (*parser <InfixExpression>)
;;        (*parser <InfixAdd>)
;;        (*caten 3)
;;        (*pack-with
;; 	(lambda (num add expression)
;; 	  (+ num expression)))
;;   done))
;; ;;        
;; ;; (define <InfixNeg>
;; ;;   (new (*parser (char #\-))
;; ;;        (*parser <InfixExpression>)
;; ;;        (*caten 2)
;; ;;        (*pack-with
;; ;; 	(lambda (neg num)
;; ;; 	  (- num)))
;; ;;   done))
;; ;; 	
;; ;; (define <InfixSub>
;; ;;   (new (*parser <Number>)
;; ;;        (*parser (char #\-))
;; ;;        (*parser <InfixExpression>)
;; ;;        (*caten 3)
;; ;;        (*pack-with
;; ;; 	(lambda (num sub expression)
;; ;; 	  (- num expression)))
;; ;;   done))
;; ;;        
;; ;; (define <InfixMul>
;; ;;   (new (*parser <Number>)
;; ;;        (*parser (char #\*))
;; ;;        (*parser <InfixExpression>)
;; ;;        (*pack-with
;; ;; 	(lambda (num mul expression)
;; ;; 	  (* num expression)))
;; ;;   done))
;; ;; 	
;; ;; (define <InfixDiv>
;; ;;   (new (*parser <Number>)
;; ;;        (*parser (char #\/))
;; ;;        (*parser <InfixExpression>)
;; ;;        (*guard (lambda (n) (not (zero? n))))
;; ;;        (*pack-with
;; ;; 	(lambda (num div expression)
;; ;; 	  (/ num expression)))
;; ;;   done))
;; ;;        
;; ;; (define <InfixPow>
;; ;;   (new (*parser <Number>)
;; ;;        (*parser <PowerSymbol>)
;; ;;        (*parser <InfixExpression>)
;; ;;        (*pack-with
;; ;; 	(lambda (num pow expression)
;; ;; 	  (^ num expression)))
;; ;;   done))
;; ;;        
;; ;; (define <action-symbol>
;; ;;   (new (*parser (char #\+))
;; ;;        (*parser (char #\-))
;; ;;        (*parser (char #\*))
;; ;;        (*parser <PowerSymbol>)
;; ;;        (*parser (char #\/))
;; ;;        (*disj 5)
;; ;;   done))
;; ;;        
;; ;; (define <InfixSymbol>
;; ;;   (new (*parser <Symbol>)
;; ;;        (*parser <action-symbol>)
;; ;;        *diff
;; ;;   done))
;; ;; 	
;; ;; (define <InfixExpression>
;; ;;   (new (*parser <InfixAdd>)
;; ;;        (*parser <InfixNeg>)
;; ;;        (*parser <InfixSub>)
;; ;;        (*parser <InfixMul>)
;; ;;        (*parser <InfixDiv>)
;; ;;        (*parser <InfixPow>)
;; ;;        (*parser <InfixArrayGet>)
;; ;;        (*parser <InfixFuncall>)
;; ;;        (*parser <InfixParen>)
;; ;;        (*parser <InfixSexprEscape>)
;; ;;        (*parser <InfixSymbol>)
;; ;;        (*parser <Number>)
;; ;;        
;; ;;        (*disj 12)
;; ;;   done))
;; 
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
;; ;;        ; How do we check for EPSILON ?
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
;; 	(*pack
;; 	  (lambda (n) (list->string n)))
       (*parser <Symbol>)
;;        (*parser <ProperList>)
;;        (*parser <ImproperList>)
;;        (*parser <Vector>)
;;        (*parser <Quoted>)
;;        (*parser <QuasiQuoted>)
;;        (*parser <Unquoted>)
;;        (*parser <UnquoteAndSpliced>)
;;        (*parser <InfixExtension>)
       (*disj 5)
;;        (*pack-with
;; 	(lambda (n) (list->string n)))
  done))
  
; ##################################### ProperList #####################################

;; ;; (define <ProperList>
;; ;;   (new (*parser (char #\())
;; ;;        (*parser <Sexpr>)
;; ;;        (*parser (char #\)))
;; ;;        (*caten 3)
;; ;;   done))

; ##################################### ImproperList #####################################

;; ;; (define <ImproperList>
;; ;;   (new (*parser (char #\())
;; ;;        (*parser <Sexpr>)
;; ;;        (*parser <Sexpr>) *star
;; ;;        (*parser (char #\.))
;; ;;        (*parser <Sexpr>)
;; ;;        (*parser (char #\)))
;; ;;        (*caten 6)
;; ;;   done))

; ##################################### Vector #####################################

;; ;; (define <Vector>
;; ;;   (new (*parser (word-ci "#("))
;; ;;        (*parser <Sexpr>) *star
;; ;;        (*parser (char #\)))
;; ;;        (*caten 3)
;; ;;   done))

; ##################################### Quoted #####################################

;; ;; (define <Quoted>
;; ;;   (new (*parser (char #\'))
;; ;;        (*parser <Sexpr>)
;; ;;        (*caten 2)
;; ;;   done))

; ##################################### QuasiQuoted #####################################

;; ;; (define <QuasiQuoted>
;; ;;   (new (*parser (char #\`))
;; ;;        (*parser <Sexpr>)
;; ;;        (*caten 2)
;; ;;   done))

; ##################################### Unquoted #####################################

;; ;; (define <Unquoted>
;; ;;   (new (*parser (char #\,))
;; ;;        (*parser <Sexpr>)
;; ;;        (*caten 2)
;; ;;   done))

; ##################################### UnquoteAndSpliced #####################################

;; ;; (define UnquoteAndSpliced
;; ;;   (new (*parser (word-ci ",@"))
;; ;;        (*parser <Sexpr>)
;; ;;        (*caten 2)
;; ;;   done))