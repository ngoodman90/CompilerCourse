
(load "pc.scm")

;digit
(define <digit-0-9>
  (range #\0 #\9))

;digit without 0
(define <digit-1-9>
  (range #\1 #\9))

;natural number
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

;integers
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

;rational numbers (Fraction)
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

;hex digit
(define <hex-digit>
  (let ((zero (char->integer #\0))
 (lc-a (char->integer #\a))
 (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
  (*pack
   (lambda (ch)
     (- (char->integer ch) zero)))

  (*parser (range #\a #\f))
  (*pack
   (lambda (ch)
     (+ 10 (- (char->integer ch) lc-a))))

  (*parser (range #\A #\F))
  (*pack
   (lambda (ch)
     (+ 10 (- (char->integer ch) uc-a))))

  (*disj 3)
  done)))

;hex unicode
(define <XX>
  (new (*parser <hex-digit>)
       (*parser <hex-digit>)
       (*caten 2)
       (*pack-with
 (lambda (h l)
   (+ l (* h 16))))
       done))

(define <XXXX>
  (new (*parser <XX>)
       (*parser <XX>)
       (*caten 2)
       (*pack-with
 (lambda (h l)
   (+ l (* 256 h))))
       done))

(define <HexChar>
  (new (*parser (word-ci "\\{0x"))

       (*parser <XXXX>)
       (*parser <XX>)
       (*disj 2)
       (*pack integer->char)

       (*parser (char #\}))
       (*caten 3)
       (*pack-with (lambda (_< ch _>) ch))
       done))
       
(define <StringHexChar>
  (new (*parser (word-ci "\\x"))
       (*parser <HexChar>) *star
       ;TODO add pack with
       (*caten 2)
       done))

;make string meta char
(define ^<MetaChar>
  (lambda (str ch)
    (new (*parser (word str))
  (*pack (lambda (_) ch))
  done)))

;string meta char
(define <StringMetaChar>
  (new (*parser (^<MetaChar> "\\\\" #\\))
       (*parser (^<MetaChar> "\\\"" #\"))
       (*parser (^<MetaChar> "\\n" #\newline))
       (*parser (^<MetaChar> "\\r" #\return))
       (*parser (^<MetaChar> "\\t" #\tab))
       (*parser (^<MetaChar> "\\f" #\page)) ; formfeed
       (*parser (^<MetaChar> "\\{lambda}" (integer->char 955)))
       (*parser (^<MetaChar> "\\{alef}" (integer->char 1488)))
       (*parser (^<MetaChar> "\\{bismillah}" (integer->char 65021)))
       (*parser (^<MetaChar> "\\{smiley}" (integer->char 9786)))

       (*disj 11)
       done))
       
;string char
(define <StringChar>
  (new (*parser <StringMetaChar>)

       (*parser <any-char>)

       (*parser (char #\"))
       (*parser (char #\\))
       (*disj 2)

       *diff
       (*disj 2)
       done))
       
(define <Symbol>
  (new (*paser <StringChar>)
       (*paser <StringChar>) *star
       (*caten 2)
       done))

;string
(define <String>
  (new (*parser (char #\"))
       (*parser <StringChar>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
 (lambda (open-delim chars close-delim)
   (list->string chars)))

       done))

;boolean
(define <Boolean>
  (new (*parser (char #\#))
       
       (*parser (char #\t))
       (*parser (char #\f))
       (*disj 2)
       
       (*caten 2)
       (*pack-with
	(lambda (ht pred)
	 (list->string
	 '(,ht ,@pred))))
	done))
       
;InfixPrefixExtensionPrefix
(define <InfixPrefixExtensionPrefix>
  (new (*parser (char #\#))
       
       (*parser (char #\#))
       (*parser (char #\%))
       (*disj 2)
       
       (*caten 2)
       (*pack-with
	(lambda (...)))
	
	done))
	
;PowerSymbol
(define <PowerSymbol>
  (new (*parser (char #\*))
       (*parser (char #\*))
       (*caten 2)
       
       (*parser (char #\^))
       
       (*disj 2)
       (*pack-with
	(lambda (...)))
	
	done))

(define <InfixAdd>
  (new (*parser <Number>)
       (*parser (char #\+))
       (*parser <InfixExpression>)
       (*caten 3)
       (*pack-with
	(lambda (num add expression)
	  (+ num expression)))
       done))
       
(define <InfixNeg>
  (new (*parser (char #\-))
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with
	(lambda (neg num)
	  (- num)))
       done))
	
(define <InfixSub>
  (new (*parser <Number>)
       (*parser (char #\-))
       (*parser <InfixExpression>)
       (*caten 3)
       (*pack-with
	(lambda (num sub expression)
	  (- num expression)))
       done))
       
(define <InfixMul>
  (new (*parser <Number>)
       (*parser (char #\*))
       (*parser <InfixExpression>)
       (*pack-with
	(lambda (num mul expression)
	  (* num expression)))
       done))
	
(define <InfixDiv>
  (new (*parser <Number>)
       (*parser (char #\/))
       (*parser <InfixExpression>)
       (*guard (lambda (n) (not (zero? n))))
       (*pack-with
	(lambda (num div expression)
	  (/ num expression)))
       done))
       
(define <InfixPow>
  (new (*parser <Number>)
       (*parser <PowerSymbol>)
       (*parser <InfixExpression>)
       (*pack-with
	(lambda (num pow expression)
	  (^ num expression)))
       done))
       
(define <action-symbol>
  (new (*parser (char #\+))
       (*parser (char #\-))
       (*parser (char #\*))
       (*parser <PowerSymbol>)
       (*parser (char #\/))
       (*disj 5)
       done))
       
(define <InfixSymbol>
  (new (*parser <Symbol>)
       (*parser <action-symbol>)
       *diff
       done))
	
(define <InfixExpression>
  (new (*parser <InfixAdd>)
       (*parser <InfixNeg>)
       (*parser <InfixSub>)
       (*parser <InfixMul>)
       (*parser <InfixDiv>)
       (*parser <InfixPow>)
       (*parser <InfixArrayGet>)
       (*parser <InfixFuncall>)
       (*parser <InfixParen>)
       (*parser <InfixSexprEscape>)
       (*parser <InfixSymbol>)
       (*parser <Number>)
       
       (*disj 12)
  done))
 
 