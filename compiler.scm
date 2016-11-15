
(load "pc.scm")

;natural number
(define <nat>
  (new (*parser (char #\0))
       (*pack (lambda (_) 0))

       (*parser <digit-1-9>)
       (*parser <digit-0-9>) *star
       (*caten 2)
       (*pack-with
 (lambda (a s)
   (string->number
    (list->stringtug
     `(,a ,@s)))))

       (*disj 2)
       done))

;integers
(define <int>
  (new (*parser (char #\+))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
         (lambda (++ n) n))

       (*parser (char #\-))
       (*parser <nat>)
       (*caten 2)
       (*pack-with
         (lambda (-- n) (- n)))

       (*parser <nat>)

       (*disj 3)

       done))

;rational numbers
(define <rat>
  (new (*parser <int>)
       (*parser (char #\/))
       (*parser <nat>)
       (*guard (lambda (n) (not (zero? n))))
       (*caten 3)
       (*pack-with
 (lambda (num div den)
   (/ num den)))
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

(define <hex-char>
  (new (*parser (word-ci "\\{0x"))

       (*parser <XXXX>)
       (*parser <XX>)
       (*disj 2)
       (*pack integer->char)

       (*parser (char #\}))
       (*caten 3)
       (*pack-with (lambda (_< ch _>) ch))
       done))

;make string meta char
(define ^<meta-char>
  (lambda (str ch)
    (new (*parser (word str))
  (*pack (lambda (_) ch))
  done)))

;string meta char
(define <string-meta-char>
  (new (*parser <hex-char>)
       (*parser (^<meta-char> "\\\\" #\\))
       (*parser (^<meta-char> "\\\"" #\"))
       (*parser (^<meta-char> "\\n" #\newline))
       (*parser (^<meta-char> "\\r" #\return))
       (*parser (^<meta-char> "\\t" #\tab))
       (*parser (^<meta-char> "\\f" #\page)) ; formfeed
       (*parser (^<meta-char> "\\{lambda}" (integer->char 955)))
       (*parser (^<meta-char> "\\{alef}" (integer->char 1488)))
       (*parser (^<meta-char> "\\{bismillah}" (integer->char 65021)))
       (*parser (^<meta-char> "\\{smiley}" (integer->char 9786)))

       (*disj 11)
       done))
;string char
(define <string-char>
  (new (*parser <string-meta-char>)

       (*parser <any-char>)

       (*parser (char #\"))
       (*parser (char #\\))
       (*disj 2)

       *diff
       (*disj 2)
       done))

;string
(define <string>
  (new (*parser (char #\"))
       (*parser <string-char>) *star
       (*parser (char #\"))
       (*caten 3)

       (*pack-with
 (lambda (open-delim chars close-delim)
   (list->string chars)))

       done))