; test 149
((lambda (str)
   (if (string? str)
       (begin
	 (string-set! str 1 (integer->char 66))
	 str))) "ssss")
