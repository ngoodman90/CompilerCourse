; test 148
((lambda (int) (if (boolean? (char? (integer->char int))) 'ok)) 5)
