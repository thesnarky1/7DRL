;;Ray casting test

;;http://roguebasin.roguelikedevelopment.org/index.php?title=Eligloscode

(define test-env 
        `((_ _ + _ _ _ _ _ _ _)
          (_ _ + _ _ _ _ _ _ _) 
          (_ _ + _ @ _ _ _ _ _) 
          (_ _ + _ _ _ _ _ _ _) 
          (_ _ + _ _ _ _ _ _ _)))

;;Methods to find out how many elements in an array
(define arr-num
    (lambda (arr num)
        (if (null? list)
            #f
            (if (pair? arr)
                (arr-num (cdr arr) (+ num 1))
                num))))

(define arr-num-help
    (lambda (arr)
        (arr-num arr 0)))

;;Functions to write out an environmental array
(define write-env
    (lambda (env)
        (if (pair? env)
            (begin
                (write-row (car env))
                (newline)
                (write-env (cdr env))))))

(define write-row
    (lambda (ls)
        (if (pair? ls)
            (begin
                (print (car ls) " ")
                (write-row (cdr ls))))))

(write-env test-env)
