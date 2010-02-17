;;Ray casting test

;;http://roguebasin.roguelikedevelopment.org/index.php?title=Eligloscode

        

(define test-env 
        `((_ _ _ _ _ _ _ _ _ _)
          (_ _ _ _ _ _ _ _ _ _) 
          (_ _ _ _ @ _ _ _ _ _) 
          (_ _ _ _ _ _ _ _ _ _) 
          (_ _ _ _ _ _ _ _ _ _)))
            
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
