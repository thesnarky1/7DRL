;;Ray casting test

;;http://roguebasin.roguelikedevelopment.org/index.php?title=Eligloscode

(define char-x 1)
(define char-y 1)
(define view-radius 3)

(define impassable-tiles (list #\# #\~))
(define opaque-tiles (list #\#))

(define test-env (list 
                    "###################"
                    "#...#.............#" 
                    "#...#...#~~~~~....#" 
                    "#.......#~~~~~....#" 
                    "###################"))
(define create-env
    (lambda (env)
        (if (null? env)
            '()
            (cons (create-env-row (string->list (car env))) (create-env (cdr env))))))

(define create-env-row
    (lambda (env-row)
        (if (null? env-row)
            '()
            (cons (create-env-cell (car env-row)) (create-env-row (cdr env-row))))))

(define create-env-cell
    (lambda (env-cell)
        (list env-cell #f)))

(define write-env
    (lambda (env)
        (if (null? env)
            (newline)
            (begin
                (write-env-row (car env))
                (write-env (cdr env))))))

(define write-env-row
    (lambda (env-row)
        (if (null? env-row)
            (newline)
            (begin
                (write-env-cell (car env-row))
                (write-env-row (cdr env-row))))))

(define write-env-cell
    (lambda (env-cell)
        (let ((char (car env-cell))
              (visible (cadr env-cell)))
            (if visible
                (print char)
                (print " "))
            (print " "))))

(define clear-fov
    (lambda (env)
        (if (not (null? env))
            (begin
                (clear-fov-row (car env))
                (clear-fov (cdr env))))))

(define clear-fov-row
    (lambda (env-row)
        (if (not (null? env-row))
            (begin
                (clear-fov-cell (car env-row))
                (clear-fov-row (cdr env-row))))))

(define clear-fov-cell
    (lambda (env-cell)
        (set-cdr! env-cell (list #f))))

(define update-fov
    (lambda (env x y)
        (clear-fov env)))

(define our-env (create-env test-env))
