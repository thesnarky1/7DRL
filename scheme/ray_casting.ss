;;Ray casting test

;;http://roguebasin.roguelikedevelopment.org/index.php?title=Eligloscode

(define char-x 1)
(define char-y 1)
(define VIEW-RADIUS 3)

(define IMPASSABLE-TILES (list #\# #\~))
(define OPAQUE-TILES (list #\#))

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
        (write-env-help env 0 0)))

(define write-env-help
    (lambda (env x y)
        (if (null? env)
            (newline)
            (begin
                (write-env-row (car env) x y)
                (write-env-help (cdr env) x (+ y 1))))))

(define write-env-row
    (lambda (env-row x y)
        (if (null? env-row)
            (newline)
            (begin
                (write-env-cell (car env-row) x y)
                (write-env-row (cdr env-row) (+ x 1) y)))))

(define write-env-cell
    (lambda (env-cell x y)
        (let ((char (car env-cell))
              (visible (cadr env-cell)))
            (if visible
                (if (and (= x char-x) (= y char-y))
                    (print "@")
                    (print char))
                (print " "))
            (print " "))))

(define update-fov
    (lambda (env char-x char-y)
        (update-fov-help env char-x char-y 0 0)))

(define update-fov-help
    (lambda (env char-x char-y x y)
        (if (not (null? env))
            (begin
                (update-fov-row (car env) char-x char-y x y)
                (update-fov-help (cdr env) char-x char-y x (+ y 1))))))

(define update-fov-row
    (lambda (env-row char-x char-y x y)
        (if (not (null? env-row))
            (begin
                (update-fov-cell (car env-row) char-x char-y x y)
                (update-fov-row (cdr env-row) char-x char-y (+ x 1) y)))))

(define update-fov-cell
    (lambda (env-cell char-x char-y x y)
        (set-cell-visible env-cell #f) ;;Set visible to false
        (let* ((dx (- x char-x))
               (dy (- y char-y))
               (distance (sqrt (+ (* dx dx) (* dy dy)))))
            (if (< distance VIEW-RADIUS)
                (set-cell-visible env-cell #t)))))

(define set-cell-visible
    (lambda (cell bool)
        (set-cdr! cell (list bool))))

(define get-cell
    (lambda (env x y)
        (get-cell-help env x y 0)))

(define get-cell-help
    (lambda (env new-x new-y y)
        (if (= y new-y)
            (get-cell-x (car env) new-x new-y 0)
            (get-cell-help (cdr env) new-x new-y (+ y 1)))))

(define get-cell-x
    (lambda (env-row new-x new-y x)
        (if (= x new-x)
            (car env-row)
            (get-cell-x (cdr env-row) new-x new-y (+ x 1)))))

(define cell-passable?
    (lambda (env x y)
        (let ((cell (get-cell env x y)))
            (not (member (car cell) IMPASSABLE-TILES)))))

(define our-env (create-env test-env))
(let read-loop ((x (read-char)))
    (if (not (char=? x #\q))
        (begin
            (case x
                [(#\l) (set! char-x (+ char-x 1))]
                [(#\k) (set! char-y (- char-y 1))]
                [(#\j) (set! char-y (+ char-y 1))]
                [(#\h) (set! char-x (- char-x 1))])
            (update-fov our-env char-x char-y)
            (write-env our-env)
            (read-loop (read-char)))
        (print "Goodbye\n")))
        

