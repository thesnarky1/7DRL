;;Scheme ray casting/FOV demo
;;Adapted from pseudo code found at
;;http://roguebasin.roguelikedevelopment.org/index.php?title=Eligloscode
 
;;Prep work for 7DLR 2010 (to brush back up on my Scheme)
 
 
;;Global defines
(define char-x 1) ;;x coordinate for the fake character
(define char-y 1) ;;y coordinate for the fake character
(define VIEW-RADIUS 3) ;;View radius for FOV demo
 
(define IMPASSABLE-TILES (list #\# #\~)) ;;List of tiles we don't want to walk through (Walls and water)
(define OPAQUE-TILES (list #\#)) ;;List of tiles that will break the ray casting (walls)
 
(define test-env (list
                    "###################"
                    "#...#.............#"
                    "#...#...#~~~~~....#"
                    "#.......#~~~~~....#"
                    "###################"))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions to create the environment;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;Function to create our working environment given an array of strings such as test-env
(define create-env
    (lambda (env)
        (if (null? env)
            '()
            (cons (create-env-row (string->list (car env))) (create-env (cdr env))))))
 
;;Function that helps create-env by creating a given row
(define create-env-row
    (lambda (env-row)
        (if (null? env-row)
            '()
            (cons (create-env-cell (car env-row)) (create-env-row (cdr env-row))))))
 
;;Function to create a given env cell, helps create-env-row
(define create-env-cell
    (lambda (env-cell)
        (list env-cell #f)))
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions to display the environment;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;Function to write the env
(define write-env
    (lambda (env)
        (write-env-help env 0 0)))
 
;;Function that does the brunt of the env write
(define write-env-help
    (lambda (env x y)
        (if (null? env)
            (newline)
            (begin
                (write-env-row (car env) x y)
                (write-env-help (cdr env) x (+ y 1))))))
 
;;Function that writes a given row of the env
(define write-env-row
    (lambda (env-row x y)
        (if (null? env-row)
            (newline)
            (begin
                (write-env-cell (car env-row) x y)
                (write-env-row (cdr env-row) (+ x 1) y)))))
 
;;Function that writes out a given cell of the env
(define write-env-cell
    (lambda (env-cell x y)
        (let ((char (car env-cell)) ;;The symbol we'll possibly display
              (visible (cadr env-cell))) ;;The boolean bit of the env cell that holds if its visible or not
            (if visible ;;If this cell was marked to be seen
                (if (and (= x char-x) (= y char-y)) ;;Check if its where the character is
                    (print "@") ;;If so, lets show an @ symbol
                    (print char)) ;;Else show whatever character should be displayed
                (print " ")) ;;If this cell wasn't marked to be shown, just put a space
            (print " ")))) ;;Put a space after it for pretty printing
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Functions for the FOV demo;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;Function to update the FOV as a whole
(define update-fov
    (lambda (env char-x char-y)
        (update-fov-help env char-x char-y 0 0)))
 
;;Function that does the actual work of updating the FOV
(define update-fov-help
    (lambda (env char-x char-y x y)
        (if (not (null? env))
            (begin
                (update-fov-row (car env) char-x char-y x y)
                (update-fov-help (cdr env) char-x char-y x (+ y 1))))))
 
;;Function that updates the FOV for a given row
(define update-fov-row
    (lambda (env-row char-x char-y x y)
        (if (not (null? env-row))
            (begin
                (update-fov-cell (car env-row) char-x char-y x y)
                (update-fov-row (cdr env-row) char-x char-y (+ x 1) y)))))
 
;;Function that does the real work to update a given cell's FOV
(define update-fov-cell
    (lambda (env-cell char-x char-y x y)
        (set-cell-visible env-cell #f) ;;Set visible to false
        (let* ((dx (- x char-x))
               (dy (- y char-y))
               (distance (sqrt (+ (* dx dx) (* dy dy))))) ;;Get the distance between the character and the cell
            (if (< distance VIEW-RADIUS) ;;If we're within out viewing radius
                (set-cell-visible env-cell #t))))) ;;set the cell to be shown
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Function for the Ray casting demo;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;Function to clear the entire env
(define clear-cells
    (lambda (env)
        (if (not (null? env))
            (begin
                (clear-cells-row (car env))
                (clear-cells (cdr env))))))
 
;;Function that clears a row of the env
(define clear-cells-row
    (lambda (env-row)
        (if (not (null? env-row))
            (begin
                (clear-cells-cell (car env-row))
                (clear-cells-row (cdr env-row))))))
 
;;Function to clear a cell in the env
(define clear-cells-cell
    (lambda (env-cell)
        (set-cell-visible env-cell #f)))
 
;;Function to do the ray-cast
(define cast-rays
    (lambda (env char-x char-y)
        (clear-cells env) ;;Clear everything first
        (cast-rays-help env char-x char-y 0)))
 
;;Function to do the real work of casting some rays
(define cast-rays-help
    (lambda (env char-x char-y i)
        (if (<= i 360)
            (let ((x (cos (* i 0.01745)))
                  (y (sin (* i 0.01745))))
                  (trace-ray env char-x char-y x y (+ char-x .0) (+ char-y .0) 0)
                  (cast-rays-help env char-x char-y (+ i 16))))))
 
;;Function to trace the specific ray to its end
(define trace-ray
    (lambda (env char-x char-y x y dx dy i)
        (if (not (> i VIEW-RADIUS))
            (let* ((cell-x (round dx))
                   (cell-y (round dy))
                   (cell (get-cell env cell-x cell-y)))
                (if cell
                    (begin
                        (set-cell-visible cell #t)
                        (if (cell-opaque? cell)
                            (trace-ray env char-x char-y x y (+ dx x) (+ dy y) (+ i 1)))))))))
                
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Random helper functions;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;Function to get a given cell
(define get-cell
    (lambda (env x y)
        (get-cell-help env x y 0)))
 
;;Function does most of the real work to get a given cell
(define get-cell-help
    (lambda (env new-x new-y y)
        (if (not (null? env))
            (if (= y new-y)
                (get-cell-x (car env) new-x new-y 0)
                (get-cell-help (cdr env) new-x new-y (+ y 1)))
            #f)))
 
;;Final helper to get a given cell
(define get-cell-x
    (lambda (env-row new-x new-y x)
        (if (not (null? env-row))
            (if (= x new-x)
                (car env-row)
                (get-cell-x (cdr env-row) new-x new-y (+ x 1)))
            #f)))
 
;;Function to tell if a cell is passable
(define cell-passable?
    (lambda (cell)
        (not (member (car cell) IMPASSABLE-TILES)))) ;;Check to see if symbol is in our list of impassable tiles
 
;;Function to tell if a cell is opaque
(define cell-opaque?
    (lambda (cell)
        (not (member (car cell) OPAQUE-TILES)))) ;;Check to see if symbol is in our list of opaque tiles
 
;;Function to 'move' our 'character' to another cell
(define move-to
    (lambda (env x y)
        (if (cell-passable? (get-cell env x y)) ;;Check to make sure they can move there
            (begin
                (set! char-x x) ;;'Move' them by changing our global vars
                (set! char-y y)))))
 
;;Function to set a given cell's visibility to the given boolean
(define set-cell-visible
    (lambda (cell bool)
        (set-cdr! cell (list bool))))
 
;;;;;;;;;;;;;
;;Test code;;
;;;;;;;;;;;;;
 
;;Setup the environment
(define our-env (create-env test-env))

;;Run the fov-demo
(define fov-demo
    (lambda (env)
        (print "FOV demo, use h, j, k, and l to move, q to quit\n")
        (update-fov our-env char-x char-y)
        (write-env our-env)
        (let read-loop ((x (read-char)))
            (if (not (or (char=? x #\q) (char=? x #\newline)))
                (begin
                    (case x
                        [(#\l) (let ((new-x (+ char-x 1))
                                     (new-y char-y))
                                    (move-to our-env new-x new-y))]
                        [(#\k) (let ((new-x char-x)
                                     (new-y (- char-y 1)))
                                    (move-to our-env new-x new-y))]
                        [(#\j) (let ((new-x char-x)
                                     (new-y (+ char-y 1)))
                                    (move-to our-env new-x new-y))]
                        [(#\h) (let ((new-x (- char-x 1))
                                     (new-y char-y))
                                    (move-to our-env new-x new-y))])
                    (update-fov our-env char-x char-y)
                    (write-env our-env)
                    (read-loop (read-char)))
                (case x
                    [(#\q) (print "--End of FOV Demo--\n")]
                    [(#\newline) (read-loop (read-char))])))))
        

;;Run the ray-casting-demo
(define ray-casting-demo
    (lambda (env)
        (print "Ray casting demo, use h, j, k, and l to move, q to quit\n")
        (cast-rays our-env char-x char-y)
        (write-env our-env)
        (let read-loop ((x (read-char)))
            (if (not (or (char=? x #\q) (char=? x #\newline)))
                (begin
                    (case x
                        [(#\l) (let ((new-x (+ char-x 1))
                                     (new-y char-y))
                                    (move-to our-env new-x new-y))]
                        [(#\k) (let ((new-x char-x)
                                     (new-y (- char-y 1)))
                                    (move-to our-env new-x new-y))]
                        [(#\j) (let ((new-x char-x)
                                     (new-y (+ char-y 1)))
                                    (move-to our-env new-x new-y))]
                        [(#\h) (let ((new-x (- char-x 1))
                                     (new-y char-y))
                                    (move-to our-env new-x new-y))])
                    (cast-rays our-env char-x char-y)
                    (write-env our-env)
                    (read-loop (read-char)))
                (case x                                         ;;Enter or q was pressed
                    [(#\q)        (print "--End of Ray casting demo--\n")]          ;;If q, lets quit
                    [(#\newline)  (read-loop (read-char))]))))) ;;If enter just read the next char, it happens  

;;Run our demos
(fov-demo our-env)
(ray-casting-demo our-env)
