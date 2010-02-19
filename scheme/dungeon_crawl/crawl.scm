;;Sample game just to test out ncurses and some ideas for roguelike

(c-declare "#include <ncurses.h>")
(define test-map (list
   "##################################################"
   "#..#.........#.#.................................#"
   "#..#..#......###.................................#"
   "#.....#......................#####......#........#"
   "#######......................~~~~#......#........#"
   "#........................~~~~~..~#...............#"
   "#.................~~~~~~~~~......................#"
   "#................~~~~~~..........................#"
   "#........#.......~~~~..................###+###...#"
   "########+#..##....~~~...#..............#.....#...#"
   "#........#.........~~....#.............#.....+...#"
   "#........#........~~......#............#+#####...#"
   "#........#.......~~..............................#"
   "##################################################"))

;;Character var definitions
(define char-index 0)
(define CHARACTER-NAME char-index)
(set! char-index (+ char-index 1))
(define CHARACTER-HP char-index)
(set! char-index (+ char-index 1))
(define CHARACTER-MAX-HP char-index)
(set! char-index (+ char-index 1))
(define CHARACTER-STRENGTH char-index)
(set! char-index (+ char-index 1))
(define CHARACTER-SPEED char-index)
(set! char-index (+ char-index 1))

(define CHARACTER-Y 5)
(define CHARACTER-X 5)

;;Create our character vector
(define character (make-vector char-index))

(define monster-index 0)
(define MONSTER-NAME monster-index)
(set! monster-index (+ monster-index 1))
(define MONSTER-HP monster-index)
(set! monster-index (+ monster-index 1))
(define MONSTER-STRENGTH monster-index)
(set! monster-index (+ monster-index 1))
(define MONSTER-SPEED monster-index)
(set! monster-index (+ monster-index 1))
(define MONSTER-ICON monster-index)
(set! monster-index (+ monster-index 1))

;;Monsters vector, it holds a series of individual monster
;;vectors of the form:
;;MONSTER-NAME
;;MONSTER-HP
;;MONSTER-STRENGTH
;;MONSTER-SPEED
;;MONSTER-ICON
(define monsters (vector (vector "Orc" 20 5 10 #\o)
                         (vector "Goblin" 10 2 14 #\g)
                         (vector "Dragon" 80 20 20 #\D)))

;;Function to create a character
(define create-char
    (lambda (name)
        (vector-set! character CHARACTER-NAME name)
        (vector-set! character CHARACTER-HP 100)
        (vector-set! character CHARACTER-MAX-HP 100)
        (vector-set! character CHARACTER-STRENGTH 10)
        (vector-set! character CHARACTER-SPEED 15)))

;;Function to return the character's name
(define get-char-name
    (lambda ()
        (vector-ref character CHARACTER-NAME)))

;;Function to return the character's HP
(define get-char-hp
    (lambda ()
        (vector-ref character CHARACTER-HP)))

;;Function to return the character's max HP
(define get-char-max-hp
    (lambda ()
        (vector-ref character CHARACTER-MAX-HP)))

;;Function to return the character's strength
(define get-char-strength
    (lambda ()
        (vector-ref character CHARACTER-STRENGTH)))

;;Function to return the character's speed
(define get-char-speed
    (lambda ()
        (vector-ref character CHARACTER-SPEED)))

;;Function to render a string for a character
(define write-char
    (lambda()
        (print (get-char-name))
        (print ": ")
        (print (get-char-hp))
        (print " of ")
        (print (get-char-max-hp))
        (print "hp")))

;;Function to do one game tick
(define do-tick
    (lambda (tmp-tick-stack)
        (if (null? tmp-tick-stack)
            (set! tick-stack '())
            (let ((curr-command (car tmp-tick-stack)))
                (eval-command curr-command)
                (do-tick (cdr tmp-tick-stack))))))

;;Function to add something to the tick stack
(define add-to-tick-stack
    (lambda (tmp-tick-stack tmp-action tmp-actor tmp-speed)
        (if (null? tmp-tick-stack)
            (cons (list tmp-speed tmp-actor tmp-action) tmp-tick-stack)
            (let ((tmp-task (car tmp-tick-stack)))
                (let ((tmp-task-speed (car tmp-task)))
                    (if (>= tmp-task-speed tmp-speed)
                        (cons tmp-task (add-to-tick-stack (cdr tmp-tick-stack) tmp-action tmp-actor tmp-speed))
                        (cons (list tmp-speed tmp-actor tmp-action) tmp-tick-stack)))))))

;;Function to evaluate a given command
(define eval-command
    (lambda (tmp-command)
        (print tmp-command)))

;;Function to init ncurses
(define ncurses-init
    (c-lambda () int "initscr();leaveok(curscr, TRUE);" ))

;;Function to kill NCurses
(define ncurses-end
    (c-lambda () int "endwin();" ))

;;Function to refresh the screen
(define refresh-screen
    (c-lambda () void "refresh();"))

;;Function to draw a row on the screen
(define draw-map-row
    (c-lambda (nonnull-char-string) void "addstr(___arg1);"))

;;Function to block for user input
(define ncurses-input
    (c-lambda () char "___result = getch();"))

;;Function to move cursor to a given location
(define ncurses-move-cursor
    (c-lambda (int int) void "move(___arg1, ___arg2);"))
        

;;Function to snag the user input, 
;;will block thanks to the called function
(define get-input-character
    (lambda ()
        (char->integer (ncurses-input))))

;;Function to draw a given character at a given spot
(define move-draw-char
    (c-lambda (int int char) void "mvaddch(___arg1, ___arg2, ___arg3);"))

;;Function to draw the map
(define draw-map
    (lambda (map)
        (let ((map-counter 0))
            (for-each (lambda (map-row)
                        (let map-row-loop ((x 0)
                                           (char (string-ref map-row 0))
                                           (map-row-len (string-length map-row)))
                            (if (< x map-row-len)
                                (begin
                                    (move-draw-char map-counter x char)
                                    (if (< (+ x 1) map-row-len)
                                        (map-row-loop (+ x 1) (string-ref map-row (+ x 1)) map-row-len)))))
                        (set! map-counter (+ map-counter 1)))
                      map))))

;;Function to put the character on the screen
(define draw-character
    (lambda ()
        (move-draw-char CHARACTER-Y CHARACTER-X #\@)))

;;Function to draw all our map objects
(define draw-whole-map
    (lambda (map)
        (draw-map map)
        (draw-character)
        (ncurses-move-cursor CHARACTER-Y CHARACTER-X)))

;;Actual code goes below here, functions go above, 
;;no exceptions!
(ncurses-init)
(draw-whole-map test-map)
(let loop ((test-char (get-input-character)))
    (if (not (eq? test-char 27))
        (begin
            (case test-char
                [(108) (set! CHARACTER-X (+ CHARACTER-X 1))]
                [(107) (set! CHARACTER-Y (- CHARACTER-Y 1))]
                [(106) (set! CHARACTER-Y (+ CHARACTER-Y 1))]
                [(104) (set! CHARACTER-X (- CHARACTER-X 1))])
            (draw-whole-map test-map)
            (refresh-screen)
            (loop (get-input-character)))
        (ncurses-end)))
(print "Goodbye!\n")
