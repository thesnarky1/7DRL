;;Sample game just to test out ncurses and some ideas for roguelike

(c-declare "#include <ncurses.h>")
(define test-map (list
  (list "##################################################")
  (list "#..#.........#.#.................................#")
  (list "#..#..#......###.................................#")
  (list "#.....#......................#####......#........#")
  (list "#######......................~~~~#......#........#")
  (list "#........................~~~~~..~#...............#")
  (list "#.................~~~~~~~~~......................#")
  (list "#................~~~~~~..........................#")
  (list "#........#.......~~~~..................###+###...#")
  (list "########+#..##....~~~...#..............#.....#...#")
  (list "#........#.........~~....#.............#.....+...#")
  (list "#........#........~~......#............#+#####...#")
  (list "#........#.......~~..............................#")
  (list "##################################################")))

;;Global var definitions
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

;;Create our character vector
(define character-arr (make-vector char-index))

(define monster-index 0)
(define MONSTER-NAME monster-index)
(set! monster-index (+ monster-index 1))
(define MONSTER-HP monster-index)
(set! monster-index (+ monster-index 1))
(define MONSTER-STRENGTH monster-index)
(set! monster-index (+ monster-index 1))
(define MONSTER-SPEED monster-index)
(set! monster-index (+ monster-index 1))

;;Monsters vector, it holds a series of individual monster
;;vectors of the form:
;;MONSTER-NAME
;;MONSTER-HP
;;MONSTER-STRENGTH
;;MONSTER-SPEED
(define monsters (vector (vector "Orc" 20 5 10)
                         (vector "Goblin" 10 2 14)
                         (vector "Dragon" 80 20 20)))

;;Function to create a character
(define create-char
    (lambda (name)
        (vector-set! character-arr CHARACTER-NAME name)
        (vector-set! character-arr CHARACTER-HP 100)
        (vector-set! character-arr CHARACTER-MAX-HP 100)
        (vector-set! character-arr CHARACTER-STRENGTH 10)
        (vector-set! character-arr CHARACTER-SPEED 15)))

;;Function to return the character's name
(define get-char-name
    (lambda ()
        (vector-ref character-arr CHARACTER-NAME)))

;;Function to return the character's HP
(define get-char-hp
    (lambda ()
        (vector-ref character-arr CHARACTER-HP)))

;;Function to return the character's max HP
(define get-char-max-hp
    (lambda ()
        (vector-ref character-arr CHARACTER-MAX-HP)))

;;Function to return the character's strength
(define get-char-strength
    (lambda ()
        (vector-ref character-arr CHARACTER-STRENGTH)))

;;Function to return the character's speed
(define get-char-speed
    (lambda ()
        (vector-ref character-arr CHARACTER-SPEED)))

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
    (c-lambda () int "initscr();" ))

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

;;Function to snag the user input, 
;;will block thanks to the called function
(define get-input-character
    (lambda ()
        (char->integer (ncurses-input))))

;;Function to draw our map to the console
(define draw-map
    (lambda (map)
       (if (not (null? map))
            (let ((to-draw (car map)))
                (draw-map-row to-draw)
                (draw-map (cdr map))))))


;;Actual code goes below here, functions go above, 
;;no exceptions!
(ncurses-init)
(draw-map-row "Hello World")
(let loop ((test-char (get-input-character)))
    (if (not (eq? test-char 27))
        (begin
            (draw-map-row (string (integer->char test-char)))
            (loop (get-input-character)))
        (ncurses-end)))
(print "Goodbye!\n")