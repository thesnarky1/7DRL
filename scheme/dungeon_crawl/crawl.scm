;;Sample game just to test out ncurses and some ideas for roguelike

(c-declare "#include <ncurses.h>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Junk to break out to OOP lib;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-object
  (syntax-rules ()
    [(_ (name . varlist)
        ((var1 val1) ...)
        ((var2 val2) ...))
     (define name
       (lambda varlist
         (let* ([var1 val1] ...)
           (letrec ([var2 val2] ...)
             (lambda (msg . args)
               (case msg
                 [(var2) (apply var2 args)]
                 ...
                 [else
                  (assertion-violation 'name
                    "invalid message"
                    (cons msg args))]))))))]
    [(_ (name . varlist) ((var2 val2) ...))
     (define-object (name . varlist)
                    ()
                    ((var2 val2) ...))]))

 ; send-message abstracts the act of sending a message from the act
 ; of applying a procedure and allows the message to be unquoted.
(define-syntax send-message
  (syntax-rules ()
    [(_ obj msg arg ...)
     (obj 'msg arg ...)]))



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

;;;;;;;;;;;;;;;;;;;;;;
;;Object Definitions;;
;;;;;;;;;;;;;;;;;;;;;;

;;Living object holds everything that's alive
;;Play with defaults in here?
(define-object (living name symbol x y z hp max-hp str inv speed)
    ((get-name      (lambda () name))
     (get-symbol    (lambda () symbol))
     (set-symbol    (lambda (new-symbol) (set! symbol new-symbol)))
     (get-x         (lambda () x))
     (set-x         (lambda (new-x) (set! x new-x)))
     (get-y         (lambda () y))
     (set-y         (lambda (new-y) (set! y new-y)))
     (get-z         (lambda () z))
     (get-hp        (lambda () hp))
     (get-max-hp    (lambda () max-hp))
     (get-str       (lambda () str))
     (get-speed     (lambda () speed))
     (set-speed     (lambda (new-speed) (set! speed new-speed)))
     (get-inv       (lambda() inv))))

;;Tile object holds our tile properties
(define-object (tile symbol x y z passable)
    ((get-symbol    (lambda () symbol))
     (set-symbol    (lambda (new-symbol) (set! symbol new-symbol)))
     (get-x         (lambda () x))
     (set-x         (lambda (new-x) (set! x new-x)))
     (get-y         (lambda () y))
     (set-y         (lambda (new-y) (set! y new-y)))
     (get-z         (lambda () z))
     (set-z         (lambda (new-z) (set! z new-z)))
     (get-passable  (lambda () passable))
     (set-passable  (lambda (new-passable) (set! passable new-passable)))))


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

;;;;;;;;;;;;;;;;;;;;;;;
;;Game Tick Functions;;
;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;
;;NCurses Functions;;
;;;;;;;;;;;;;;;;;;;;;

;;Function to init ncurses
(define ncurses-init
    (c-lambda () int "initscr();/*leaveok(curscr, TRUE);*/noecho();" ))

;;Function to kill NCurses
(define ncurses-end
    (c-lambda () int "endwin();" ))

;;Function to refresh the screen
(define ncurses-refresh-screen
    (c-lambda () void "refresh();"))

;;Function to block for user input
(define ncurses-input
    (c-lambda () char "___result = getch();"))

;;Function to move cursor to a given location
(define ncurses-move-cursor
    (c-lambda (int int) void "move(___arg1, ___arg2);"))

;;Function to snag the user input, 
;;will block thanks to ncurses-input
(define get-input-character
    (lambda ()
        (char->integer (ncurses-input))))

;;Function to draw a given character at a given spot
(define move-draw-char
    (c-lambda (int int char) void "mvaddch(___arg1, ___arg2, ___arg3);"))

;;Function to put a living object on the screen
(define ncurses-draw-living
    (lambda (living-object)
        (let ((x (send-message living-object get-x))
              (y (send-message living-object get-y)))
            (move-draw-char y x (send-message living-object get-symbol)))))


;;;;;;;;;;;;;;;;;
;;Map Functions;;
;;;;;;;;;;;;;;;;;

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

;;Function to draw all our map objects
(define draw-whole-map
    (lambda (map character)
        (draw-map map)
        (ncurses-draw-living character)
        (ncurses-move-cursor (send-message character get-y) (send-message character get-x))))


;;;;;;;;;;;;;
;;GAME CODE;;
;;;;;;;;;;;;;

;;init the screen
(ncurses-init)

;;Init out character
(define character (living "Segfault" #\@ 5 5 0 100 100 10 '() 15))

;;Draw our map to start the view
(draw-whole-map test-map character)

;;Block for input and deal with it as it comes
(let loop ((test-char (get-input-character)))
    (if (not (eq? test-char 27))
        (begin
            ;;Case the char and deal with the input
            (case test-char
                [(108) (send-message character set-x (+ (send-message character get-x) 1))] ;;l - move right
                [(107) (send-message character set-y (- (send-message character get-y) 1))] ;;k - move up
                [(106) (send-message character set-y (+ (send-message character get-y) 1))] ;;j - move down
                [(104) (send-message character set-x (- (send-message character get-x) 1))] ;;h - move left
            )
            ;;Refresh the screen
            (draw-whole-map test-map character)
            (ncurses-refresh-screen)
            (loop (get-input-character)))

        ;;Kill off the screen
        (ncurses-end)))

;;Encourage them to return
(print "Farewell hero!\n")
