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


;;Real map should be a vector of tile vectors... 
;;read in something like the below and hold the vector to allow for 
;;simple getting of x,y coordinates
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
     (set-z         (lambda (new-z) (set! z new-z)))
     (get-hp        (lambda () hp))
     (set-hp        (lambda (new-hp) (set! hp new-hp)))
     (get-max-hp    (lambda () max-hp))
     (get-str       (lambda () str))
     (get-speed     (lambda () speed))
     (set-speed     (lambda (new-speed) (set! speed new-speed)))
     (get-inv       (lambda() inv))))

;;Tile object holds our tile properties
(define-object (tile-obj symbol x y z passable visible living item)
    ((get-symbol    (lambda () symbol))
     (set-symbol    (lambda (new-symbol) (set! symbol new-symbol)))
     (get-x         (lambda () x))
     (set-x         (lambda (new-x) (set! x new-x)))
     (get-y         (lambda () y))
     (set-y         (lambda (new-y) (set! y new-y)))
     (get-z         (lambda () z))
     (set-z         (lambda (new-z) (set! z new-z)))
     (get-passable  (lambda () passable))
     (set-passable  (lambda (new-passable) (set! passable new-passable)))
     (get-visible   (lambda () visible))
     (set-visible   (lambda (new-visible) (set! visible new-visible)))
     (get-living    (lambda () living))
     (set-living    (lambda (new-living) (set! living new-living)))
     (get-item      (lambda () item))
     (set-item      (lambda (new-item) (set! item new-item)))
     ))


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

;;;;;;;;;;;;;;;;;;
;;Game Functions;;
;;;;;;;;;;;;;;;;;;

;;Function to Move a living object
(define move-living 
    (lambda (living src dest)
        (if ((send-message src get-passable))
            (if ((send-message dest get-passable))
                (let ((enemy (send-message dest get-living)))
                    (if (null? enemy)
                        (let ((x (send-message dest get-x))
                              (y (send-message dest get-y))
                              (z (send-message dest get-z)))
                            (send-message src set-living '())
                            (send-message dest set-living (list living))
                            (send-message living set-x x)
                            (send-message living set-y y)
                            (send-message living set-z z))))))))

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

;;Function to prepare our overall map object
(define create-tile-set
    (lambda (map)
        (list->vector (create-map-vector map 0))))

;;Function that creates the entire map vector
(define create-map-vector
    (lambda (source y)
        (let ((curr-row (car source)))
            (if (= (length source) 1)
                (cons (list->vector (create-tiles-from-string (string->list curr-row)
                                                              y
                                                              0))
                      '())
                (cons (list->vector (create-tiles-from-string (string->list curr-row)
                                                              y
                                                              0))
                      (create-map-vector (cdr source) (+ y 1)))))))

;;Function that creates a Tile vector given a string
(define create-tiles-from-string
    (lambda (char-list y x)
        (let ((curr-char (car char-list)))
            (if (= (length char-list) 1)
                (cons (tile-from-char curr-char y x) '())
                (cons (tile-from-char curr-char y x)
                      (create-tiles-from-string (cdr char-list)
                                                y
                                                (+ x 1)))))))

;;Function to create a tile given a characetr
(define tile-from-char
    (lambda (char y x)
        (if (char=? char #\#)
            (tile-obj char x y 0 (lambda () #f) (lambda () #f) '() '())
            (tile-obj char x y 0 (lambda () #t) (lambda () #t) '() '()))))


;;Function to draw all our map objects
(define draw-whole-map
    (lambda (map character)
        (draw-tile-set map 0)
        (ncurses-draw-living character)
        (ncurses-move-cursor (send-message character get-y) (send-message character get-x))))

;;Function to draw a tileset
(define draw-tile-set
    (lambda (tile-set index)
        (if (< index (vector-length tile-set))
            (begin 
                (draw-tile-row (vector-ref tile-set index) 0)
                (draw-tile-set tile-set (+ index 1))))))

;;Function to draw a tile row 
(define draw-tile-row
    (lambda (tile-set-row index)
        (if (< index (vector-length tile-set-row))
            (let ((curr-tile (vector-ref tile-set-row index)))
                (move-draw-char (send-message curr-tile get-y) 
                                (send-message curr-tile get-x)
                                (send-message curr-tile get-symbol))
                (draw-tile-row tile-set-row (+ index 1))))))

;;Function to fetch a tile identified by X/Y coords
(define fetch-tile
    (lambda (map x y)
        (if (< y (vector-length map))
            (let ((row (vector-ref map y)))
                (if (< x (vector-length row))
                    (vector-ref row x)
                    #f))
            #f)))

;;;;;;;;;;;;;
;;GAME CODE;;
;;;;;;;;;;;;;

;;init the screen
(ncurses-init)

;;Init our character
(define character (living "Segfault" #\@ 5 5 0 100 100 10 '() 15))

;;Init the map
(define our-map (create-tile-set test-map))

;;Draw our map to start the view
(draw-whole-map our-map character)

;;Block for input and deal with it as it comes
(let loop ((test-char (get-input-character)))
    (if (not (eq? test-char 27))
        ;:Get characters x/y and tile for future reference
        (let* ((char-x (send-message character get-x))
              (char-y (send-message character get-y))
              (char-tile (fetch-tile our-map char-x char-y)))
            ;;Case the char and deal with the input
            (case test-char
;;                [(108) (send-message character set-x (+ (send-message character get-x) 1))] ;;l - move right
;;                [(107) (send-message character set-y (- (send-message character get-y) 1))] ;;k - move up
;;                [(106) (send-message character set-y (+ (send-message character get-y) 1))] ;;j - move down
;;                [(104) (send-message character set-x (- (send-message character get-x) 1))] ;;h - move left
                [(108) (move-living character char-tile (fetch-tile our-map (+ char-x 1) char-y))] ;;l - move right
                [(107) (move-living character char-tile (fetch-tile our-map char-x (- char-y 1)))] ;;k - move up
                [(106) (move-living character char-tile (fetch-tile our-map char-x (+ char-y 1)))] ;;j - move down
                [(104) (move-living character char-tile (fetch-tile our-map (- char-x 1) char-y))] ;;h - move left
            )
            ;;Refresh the screen
            (draw-whole-map our-map character)
            (ncurses-refresh-screen)
            (loop (get-input-character)))

        ;;Kill off the screen
        (ncurses-end)))

;;Encourage them to return
(print "Farewell hero!\n")

