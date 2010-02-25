;;Hello World example for Gambit/NCurses

;;Include our headers
(c-declare "#include <ncurses.h>")

;;Define the function
(define hello-world
    ;;C lambda performs c commands, this is a basic ncurses
    ;;example that inits the screen, prints our string, waits 
    ;;for input so it stays on the screen, and then kills the window
    (c-lambda () int "initscr(); printw(\"Hello World\"); refresh(); getch(); endwin();" ))

;;Gotta remember to call it!
(hello-world)
