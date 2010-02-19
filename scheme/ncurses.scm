(c-declare "#include <ncurses.h>")


(define hello-world
    (c-lambda () int "initscr(); printw(\"Hello World\"); refresh(); getch(); endwin();" ))

(hello-world)

;;#include <ncurses.h>
;;
;;int main()
;;{   
;;    initscr();          /* Start curses mode          */
;;    printw("Hello World !!!");  /* Print Hello World          */
;;    refresh();          /* Print it on to the real screen */
;;    getch();            /* Wait for user input */
;;    endwin();           /* End curses mode        */
;;    return 0;
;;}
