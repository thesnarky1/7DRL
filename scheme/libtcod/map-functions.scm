;;Functions for our maps

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
        (let ((new-tile (tile-obj char x y 0 
                                  (lambda () #t) 
                                  (lambda () #t) 
                                  (lambda () #f)
                                  '()
                                  '())))
            (if (member char IMPASSABLE-TILES)
                (send-message new-tile set-passable (lambda () #f)))
            (if (member char VISION-BLOCKING-TILES)
                (send-message new-tile set-blocks-vision (lambda () #t)))
            new-tile)))
