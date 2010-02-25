(define test-tile (tile-obj "@" 0 0 0 (lambda () #t) (lambda () #t) (lambda () #t) '() '()))

(print (send-message test-tile get-symbol) "\n")
