(define possibilities `(cat dog mouse))

(define pick-random
    (lambda (array)
        (random-integer (length possibilities))))

(pick-random possibilities)
