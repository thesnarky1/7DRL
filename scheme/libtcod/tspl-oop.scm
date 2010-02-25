
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

