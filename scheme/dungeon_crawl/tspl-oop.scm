;;Code from The Scheme Programming Langauge, Version 4 by Kent Dybvig
;;http://www.scheme.com/tspl/

(library (tspl oop)
  (export define-object send-message)
  (import (rnrs))

 ; define-object creates an object constructor that uses let* to bind
 ; local fields and letrec to define the exported procedures.  An
 ; object is itself a procedure that accepts messages corresponding
 ; to the names of the exported procedures.  The second pattern is
 ; used to allow the set of local fields to be omitted.
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
       (obj 'msg arg ...)])))


