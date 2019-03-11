;gnu clisp 2.49

(defun start ()
    
    (play (askBethinkNumber))
    
)

(defun askBethinkNumber ()

    (print "Input number to be guessed: ")
    (read-int)
    
)

(defun play (x)
    
    (let ((res (askToGuess)))
     
		(cond
			((< res x)
				(print "Try greater value")
				(play x)
			)
			((> res x)
				(print "Try smaller value")
				(play x)
			)
			((= res x)
				(print "You are right!")
			)
		)
	)
)

(defun askToGuess ()

    (print "Try to guess the number: ")
    (read-int)
    
)

(start)