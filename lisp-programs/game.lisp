;gnu clisp 2.49

(defun start ()
    
    (play (askBethinkNumber))
    
)

(defun askBethinkNumber () (seq

    (print "Input number to be guessed: ")
    (read-int)
    
))

(defun play (x)
    
    (let ((res (askToGuess)))
     
		(if (< res x)
			(seq
				(print "Try greater value")
				(play x)
			)
			(if (> res x)
				(seq
					(print "Try smaller value")
					(play x)
				)
				(print "You are right!")
			)
		)
	)
)

(defun askToGuess () (seq

    (print "Try to guess the number: ")
    (read-int)
    
))

(start)