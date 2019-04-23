;gnu clisp 2.49

(defun start () (seq
    (print "Key-Value database started!")
    (print "/put k v - for setting 'v' value for key 'k' ")
    (print "/get k - getting value of 'key' ")
    (parseCommand (map))
))

(defun parseCommand (curMap) 
    (let ((line (split " " (read))))
    
        (let (
            (command (car line))
            (args (cdr line)))
            (if (= "put" command)
                (let (
                        (key (car args))
                        (val (cdr args))
                    )
                    (parseCommand (put key val curMap))
                )
                (if (= "get" command)
                    (seq 
                        (print (lookup (car args) curMap))
                        (parseCommand curMap)
                    )
                    (if (= "quit" command)
                        (seq
                            (print "exiting app")
                            ()
                        )
                        (seq 
                            (print (concat "command " (concat command " not found") ) )
                            (parseCommand curMap)
                        )
                    )
                )
            )

        )
    )

)

(start)