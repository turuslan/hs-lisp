(defun list< (N L)
	(if (null L)
		nil
		(if (< (first L) N)
		(cons (first L)
		(list< N (rest L))
		)
		(list< N (rest L))
		)
	)
)

(defun list>= (N L)
	(if (null L)
		nil
		(if (>= (first L) N)
		(cons (first L)
		(list>= N (rest L))
		)
		(list>= N (rest L))
		)
	)
)

(defun qsort (L)
	(if (null L)
		nil
		(append
			(qsort (list< (first L) (rest L)) )
		(cons (first L) nil)
		(qsort (list>= (first L) (rest L)) )
		)
	)
)

; start here
(print (qsort (list 1 5 3 8 2 0 nil)))