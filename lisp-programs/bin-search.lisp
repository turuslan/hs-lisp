
(defun guess (ileft eright)
  (if (= ileft (- eright 1))
    (seq
      (print "number is ")
      (princ ileft)
      ileft)
    (let ((mid (floor (/ (+ ileft eright) 2))))
      (seq
        (print "less than ")
        (princ mid)
        (princ "?")
        (if (= (read-int) 0)
          (guess mid eright)
          (guess ileft mid)
          )))))

(defun game ()
  (guess
    (seq (print "min (i): ") (read-int))
    (seq (print "max (i): ") (read-int))))

(game)
