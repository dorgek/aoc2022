;; read in file line by line
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop 
      for line = (read-line stream nil)
      while line
      collect (coerce line 'list))))

;; loop through string 
(defun loop-through-string (input idx)
  (loop with temp
        for i from 0 to (length input)
        if (= (length temp) idx)
        do (setf temp  (subseq temp 0 (- idx 1)))
           (push (pop input) temp)
        else
          do (push (pop input) temp)
        until (= idx (length (remove-duplicates temp)))
        finally (return (+ 1 i))))

;; Part 1:
(loop-through-string (car (get-file "input.txt")) 4)

;; Part 2: 
(loop-through-string (car (get-file "input.txt")) 14)
