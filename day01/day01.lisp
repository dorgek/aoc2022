;; Read in a file from the file system and treat as a list of lines
(defun get-file (filename)
    (with-open-file (stream filename)
          (loop for line = (read-line stream nil)
                          while line
                                    collect line)))

;; Convert file from list to matrix - see https://www.reddit.com/r/lisp/comments/m5grm5/comment/gr02eaz/?utm_source=share&utm_medium=web2x&context=3
(defun list-to-matrix (list-value)
  (loop with sublist
        for el in list-value
            if (equal el "")
                collect (nreverse sublist) and do (setf sublist nil)
            else 
            do (push (parse-integer el) sublist)))

;; sum all sublists together
(defun sum-sublist (list-of-lists)
  (loop
     for li in list-of-lists
          collect (reduce '+ li)))

(defun max-item (list)
  (loop
    for item in list
      maximizing item))

(defun part-one (input-data)
  (print (max-item (sum-sublist input-data))))

(defun part-two (sums)
  (reduce '+ (loop with new-sums = sums 
                    for i below 3
                    collect (max-item new-sums)  
                    do (setf new-sums (remove (max-item new-sums) new-sums)))))


(setq data (list-to-matrix (get-file "input.txt"))) ;; why is this getting an undefined variable when it does define the variable

(print "Part One: ")
(part-one data)


(print "Part Two: ")
(setq sum-of-values (sum-sublist data))
(part-two sum-of-values)
