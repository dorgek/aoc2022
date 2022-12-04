;; read in file line by line
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop 
      for line = (read-line stream nil)
      while line
      collect line)))

;; find range of values
(defun range (max &key (min 0) (step 1))
  (loop 
    for n from min below (+ 1 max) by step
    collect n))

(defun format-input-per-elf (li)
  (range (parse-integer (elt li 1)) :min (parse-integer (elt li 0))))

;; takes in an array and expands the input to a range
(defun expand-out (li)
  (loop 
    for el in li
    collect (format-input-per-elf (split-sequence:SPLIT-SEQUENCE #\- el))))

;; expand out input to list all possible values
(defun format-input (lines)
  (loop 
    for li in lines
    collect (expand-out (split-sequence:SPLIT-SEQUENCE #\, li))))

;; check if there is not any unique elements from one set to another
(defun is-not-unique (s1 s2)
  (or (not (set-difference s1 s2)) (not (set-difference s2 s1))))

;; find fully overlapping ranges
(defun find-overlapping-ranges (input)
  (loop 
    for li in input
    collect (is-not-unique (elt li 0) (elt li 1))))

;; find any overlapping ranges
(defun no-overlapping-ranges (s1 s2)
  (not (not (intersection s2 s1))))

;; find any overlapping ranges
(defun find-any-overlapping-ranges (input)
  (loop
    for li in input
    collect (no-overlapping-ranges (elt li 0) (elt li 1))))

;; Part one: determine number of overlapping work between pairs
(length (remove nil (find-overlapping-ranges (format-input (get-file "input.txt")))))

;; Part two: determine any overlapping work between pairs
(length (remove nil (find-any-overlapping-ranges (format-input (get-file "input.txt")))))
