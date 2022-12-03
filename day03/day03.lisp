;; send in file line by line
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop 
      for line = (read-line stream nil)
      while line
      collect line)))

;; convert string to array
(defun format-string-to-array (str)
  (loop
    for ch across str
    collect ch))

;; split loop in half 
(defun split-in-half (arr) 
  (loop with l = arr
        for x = l then (cdr x)
        for y = x then (cddr y)
        when (null y)
        return (list (ldiff l x) x)))

;; format the file input
(defun format-input (lines)
  (loop
        for li in lines
        collect (format-string-to-array li)))

;; loop and split in half
(defun split-lines-to-compartments (lines)
  (loop
    for li in lines 
    collect (split-in-half li)))

;; find duplicate values in the two compartments 
(defun find-duplicates (l1 l2)
  (intersection l1 l2))

;; calcualte numeric value of char
(defun convert-to-numeric-priority (ch)
  (if (upper-case-p ch)
      (- (char-code ch) 38)
      (- (char-code ch) 96)))

;; get the list of priority for each duplicated value - part one
(defun calculate-priority (input)
  (loop
    for i in input
    collect (convert-to-numeric-priority (first (find-duplicates (elt i 0) (elt i 1))))))

;; group into 3 lots
(defun group-into-three (input)
  (loop with group
        for li in input
          if (/= (list-length group) 3)
            do (push li group) 
          else
            collect (nreverse group) into groups and do (setf group (list li))
          finally (return (push group groups))))

;; find common values between 3 lists
(defun common-values (l1 l2 l3)
  (intersection l1 (intersection l2 l3)))

;; find badges
(defun find-badges-priority (input)
  (loop 
    for set-of-three in input
    collect (convert-to-numeric-priority (first (common-values (elt set-of-three 0) (elt set-of-three 1) (elt set-of-three 2))))))

;; Part one: 
(reduce '+ (calculate-priority (split-lines-to-compartments (format-input (get-file "input.txt")))))

;; Part two:
(reduce '+ (find-badges-priority (group-into-three (format-input (get-file "input.txt")))))
