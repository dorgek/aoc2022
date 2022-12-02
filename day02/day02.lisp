;; send in file line by line
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop 
      for line = (read-line stream nil)
      while line
      collect line)))

;; format the array into a matrix of columns
(defun format-file (lines)
  (loop
        for el in lines
        collect (split-sequence:SPLIT-SEQUENCE #\Space el)))

;; takes two inputs and computes winner, 6 for player, 3 for draw, 0 for lose
;; p1 = oponent, p2 = player
(defun find-winner (p1 p2)
  (if (= (mod (+ p1 1) 3) p2)
      (+ 6 0)
      (if (= p1 p2)
          (+ 3 0)
          (+ 0 0)))) ;; TODO: how to return a singular value

;; parse into ascii with the required encoding
(defun format-character (a modifier)
  (- (char-code (coerce a 'character)) modifier))

;; Calculate the score based on the part one encoding
(defun calculate-score (lines)
  (loop 
    for li in lines
    collect (+ (find-winner (format-character (elt li 0) 65) (format-character (elt li 1) 88)) (format-character (elt li 1) 87))))

;; Part One: calaculate sum of the score for the current scheme
(print "Part one: ")
(reduce '+ (calculate-score (format-file (get-file "input.txt"))))

;; Part two: find the total score following the proper guide
;; calculate score based on who one
(defun calculate-outcome (p1 p2)
  (if (= p2 0) ;; loose outcome
      (+ 0 (mod (+ p1 2) 3) 1)
      (if (= p2 1) ;; draw outcome
          (+ 3 p1 1)
          (+ 6 (mod (+ p1 1) 3) 1))))

(defun calculate-score-part-two (lines)
  (loop 
    for li in lines
    collect (calculate-outcome (format-character (elt li 0) 65) (format-character (elt li 1) 88))))

(print "Part two: ")
(reduce '+ (calculate-score-part-two (format-file (get-file "input.txt"))))
