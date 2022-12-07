;; load in dependencies
(require "cl-ppcre")

;; read in file line by line
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop 
      for line = (read-line stream nil)
      while line
      collect line)))

;; format character found, if none return nil
(defun format-character (input)
  (cl-ppcre:scan-to-strings "[A-Z]" input))

;; push in new values found onto the existing stack to build up the current boxes
(defun build-stack-values (new-entries existing-stack)
  (if (not existing-stack)
      (loop 
        for a in new-entries
        collect (list (format-character a)))
      (loop
        for a in new-entries
        for b in existing-stack
        collect (append b (list (format-character a))))))

;; extract starting stack
(defun get-starting-stacks (input)
  (loop with temp
    for line in input
    until (not (cl-ppcre:all-matches-as-strings "\\[[A-Z]\\]|\\s{4}" line))
    do (setf temp (build-stack-values (cl-ppcre:all-matches-as-strings "\\[[A-Z]\\]|\\s{4}" line) temp))
    finally (return (values temp))))

;; remove nils for the stack
(defun remove-nil-recursively (x)
  (if (listp x)
      (mapcar #'remove-nil-recursively
              (remove nil x))
      x))

;; format integers
(defun format-ints (l)
  (loop 
    for li in l
    collect (parse-integer li)))

;; decode instructions
(defun determine-instructions (input idx)
  (loop 
    for i in (nthcdr idx input)
    collect (format-ints (cl-ppcre:all-matches-as-strings "\\d+" i))))

;; move from one stack to another
(defun move-stack (total-move from to stack)
  (loop
    for i from 0 below total-move by 1
    do (push (pop (elt stack (- from 1))) (elt stack (- to 1)))
    finally (return (values stack))))

;; execute instructions returned
(defun execute-instructions (stack instructions)
   (loop
         for instruction in instructions
         do (setf stack (move-stack (elt instruction 0) (elt instruction 1) (elt instruction 2) stack))
         finally (return (values stack))))

;; get first item in stacks
(defun get-first-in-stacks (stack)
  (loop
    for s in stack
    collect (car s)))

;; move stack part 2
(defun move-stack-2 (total-move from to stack)
  (setf (elt stack (- to 1)) (nconc (loop 
                                      for i from 0 below total-move by 1
                                      collect (pop (elt stack (- from 1)))
                                      ) (elt stack (- to 1))))
  stack)

;; execute instructions part two
(defun execute-instructions-2 (stack instructions)
  (loop
    for instruction in instructions
    do (setf stack (move-stack-2 (elt instruction 0) (elt instruction 1) (elt instruction 2) stack))
    finally (return (values stack))))

;; Part 1
(get-first-in-stacks (execute-instructions (remove-nil-recursively (get-starting-stacks (get-file "input.txt")))
                                           (determine-instructions (get-file "input.txt") 10)))  ;; input = 10, example = 5

;; Part 2
(get-first-in-stacks (execute-instructions-2 (remove-nil-recursively (get-starting-stacks (get-file "input.txt")))
                                             (determine-instructions (get-file "input.txt") 10)))
