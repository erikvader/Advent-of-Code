(ql:quickload "aoc")
(use-package :iterate)

(defun reflect (x line)
  (if (< x line)
      x
      (- line (- x line))))

(defun fold (dot fld)
  (if (char= (car fld) #\x)
      (cons (reflect (car dot) (cdr fld))
            (cdr dot))
      (cons (car dot)
            (reflect (cdr dot) (cdr fld)))))

(defun fold-all (folds dot)
  (reduce #'fold folds :initial-value dot))

(defun fold-paper (folds dots)
  (let ((final (make-hash-table :test 'equal)))
    (iter (for d in dots)
          (setf (gethash (fold-all folds d)
                         final)
                t))
    final))

(defun part1 (dots folds)
  (iter (for (nil v) in-hashtable (fold-paper (list (car folds)) dots))
        (count v)))

(defun part2 (dots folds)
  (let* ((paper (fold-paper folds dots))
         (max-x (iter (for ((x . nil) nil) in-hashtable paper)
                      (maximize x)))
         (min-x (iter (for ((x . nil) nil) in-hashtable paper)
                      (minimize x)))
         (max-y (iter (for ((nil . y) nil) in-hashtable paper)
                      (maximize y)))
         (min-y (iter (for ((nil . y) nil) in-hashtable paper)
                      (minimize y))))
    (terpri)
    (iter (for y from min-y to max-y)
          (iter (for x from min-x to max-x)
                (princ (if (gethash (cons x y) paper)
                           #\#
                           #\Space)))
          (terpri))))

(defparameter *parser* (aoc:boll (aoc:header (aoc:each-line 'list (aoc:regex :pair number "," number))
                                             (aoc:each-line 'list (aoc:regex :pair "fold along " char "=" number)))
                                 (aoc:paragraphs)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 759
                     :unpack t)

(aoc:run-day-stdout #'part2 :parser *parser*
                            :unpack t
                            :expected-answer "
#  # ####  ##  ###  #### #  # ###  ### 
#  # #    #  # #  #    # # #  #  # #  #
#### ###  #    #  #   #  ##   #  # #  #
#  # #    #    ###   #   # #  ###  ### 
#  # #    #  # # #  #    # #  #    # # 
#  # ####  ##  #  # #### #  # #    #  #
")
