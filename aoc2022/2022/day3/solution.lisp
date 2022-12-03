(defun priority (type)
  (let ((c (char-code type)))
    (if (>= c 97)
        (- c 96)
        (- c 38))))

(defun part1 (sackpacks)
  (iter (for (comp1 . comp2) in sackpacks)
        (sum (-> (intersection comp1 comp2)
                 (car)
                 (priority)))))

(defun part2 (sackpacks)
  (iter (for group in (aoc:group-size 3 sackpacks))
        (for common = (iter (for (comp1 . comp2) in group)
                            (reducing (append comp1 comp2)
                                      by #'intersection)))
        (sum (-> common
                 (car)
                 (priority)))))

(defparameter *parser* (aoc:each-line 'list (aoc:boll (aoc:split-half)
                                                      (aoc:chars 'list))))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 7831)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 2683)
