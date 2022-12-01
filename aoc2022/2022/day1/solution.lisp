(defun summ (values)
  (reduce #'+ values :initial-value 0))

(defun part1 (elves)
  (iter (for elf in elves)
        (maximize (summ elf))))

(defun part2 (elves)
  (-> (iter (for elf in elves)
            (collect (summ elf)))
      (sort #'>)
      (subseq 0 3)
      (summ)))

(defparameter *parser* (aoc:boll
                        (aoc:each-line 'list (aoc:each-line 'list #'parse-integer))
                        (aoc:paragraphs)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 69795)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 208437)
