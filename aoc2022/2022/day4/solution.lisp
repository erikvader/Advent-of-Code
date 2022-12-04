(defun snitt (a b)
  (cons (max (car a) (car b))
        (min (cdr a) (cdr b))))

(defun validp (a)
  (<= (car a) (cdr a)))

(defun part1 (sections)
  (iter (for (a . b) in sections)
        (for s = (snitt a b))
        (counting (or (equalp s a)
                      (equalp s b)))))

(defun part2 (sections)
  (iter (for (a . b) in sections)
        (for s = (snitt a b))
        (counting (validp s))))

(defparameter *parser* (aoc:each-line
                        'list
                        (aoc:boll
                         (aoc:map-cons (aoc:boll (aoc:map-cons #'parse-integer nil)
                                                 (aoc:split-sep "-"))
                                       nil)
                         (aoc:split-sep ","))))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 464)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 770)
