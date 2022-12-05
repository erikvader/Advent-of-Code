(defun move9000 (crates num from to)
  (iter (for i from 1 to num)
        (push (pop (nth from crates))
              (nth to crates))))

(defun move9001 (crates num from to)
  (let ((stack (subseq (nth from crates) 0 num)))
    (setf (nth from crates)
          (remove-if (constantly t) (nth from crates) :count num))
    (setf (nth to crates)
          (append stack (nth to crates)))))

(defun followsteps (cratesmoves movefun)
  (destructuring-bind (crates . moves) cratesmoves
    (iter (for m in moves)
          (destructuring-bind (num from to) m
            (funcall movefun crates num (1- from) (1- to)))
          (finally (return (->
                            (mapcar #'car crates)
                            (coerce 'string)))))))

(defun part1 (cratesmoves)
  (followsteps cratesmoves #'move9000))

(defun part2 (cratesmoves)
  (followsteps cratesmoves #'move9001))

(defparameter *parser* (aoc:boll
                        (aoc:map-cons (aoc:each-line 'list
                                                     (aoc:boll #'reverse (aoc:chars 'list)))
                                      (aoc:boll
                                       (aoc:each-line 'list
                                                      (aoc:regex 'list "move " :number " from " :number " to " :number))
                                       #'car))
                        (aoc:paragraphs)))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer "PTWLTDSJV")
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer "WZMFVGGZP")
