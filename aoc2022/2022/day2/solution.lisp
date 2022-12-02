(defun winner (opp me)
  (cond ((= opp me) 3)
        ((= (mod (1+ me) 3) opp) 0)
        (t 6)))

(defun part1 (rounds)
  (iter (for (o . m) in rounds)
        (sum (+ (winner o m)
                (1+ m)))))

(defun winner-inverse (opp outcome)
  (case outcome
    (0 (mod (1- opp) 3))
    (1 opp)
    (2 (mod (1+ opp) 3))))

(defun part2 (rounds)
  (iter (for (o . outcome) in rounds)
        (for m = (winner-inverse o outcome))
        (sum (+ (winner o m)
                (1+ m)))))

(defparameter *parser* (aoc:each-line 'list (aoc:boll
                                             (aoc:map-cons (aoc:index-of '(#\A #\B #\C))
                                                           (aoc:index-of '(#\X #\Y #\Z)))
                                             (aoc:map-cons (aoc:to-char) nil)
                                             (aoc:split-sep " "))))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 13565)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 12424)
