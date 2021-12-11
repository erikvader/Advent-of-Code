(ql:quickload "aoc" :silent t)
(import 'alexandria:switch)

(defun part1 (instructions)
  (loop with fwd = 0 and depth = 0
        for (dir num) in instructions
        do (switch (dir :test #'string=)
             ("forward"
              (incf fwd num))
             ("down"
              (incf depth num))
             ("up"
              (decf depth num)))
        finally (return (* fwd depth))))

(defun part2 (instructions)
  (loop with fwd = 0 and depth = 0 and aim = 0
        for (dir num) in instructions
        do (switch (dir :test #'string=)
             ("forward"
              (incf fwd num)
              (incf depth (* aim num)))
             ("down"
              (incf aim num))
             ("up"
              (decf aim num)))
        finally (return (* fwd depth))))

(defparameter *parser* (aoc:each-line 'list (aoc:regex 'list ("[a-zA-Z]+" . identity) " " ("[0-9]+" . parse-integer))))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 1882980)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 1971232560)
