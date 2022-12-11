(defparameter *milestones* '(20 60 100 140 180 220))

(defun part1 (instrs)
  (iter (with cycle = 1) ;;NOTE: this should be 0, but it doesn't want to work then...
        (with x = 1)
        (for inst in instrs)
        (incf cycle)
        (when (member cycle *milestones*)
          (sum (* cycle x)))
        (when inst
          (incf x inst)
          (incf cycle)
          (when (member cycle *milestones*)
            (sum (* cycle x))))))

(defun on-sprite-p (x cycle)
  (-> (- x (1- cycle))
      (abs)
      (<= 1)))

(defun crt-print (x cycle)
  (when (= 0 (mod cycle 40))
    (terpri))
  (if (on-sprite-p x (1+ (mod cycle 40)))
      (princ "#")
      (princ ".")))

(defun part2 (instrs)
  (iter (with cycle = 0)
        (with x = 1)
        (for inst in instrs)
        (crt-print x cycle)
        (incf cycle)
        (when inst
          (crt-print x cycle)
          (incf cycle)
          (incf x inst))))

(defparameter *parser* (aoc:each-line
                        'list
                        (aoc:branch (aoc:boll #'parse-integer
                                              #'cdr
                                              (aoc:split-at 4)
                                              (aoc:guard (complement (aoc:line-equal "noop")))))))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 14040)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer "ZGCJZJFL")
