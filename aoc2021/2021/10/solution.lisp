(ql:quickload "aoc")

(defun the-other (opening)
  (ecase opening
    (#\< #\>)
    (#\{ #\})
    (#\( #\))
    (#\[ #\])))

(defun is-opening (paren)
  (member paren '(#\< #\( #\{ #\[)))

(defun syntax-score (closing)
  (ecase closing
    (#\> 25137)
    (#\} 1197)
    (#\] 57)
    (#\) 3)))

(defun auto-score (closing)
  (ecase closing
    (#\> 4)
    (#\} 3)
    (#\] 2)
    (#\) 1)))

(defun syntax-check (line)
  (loop with stack = nil
        for p in line
        do (cond ((is-opening p)
                  (push (the-other p) stack))
                 ((null stack)
                  (return 'incomplete))
                 ((not (eql p (car stack)))
                  (return p))
                 (t
                  (pop stack)))
        finally (when stack
                  (return stack))))

(defun part1 (parenses)
  (loop for line in parenses
        for check = (syntax-check line)
        when (characterp check)
          sum (syntax-score check)))

(defun part2 (parenses)
  (loop for line in parenses
        for check = (syntax-check line)
        when (listp check)
          collect (reduce (lambda (s p)
                            (+ (* 5 s)
                               (auto-score p)))
                          check
                          :initial-value 0)
            into scores
        finally (return (nth (floor (length scores) 2)
                             (sort scores #'<)))))

(defparameter *parser* (aoc:each-line (aoc:chars)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 288291)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 820045242)
