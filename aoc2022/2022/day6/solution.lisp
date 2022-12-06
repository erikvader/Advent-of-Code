(defun all-unique-p (packet)
  (iter (for i index-of-vector packet)
        (always (iter (for j from (1+ i) below (length packet))
                      (never (eql (aref packet i)
                                  (aref packet j)))))))

(defun find-start (chars size)
  (let ((packet (make-array size :initial-contents (subseq chars 0 size))))
    (iter (for c in chars)
          (for i from 0)
          (setf (aref packet (mod i size))
                c)
          (thereis (and (all-unique-p packet)
                        (1+ i))))))

(defun part1 (chars)
  (find-start chars 4))

(defun part2 (chars)
  (find-start chars 14))

(defparameter *parser* (aoc:boll
                        (aoc:chars 'list)
                        #'car))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 1802)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 3551)
