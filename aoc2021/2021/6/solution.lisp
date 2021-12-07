(ql:quickload "aoc")

(defun shift-left (vec newvalue)
  (let ((first (aref vec 0)))
    (loop for i from 1 below (length vec)
          do (setf (aref vec (1- i))
                   (aref vec i)))
    (setf (aref vec (1- (length vec)))
          newvalue)
    first))

(defun calc-fishes (initial cycles)
  (let ((fishes (loop with v = (make-array 9)
                      for i from 0 to 8
                      do (setf (aref v i)
                               (count i initial))
                      finally (return v))))
    (dotimes (i cycles (reduce #'+ fishes))
      (let ((zeros (shift-left fishes 0)))
        (incf (aref fishes 8)
              zeros)
        (incf (aref fishes 6)
              zeros)))))

(defun part1 (numbers)
  (calc-fishes numbers 80))

(defun part2 (numbers)
  (calc-fishes numbers 256))

(defparameter *parser* (aoc:single-line-numbers))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 379114)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 1702631502303)
