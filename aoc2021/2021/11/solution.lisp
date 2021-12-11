(ql:quickload "aoc")

(defun flash (energy pos flashed)
  (unless (gethash pos flashed)
    (setf (gethash pos flashed) t)
    (loop for x from -1 upto 1
          for xx = (+ x (car pos))
          do (loop for y from -1 upto 1
                   for yy = (+ y (cdr pos))
                   when (and (or (/= x 0)
                                 (/= y 0))
                             (array-in-bounds-p energy xx yy))
                     do (incf (aref energy xx yy))
                     and do (when (> (aref energy xx yy) 9)
                              (flash energy (cons xx yy) flashed))))))

(defun step-energy (energy)
  (let ((flashed (make-hash-table :test #'equal :size 50)))
    (loop for x below (array-dimension energy 0)
          do (loop for y below (array-dimension energy 1)
                   do (incf (aref energy x y))
                   do (when (> (aref energy x y) 9)
                        (flash energy (cons x y) flashed))))
    (loop for f being the hash-keys in flashed
          do (setf (aref energy (car f) (cdr f))
                   0)
          count f)))

(defun part1 (energy)
  (loop repeat 100
        sum (step-energy energy)))

(defun part2 (energy)
  (loop for i from 1
        until (= (step-energy energy) 100)
        finally (return i)))

(defparameter *parser* #'aoc:integer-grid-tight)
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 1546)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 471)
