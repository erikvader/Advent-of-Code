(ql:quickload "aoc" :silent t)
(import 'arrows:->)

(defun part1 (numbers)
  (let ((median (-> numbers
                    (sort #'<)
                    (aref (-> (length numbers)
                              (floor 2))))))
    (loop for n across numbers
          sum (abs (- median n)))))

(defun triangle (n)
  (floor (* n (1+ n)) 2))

(defun fuel (crabs pos)
  (loop for c across crabs
        sum (-> (- pos c)
                (abs)
                (triangle))))

(defun part2 (numbers)
  (let ((crabs (sort numbers #'<)))
    (loop with i = 0
          with j = (1- (length crabs))
          while (> j i)
          do (let* ((m (floor (+ j i) 2))
                    (n (1+ m))
                    (mf (fuel crabs m))
                    (nf (fuel crabs n)))
               (if (< mf nf)
                   (setf j m)
                   (setf i n)))
          finally (return (fuel crabs i)))))

(defun part2-slow (numbers)
  (let ((crabs (sort numbers #'<)))
    (loop with min-pos = (aref crabs 0)
          with min-fuel = (fuel crabs min-pos)
          for i from (aref crabs 1) to (aref crabs (1- (length crabs)))
          for fuel = (fuel crabs i)
          when (< fuel min-fuel)
            do (setf min-pos i
                     min-fuel fuel)
          finally (return min-fuel))))

(defparameter *parser* (aoc:single-line-numbers 'vector))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 335271)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 95851339)
