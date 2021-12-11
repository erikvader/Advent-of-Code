(ql:quickload "aoc" :silent t)
(use-package :arrows)

(defun count-increasing (numbers)
  (->> (mapcar #'cons numbers (cdr numbers))
       (remove-if (lambda (ns) (>= (car ns) (cdr ns))))
       (length)))

(defun count-increasing2 (numbers)
  (loop for a in numbers
        for b in (cdr numbers)
        when (> b a)
          sum 1))

(defun part1 (numbers)
  (count-increasing2 numbers))

(defun part2 (numbers)
  (-> (mapcar #'+ numbers (cdr numbers) (cddr numbers))
      (count-increasing2)))

(aoc:run-day #'part1 :parser (aoc:each-line 'list #'parse-integer)
                     :expected-answer 1752)

(aoc:run-day #'part2 :parser (aoc:each-line 'list #'parse-integer)
                     :expected-answer 1781)
