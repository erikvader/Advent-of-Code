(ql:quickload "aoc")

(defun part1 (lines)
  lines)

(defun part2 (lines)
  "hej2")

(aoc:run-day #'part1)

(funcall
 ;; (aoc::regex-parse ".*([0-9]+).*" #'parse-integer)
 ;; (aoc::regex ".*" ("[0-9]+" . #'parse-integer) ".*")
 (aoc::split-at 2)
 "123asdasd")

(aref
 (aoc::char-grid '("123" "456" "789"))
 0 1)
