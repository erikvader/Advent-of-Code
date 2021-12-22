(ql:quickload "aoc")
(use-package :iterate)
(use-package :alexandria)
(use-package :arrows)

(defun part1 (cuboids)
  (iter (with on = (make-hash-table :test 'equalp))
        (for cube in cuboids)
        (iter (for x from (getf cube :x1) to (getf cube :x2))
              (iter (for y from (getf cube :y1) to (getf cube :y2))
                    (iter (for z from (getf cube :z1) to (getf cube :z2))
                          (setf (gethash (vector x y z) on)
                                (getf cube :on)))))
        (finally (return (iter (for (nil v) in-hashtable on)
                               (count v))))))

(defun intersect (c1 c2)
  "Returns the intersected region of C1 and C2, or nil if there is none"
  )

(defun explode (cube exploder)
  "Returns a list of sub-cubes of EXPLODER that do not intersect with CUBE nor each
  other.")

(defun part2 (players)
  ;; ha en lista på icke-intersectade kuber som är on
  )

(defparameter *parser* (aoc:each-line 'list (aoc:boll (aoc:to-plist '(:on :x1 :x2 :y1 :y2 :z1 :z2))
                                                      (aoc:regex 'list ("[a-z]+" . (lambda (w) (string= "on" w)))
                                                                 " x=" :inumber "\.\." :inumber
                                                                 ",y=" :inumber "\.\." :inumber
                                                                 ",z=" :inumber "\.\." :inumber))))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 652209
                     :input-file "./input_part1")

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 131180774190079)
