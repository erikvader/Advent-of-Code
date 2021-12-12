(ql:quickload "aoc")
(use-package :iterate)

(defun small-cave-p (cave)
  (lower-case-p (aref cave 0)))

(defun end-p (cave)
  (string= "end" cave))

(defun start-p (cave)
  (string= "start" cave))

(defun to-graph (conn)
  (iter (with g = (make-hash-table :test 'equal))
        (for (a . b) in conn)
        (push a (gethash b g))
        (push b (gethash a g))
        (finally (return g))))

(defun dfs (cur small-visited graph small-twice-p)
  (cond
    ((end-p cur) 1)
    ((and (not small-twice-p)
          (not (start-p cur))
          (small-cave-p cur)
          (gethash cur small-visited))
     (iter (for neigh in (gethash cur graph))
           (sum (dfs neigh small-visited graph t))))
    ((or (not (small-cave-p cur))
         (not (shiftf (gethash cur small-visited) t)))
     (iter (for neigh in (gethash cur graph))
           (sum (dfs neigh small-visited graph small-twice-p))
           (finally (remhash cur small-visited))))
    (t 0)))

(defun part1 (conn)
  (dfs "start"
       (make-hash-table :test 'equal)
       (to-graph conn)
       t))

(defun part2 (conn)
  (dfs "start"
       (make-hash-table :test 'equal)
       (to-graph conn)
       nil))

(defparameter *parser* (aoc:each-line 'list (aoc:split-sep "-")))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 3298)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 93572)
