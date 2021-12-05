(ql:quickload "aoc")

(defparameter x1 0)
(defparameter y1 1)
(defparameter x2 2)
(defparameter y2 3)

(defun diag-line-p (line)
  (and (not (= (aref line x1)
               (aref line x2)))
       (not (= (aref line y1)
               (aref line y2)))))

(defun dir (src dst)
  (cond ((> dst src)
         1)
        ((< dst src)
         -1)
        (t
         0)))

(defun fill-grid (grid line)
  (let ((xdir (dir (aref line x1) (aref line x2)))
        (ydir (dir (aref line y1) (aref line y2)))
        (cur (cons (aref line x1) (aref line y1)))
        (target (cons (aref line x2) (aref line y2))))
    (loop do (incf (gethash cur grid 0))
          while (not (equal cur target))
          do (setf cur (cons (+ xdir (car cur))
                             (+ ydir (cdr cur)))))))

(defun overlapping-points (grid)
  (loop for v being the hash-values of grid
        when (> v 1)
          count v))

(defun part1 (lines)
  (loop with grid = (make-hash-table :test #'equal)
        for line in lines
        when (not (diag-line-p line))
          do (fill-grid grid line)
        finally (return (overlapping-points grid))))

(defun part2 (lines)
  (loop with grid = (make-hash-table :test #'equal)
        for line in lines
        do (fill-grid grid line)
        finally (return (overlapping-points grid))))

(defparameter *parser* (aoc::each-line (aoc:regex '(vector * 4) number "," number " -> " number "," number)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 6007)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 19349)
