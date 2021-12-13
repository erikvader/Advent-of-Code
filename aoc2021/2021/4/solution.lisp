(ql:quickload "aoc" :silent t)
(sb-ext:add-package-local-nickname :a :alexandria)

(defstruct bingo
  inverse-grid
  column-counts
  row-counts)

(defun new-bingo (int-array)
  (destructuring-bind (height width) (array-dimensions int-array)
      (make-bingo :inverse-grid (loop with res = (make-hash-table)
                                      for h below height
                                      do (loop for w below width
                                               do (setf (gethash (aref int-array h w) res)
                                                        (cons h w)))
                                      finally (return res))
                  :column-counts (make-array width :initial-element 0)
                  :row-counts (make-array height :initial-element 0))))

(defun is-victor-p (bing)
  (let ((h (array-dimension (bingo-row-counts bing) 0))
        (w (array-dimension (bingo-column-counts bing) 0)))
    (and
     (or (some (a:curry #'= w) (bingo-row-counts bing))
         (some (a:curry #'= h) (bingo-column-counts bing)))
     bing)))

(defun mark-bingo (num bing)
  (a:when-let ((pos (gethash num (bingo-inverse-grid bing))))
    (incf (aref (bingo-column-counts bing) (cdr pos)))
    (incf (aref (bingo-row-counts bing) (car pos)))
    (remhash num (bingo-inverse-grid bing))))

(defun sum-unmarked (bing)
  (loop for v being the hash-keys of (bingo-inverse-grid bing)
        sum v))

(defun part1 (input)
  (let ((calls (car input))
        (boards (mapcar #'new-bingo (cdr input))))
    (loop for c in calls
          for _ = (mapc (a:curry #'mark-bingo c)
                        boards)
          for victor = (some #'is-victor-p boards)
          when victor
            return (* c (sum-unmarked victor)))))

(defun part2 (input)
  (let ((calls (car input))
        (boards (mapcar #'new-bingo (cdr input))))
    (loop for c in calls
          for cs on calls
          do (mapc (a:curry #'mark-bingo c)
                   boards)
          do (setf boards (delete-if #'is-victor-p boards))
          when (null (cdr boards))
            return (loop with b = (car boards)
                         for c in cs
                         do (mark-bingo c b)
                         until (is-victor-p b)
                         finally (return (* c (sum-unmarked b)))))))

(defparameter *parser* (aoc:map-cons (aoc:boll (aoc:each-line 'list #'parse-integer)
                                               #'aoc:commas)
                                     (aoc:boll (aoc:each-line 'list #'aoc:integer-grid)
                                               #'aoc:paragraphs)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 34506)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 7686)
