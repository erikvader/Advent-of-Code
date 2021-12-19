(ql:quickload "aoc")
(use-package :iterate)
(use-package :alexandria)
(use-package :arrows)

(defun vec3 (f v1 v2)
  (map 'vector f v1 v2))

(defun roll (v)
  (vector (aref v 0)
          (- (aref v 2))
          (aref v 1)))

(defun rotatecw (v)
  (vector (aref v 2)
          (aref v 1)
          (- (aref v 0))))

;TODO: modify in-place?
(defun rotateccw (v)
  (vector (- (aref v 2))
          (aref v 1)
          (aref v 0)))

(defun merge-scanners-rot (scan0 scan1)
  ;; https://stackoverflow.com/a/58471362 rotate scan1 24 times and run `merge-scanners'
  ;; on each, returning on the first one that succeeds
  )

(defun merge-scanners (scan0 scan1)
  (iter outer (with s0points = (let ((ht (make-hash-table :size (length scan0) :test 'equal)))
                                 (iter (for s0 in scan0)
                                       (setf (gethash s0 ht) t))
                                 ht))
        ;; (print (hash-table-alist s0points))
        (for s0 in scan0)
        (iter (for s1 in scan1)
              ;; (format t "trying ~s and ~s~%" s0 s1)
              (let* ((s1pos (vec3 #'- s0 s1))
                     (pairs (iter (for ss1 in scan1)
                                  (for x next (vec3 #'+ s1pos ss1))
                                  (if (gethash x s0points nil)
                                      (count t into common)
                                      ;TODO: figure out what to append after 12 pairs has
                                      ;been found, and not all the time?
                                      (collect x into others))
                                  (finally (return (cons common others))))))
                ;; (format t "s1 at ~s~%" s1pos)
                ;; (format t "they got ~s points in common ~%~%" (car pairs))
                (when (>= (car pairs) 12)
                  ;TODO: input is nice enough to always give us exactly 12?
                  (in outer (finding (cdr pairs) maximizing (car pairs) into beacons)))))
        (finally (return-from outer (and beacons
                                         (nconc beacons scan0))))))

(defun part1 (scanners)
  (merge-scanners (cdr (first scanners))
                  (cdr (second scanners))))

(defun part2 (numbers)
  )

(defparameter *parser* (aoc:boll (aoc:each-line 'list
                                                (aoc:header (aoc:regex :single "--- scanner " :number " ---")
                                                            (aoc:commas-numbers 'vector)))
                                 (aoc:paragraphs)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 3793)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 4695)
