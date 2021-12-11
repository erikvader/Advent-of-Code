(ql:quickload "aoc" :silent t)
(import 'arrows:->>)
(import 'arrows:->)

(defmacro each-neighbour (&body body)
  `(loop for (a b) in '((-1 0) (1 0) (0 -1) (0 1))
         for xx = (+ x a)
         for yy = (+ y b)
         ,@body))

(defun is-low-p (grid x y)
  (each-neighbour
    with mid = (aref grid x y)
    always (or (not (array-in-bounds-p grid xx yy))
               (< mid (aref grid xx yy)))))

(defun part1 (grid)
  (loop for x below (array-dimension grid 0)
        sum (loop for y below (array-dimension grid 1)
                  when (is-low-p grid x y)
                    sum (1+ (aref grid x y)))))

(defun basin-size (grid low-x low-y)
  (let ((visited nil)
        (size 0))
    (labels ((walk (x y)
               (unless (member (cons x y) visited :test #'equal)
                 (pushnew (cons x y) visited :test #'equal)
                 (incf size)
                 (each-neighbour
                   when (and (array-in-bounds-p grid xx yy)
                             (not (= 9 (aref grid xx yy))))
                   do (walk xx yy)))))
      (walk low-x low-y))
    size))

(defun part2 (grid)
  (let ((basins (loop for x below (array-dimension grid 0)
                      nconc (loop for y below (array-dimension grid 1)
                                  when (is-low-p grid x y)
                                    collect (basin-size grid x y)))))
    (-> (sort basins #'>)
        (subseq 0 3)
        (->> (reduce #'*)))))

(defparameter *parser* #'aoc:integer-grid-tight)
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 603)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 786780)
