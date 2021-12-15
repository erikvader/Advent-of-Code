(ql:quickload "aoc")
(use-package :iterate)
(import 'alexandria:if-let)

(defstruct repeating-grid src repeat)

(defmethod grid-get ((g array) x y)
  (aref g x y))

(defmethod in-bounds-p ((g array) x y)
  (array-in-bounds-p g x y))

(defmethod grid-get ((g repeating-grid) x y)
  (let* ((arr (repeating-grid-src g))
         (quadx (floor x (array-dimension arr 0)))
         (quady (floor y (array-dimension arr 1)))
         (modx (mod x (array-dimension arr 0)))
         (mody (mod y (array-dimension arr 1))))
    (1+ (mod (1- (+ (aref arr modx mody)
                    quadx
                    quady))
             9))))

(defmethod in-bounds-p ((g repeating-grid) x y)
  (let ((rep (repeating-grid-repeat g))
        (arr (repeating-grid-src g)))
    (and (>= x 0)
         (>= y 0)
         (< x (* rep (array-dimension arr 0)))
         (< y (* rep (array-dimension arr 1))))))

(defun min-to-front (ls &key (key #'identity))
  (let ((i (iter (for l on ls)
                 (finding l minimizing (funcall key (car l))))))
    (rotatef (car ls) (car i))
    ls))

(defun shortest-path (grid start goal)
  (iter (with visited = (make-hash-table :test 'equal))
        (with queue = (list (cons start 0)))
        (for q next (if (null queue)
                        (terminate)
                        (progn
                          (min-to-front queue :key #'cdr)
                          (pop queue))))
        (when (gethash (car q) visited)
          (next-iteration))
        (when (equal (car q) goal)
          (return (cdr q)))
        (setf (gethash (car q) visited) t)
        (iter (for (dx dy) in '((-1 0) (1 0)
                                (0 -1) (0 1)))
              (for xy next (cons (+ (caar q) dx)
                                 (+ (cdar q) dy)))
              (when (or (not (in-bounds-p grid (car xy) (cdr xy)))
                        (gethash xy visited))
                (next-iteration))
              (if-let ((old (member xy queue :key #'car :test #'equal))
                       (new-cost (+ (cdr q)
                                    (grid-get grid (car xy) (cdr xy)))))
                (when (< new-cost (cdar old))
                  (setf (cdar old) new-cost))
                (push (cons xy new-cost) queue)))))

(defun part1 (grid)
  (shortest-path grid
                 (cons 0 0)
                 (cons (1- (array-dimension grid 0))
                       (1- (array-dimension grid 1)))))

(defun part2 (grid)
  (shortest-path (make-repeating-grid :src grid :repeat 5)
                 (cons 0 0)
                 (cons (1- (* 5 (array-dimension grid 0)))
                       (1- (* 5 (array-dimension grid 1))))))

;NOTE: The grid needs to be transposed to match the rest of the code, but it is fine since
;it is square and the start and end are in opposite corners.
(defparameter *parser* (aoc:integer-grid-tight))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 462)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 2846)
