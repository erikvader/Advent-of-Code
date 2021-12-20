(ql:quickload "aoc")
(use-package :iterate)
(use-package :arrows)

(defun staket (c)
  (if (char= c #\.)
      #\0
      #\1))

(defun convolve (grid iea bg r c &key bounds-check)
  (iter (with num = (make-string 9 :initial-element (staket bg)))
        (for dr from -1 to 1)
        (iter (for dc from -1 to 1)
              (let ((nr (+ r dr))
                    (nc (+ c dc)))
                (when (or (not bounds-check)
                          (array-in-bounds-p grid nr nc))
                  (setf (aref num (+ (* 3 (1+ dr))
                                     (1+ dc)))
                        (staket (aref grid nr nc))))))
        (finally (return (aref iea (parse-integer num :radix 2))))))

(defun enhance (grid iea bg)
  (let ((new (make-array (mapcar (lambda (x) (+ x 2))
                                 (array-dimensions grid)))))
    (iter (for r from 0 below (array-dimension new 0))
          (iter (for c from 0 below (array-dimension new 1))
                (setf (aref new r c)
                      (convolve grid iea bg (1- r) (1- c)
                                :bounds-check (or (<= r 1)
                                                  (<= c 1)
                                                  (>= r (- (array-dimension new 0) 2))
                                                  (>= c (- (array-dimension new 1) 2)))))))
    new))

(defun count-lit (grid)
  (iter (for c in-vector (make-array (array-total-size grid) :displaced-to grid))
        (count (char= c #\#))))

(defun part1 (iea grid)
  (-> (enhance grid iea #\.)
      (enhance iea #\#)
      (count-lit)))

(defun part2 (iea grid)
  (iter (repeat 50)
        (with bg = #\.)
        (setq grid (enhance grid iea bg))
        (setq bg (if (char= bg #\.)
                     #\#
                     #\.)))
  (count-lit grid))

(defparameter *parser* (aoc:boll (aoc:header (aoc:boll (aoc:chars 'vector) #'car)
                                             (aoc:char-grid))
                                 (aoc:paragraphs)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 5291
                     :unpack :list)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 16665
                     :unpack :list)
