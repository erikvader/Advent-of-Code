(defun stones (trails)
  (let ((rocks (make-hash-table :test #'equalp)))
    (iter (for trail in trails)
          (iter (for to in (cdr trail))
                (for from previous to initially (car trail))
                (if (= (car from) (car to))
                    (iter (for i from (min (cdr from) (cdr to)) to (max (cdr from) (cdr to)))
                          (setf (gethash (cons (car from) i) rocks) t))
                    (iter (for i from (min (car from) (car to)) to (max (car from) (car to)))
                          (setf (gethash (cons i (cdr from)) rocks) t)))))
    rocks))

(defun max-height (trails)
  (iter outer (for trail in trails)
        (iter (for pos in trail)
              (in outer (maximize (cdr pos))))))

(defun sand-fall (sand boulders)
  (let ((down (aoc:map-cons #'identity #'1+ sand))
        (down-left (aoc:map-cons #'1- #'1+ sand))
        (down-right (aoc:map-cons #'1+ #'1+ sand)))
    (cond ((null (gethash down boulders))
           down)
          ((null (gethash down-left boulders))
           down-left)
          ((null (gethash down-right boulders))
           down-right)
          (t
           sand))))

(defun sand-floor (sand floor)
  (if (= floor (cdr sand))
      (cons (car sand) (1- floor))
      sand))

(defun simulate-sand (pebbles max-height)
  (iter outer (while t)
        (iter (with sand = (cons 500 0))
              (for new-sand = (sand-fall sand pebbles))
              (when (equalp new-sand sand)
                (setf (gethash sand pebbles) t)
                (in outer (counting t))
                (finish))
              (setq sand new-sand)
              (when (> (cdr sand) max-height)
                (in outer (finish))))))

(defun simulate-sand-floor (pebbles floor)
  (iter outer (while t)
        (iter (with sand = (cons 500 0))
              (for new-sand = (-> (sand-fall sand pebbles)
                                  (sand-floor floor)))
              (when (equalp new-sand '(500 . 0))
                (in outer (counting t))
                (in outer (finish)))
              (when (equalp new-sand sand)
                (setf (gethash sand pebbles) t)
                (in outer (counting t))
                (finish))
              (setq sand new-sand))))

(defun part1 (trails)
  (simulate-sand (stones trails) (max-height trails)))

(defun part2 (trails)
  (simulate-sand-floor (stones trails) (+ 2 (max-height trails))))

(defparameter *parser* (aoc:each-line 'list
                                      (aoc:boll
                                       (aoc:each-line 'list
                                                      (aoc:boll
                                                       (aoc:map-cons #'parse-integer nil)
                                                       (aoc:split-sep ",")))
                                       (aoc:split-sep-list " -> "))))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 799)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 29076)
