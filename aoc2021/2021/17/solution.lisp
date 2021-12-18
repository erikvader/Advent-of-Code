(ql:quickload "aoc")
(use-package :iterate)

(defun inside-square-p (square point)
  (not (or (> (car point) (getf square :x2))
           (< (car point) (getf square :x1))
           (> (cdr point) (getf square :y2))
           (< (cdr point) (getf square :y1)))))

(defun outside-p (square point)
  (or (> (car point) (getf square :x2))
      (< (cdr point) (getf square :y1))))

(defun hits-square-p (square velx vely)
  (iter (with x = 0)
        (with y = 0)
        (when (inside-square-p square (cons x y))
          (return t))
        (while (not (outside-p square (cons x y))))
        (incf x velx)
        (incf y vely)
        (setq velx (max 0 (1- velx)))
        (decf vely)))

(defun get-v0 (dt)
  (+ (- (/ 1 2))
     (sqrt (+ (/ 1 4)
              (* 2 dt)))))

(defun part1 (square)
  (iter (with velx = (floor (get-v0 (getf square :x2))))
        (for vely from 1 to 300)
        (when (hits-square-p square velx vely)
          (maximize (floor (* vely (1+ vely)) 2)))))

(defun part2 (square)
  (iter outer (for velx from 20 to 500)
        (iter (for vely from -100 to 400)
              (in outer (count (hits-square-p square velx vely))))))

(defparameter *parser* (aoc:boll (aoc:to-plist '(:x1 :x2 :y1 :y2))
                                 (aoc:regex 'list "target area: x=" :inumber "\.\." :inumber ", y=" :inumber "\.\." :inumber)
                                 #'car))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 4186)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 2709)
