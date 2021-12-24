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

(defclass cuboid ()
  ((up-left
    :initarg :up-left
    :accessor up-left)
   (down-right
    :initarg :down-right
    :accessor down-right)))

(defun x1 (c) (aref (up-left c) 0))
(defun y1 (c) (aref (up-left c) 1))
(defun z1 (c) (aref (up-left c) 2))
(defun x2 (c) (aref (down-right c) 0))
(defun y2 (c) (aref (down-right c) 1))
(defun z2 (c) (aref (down-right c) 2))

(defmethod print-object ((obj cuboid) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ul: ~a, dr: ~a" (up-left obj) (down-right obj))))

(defun cuboid (x1 y1 z1 x2 y2 z2)
  (make-instance 'cuboid
                 :up-left (vector x1 y1 z1)
                 :down-right (vector x2 y2 z2)))

(defun plist-cuboid (coords)
  (cuboid (getf coords :x1) (getf coords :y1) (getf coords :z1)
          (getf coords :x2) (getf coords :y2) (getf coords :z2)))

(defun validp (c)
  "non-nil if volume is positive, i.e. a valid cuboid"
  (every #'<= (up-left c) (down-right c)))

(defun volume (c)
  (iter (for ul in-vector (up-left c))
        (for dr in-vector (down-right c))
        (multiplying (1+ (- dr ul)))))

(defun intersectp (c1 c2)
  "Returns the intersected region of C1 and C2, or nil if there is none"
  (let* ((ul (iter (for i in-vector (up-left c1))
                   (for j in-vector (up-left c2))
                   (collect (max i j))))
         (dr (iter (for i in-vector (down-right c1))
                   (for j in-vector (down-right c2))
                   (collect (min i j))))
         (c (apply #'cuboid (nconc ul dr))))
    (when (validp c)
      c)))

(defun diff (cube sub-cube)
  "Returns a list of sub-cubes of CUBE that do not intersect with SUB-CUBE nor each other.
SUB-CUBE must be contained in CUBE."
  (iter outer (with offsets = '((0 -1) (0 0) (1 0)))
        (with xs = (list (x1 cube) (x1 sub-cube) (x2 sub-cube) (x2 cube)))
        (for xl in xs)
        (for xr in (cdr xs))
        (for (oxl oxr) in offsets)
        (for xc below 3)
        (iter (with ys = (list (y1 cube) (y1 sub-cube) (y2 sub-cube) (y2 cube)))
              (for yl in ys)
              (for yr in (cdr ys))
              (for (oyl oyr) in offsets)
              (for yc below 3)
              (iter (with zs = (list (z1 cube) (z1 sub-cube) (z2 sub-cube) (z2 cube)))
                    (for zl in zs)
                    (for zr in (cdr zs))
                    (for (ozl ozr) in offsets)
                    (for zc below 3)
                    (unless (= 1 xc yc zc)
                      (let ((c (cuboid (+ xl oxl) (+ yl oyl) (+ zl ozl)
                                       (+ xr oxr) (+ yr oyr) (+ zr ozr))))
                        (when (validp c)
                          (in outer (collect c)))))))))

(defun remove-overlap (on-cubes cub)
  (iter (for c in on-cubes)
        (if-let (i (intersectp c cub))
          (nconcing (diff c i))
          (collect c))))

(defun part2 (cuboids)
  (let (on-cubes)
    (iter (for clist in cuboids)
          (for cub next (plist-cuboid clist))
          (setq on-cubes (remove-overlap on-cubes cub))
          (when (getf clist :on)
            (push cub on-cubes)))
    (iter (for c in on-cubes)
          (summing (volume c)))))

(defparameter *parser*
  (aoc:each-line 'list
                 (aoc:boll (aoc:to-plist '(:on :x1 :x2 :y1 :y2 :z1 :z2))
                           (aoc:regex 'list ("[a-z]+" . (lambda (w) (string= "on" w)))
                                      " x=" :inumber "\.\." :inumber
                                      ",y=" :inumber "\.\." :inumber
                                      ",z=" :inumber "\.\." :inumber))))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 652209
                     :input-file "./input_part1")

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 1217808640648260)
