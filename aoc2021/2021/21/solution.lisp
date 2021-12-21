(ql:quickload "aoc")
(use-package :iterate)
(use-package :alexandria)
(use-package :arrows)

(defclass det-dice ()
  ((rolls
    :initform 0)
   (cur
    :initform 0)))

(defmethod roll ((d det-dice))
  (with-slots (rolls cur) d
    (incf rolls)
    (prog1 (1+ cur)
      (setf cur (mod (1+ cur)
                     100)))))

(defun roll3 (d)
  (iter (repeat 3)
        (summing (roll d))))

(defun part1 (players)
  (let ((d (make-instance 'det-dice))
        (p1score 0)
        (p2score 0)
        (p1pos (1- (first players)))
        (p2pos (1- (second players))))
    (->
     (iter (while t)
           (setf p1pos (mod (+ p1pos (roll3 d))
                            10))
           (incf p1score (1+ p1pos))
           (when (>= p1score 1000)
             (return p2score))
           (setf p2pos (mod (+ p2pos (roll3 d))
                            10))
           (incf p2score (1+ p2pos))
           (when (>= p2score 1000)
             (return p1score)))
     (* (slot-value d 'rolls)))))

(defun cons-add (c1 c2)
  (incf (car c1) (car c2))
  (incf (cdr c1) (cdr c2))
  c1)

(defparameter *all-splits* (iter outer (for i from 1 to 3)
                                 (iter (for j from 1 to 3)
                                       (iter (for k from 1 to 3)
                                             (in outer (collect (+ i j k)))))))

(defun dirac (p1pos p2pos p1score p2score p1turn)
  (if p1turn
      (if (>= p2score 21)
          (cons 0 1)
          (iter (for i in *all-splits*)
                (for newp1pos next (mod (+ i p1pos) 10))
                (reducing (dirac newp1pos
                                 p2pos
                                 (+ p1score (1+ newp1pos))
                                 p2score
                                 nil)
                          by #'cons-add
                          :initial-value (cons 0 0))))
      (if (>= p1score 21)
          (cons 1 0)
          (iter (for i in *all-splits*)
                (for newp2pos next (mod (+ i p2pos) 10))
                (reducing (dirac p1pos
                                 newp2pos
                                 p1score
                                 (+ p2score (1+ newp2pos))
                                 t)
                          by #'cons-add
                          :initial-value (cons 0 0))))))

(let ((m (make-hash-table :test 'equal))
      (org-dirac (symbol-function 'dirac)))
  (defun dirac (&rest args)
    (or (gethash args m nil)
        (setf (gethash args m)
              (apply org-dirac args)))))

(defun part2 (players)
  (destructuring-bind (a . b) (dirac (1- (first players))
                                     (1- (second players))
                                     0
                                     0
                                     t)
    (max a b)))

(defparameter *parser* (aoc:each-line 'list (aoc:regex :single "Player [0-9] starting position: " :number)))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 678468)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 131180774190079)
