(defun move-head (head dir)
  (ecase dir
    (#\R (aoc:map-cons #'1+ #'identity head))
    (#\L (aoc:map-cons #'1- #'identity head))
    (#\U (aoc:map-cons #'identity #'1+ head))
    (#\D (aoc:map-cons #'identity #'1- head))))

(defun adjacentp (head tail)
  (iter outer
        (for i from -1 to 1)
        (iter (for j from -1 to 1)
              (in outer (thereis (equalp head
                                         (cons (+ i (car tail))
                                               (+ j (cdr tail)))))))))

(defun get-delta (from to)
  (clamp (- to from) -1 1))

(defun follow-head (head tail)
  (if (adjacentp head tail)
      tail
      (cons (+ (car tail) (get-delta (car tail) (car head)))
            (+ (cdr tail) (get-delta (cdr tail) (cdr head))))))

(defun part1 (moves)
  (let ((head (cons 0 0))
        (tail (cons 0 0))
        (tail-poses (make-hash-table :test #'equalp)))
    (iter (for (dir . amount) in moves)
          (iter (repeat amount)
                (setq head (move-head head dir))
                (setq tail (follow-head head tail))
                (setf (gethash tail tail-poses) t)))
    (iter (for (k v) in-hashtable tail-poses)
          (count t))))

(defun part2 (moves)
  (let ((head (cons 0 0))
        (tails (iter (repeat 9)
                     (collect (cons 0 0))))
        (tail-poses (make-hash-table :test #'equalp)))
    (iter (for (dir . amount) in moves)
          (iter (repeat amount)
                (setq head (move-head head dir))
                (setq tails
                      (iter (with temphead = head)
                            (for b in tails)
                            (collect (setq temphead (follow-head temphead b)))))
                (setf (gethash (last-elt tails) tail-poses) t)))
    (iter (for (k v) in-hashtable tail-poses)
          (count t))))

(defparameter *parser* (aoc:each-line 'list
                                      (aoc:boll
                                       (aoc:map-cons (aoc:to-char) #'parse-integer)
                                       (aoc:split-at 1))))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 6494)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 2691)
