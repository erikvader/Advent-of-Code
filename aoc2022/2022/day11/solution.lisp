(defun divide-by-three (worry monkeys)
  (declare (ignore monkeys))
  (floor worry 3))

(defun modulo-worry (worry monkeys)
  ;;TODO: don't re-calculate the lcm each time
  (->> monkeys
       (mapcar (lambda (m) (cdr (assoc :test-divider m))))
       (reduce #'lcm)
       (mod worry)))

(defun monkey-round (monkeys stress-handler)
  (iter (for m in monkeys)
        (iter (for item in (nreverse (cdr (assoc :items m))))
              (let* ((worry (-<> (cdr (assoc :operation m))
                                (funcall <> item)
                                (funcall stress-handler <> monkeys)))
                     (throw-to (if (-> (cdr (assoc :test m))
                                       (funcall worry))
                                   (cdr (assoc :true m))
                                   (cdr (assoc :false m)))))
                (incf (cdr (assoc :activity m)))
                (push worry (cdr (assoc :items (nth throw-to monkeys))))))
        (setf (cdr (assoc :items m)) nil)))

(defun run-monkeys (monkeys rounds stress-handler)
  (setq monkeys
        (mapcar (lambda (m)
                  (acons :activity 0 m))
                monkeys))
  (iter (for m in monkeys)
        (setf (cdr (assoc :items m))
              (nreverse (cdr (assoc :items m)))))
  (iter (repeat rounds)
        (monkey-round monkeys stress-handler))
  (setq monkeys
        (sort monkeys #'> :key (lambda (m) (cdr (assoc :activity m)))))
  (->>
   (subseq monkeys 0 2)
   (mapcar (lambda (m) (cdr (assoc :activity m))))
   (reduce #'*)))

(defun part1 (monkeys)
  (run-monkeys monkeys 20 #'divide-by-three))

(defun part2 (monkeys)
  (run-monkeys monkeys 10000 #'modulo-worry))

(defparameter *parser* (aoc:lisp-read))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 107822)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 27267163742)
