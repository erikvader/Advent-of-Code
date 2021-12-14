(ql:quickload "aoc")
(use-package :iterate)
(import 'alexandria:if-let)

(defun count-pairs (seq)
  (iter (with cs = (make-hash-table :test 'equal))
        (for b in (cdr seq))
        (for a previous b initially (car seq))
        (incf (gethash (cons a b) cs 0))
        (finally (return cs))))

(defun poly (pairs rules)
  (iter (with cs = (make-hash-table :test (hash-table-test pairs)
                                    :size (hash-table-size pairs)))
        (for (k v) in-hashtable pairs)
        (if-let (x (gethash k rules))
          (progn (incf (gethash (cons (car k) x) cs 0)
                       v)
                 (incf (gethash (cons x (cdr k)) cs 0)
                       v))
          (incf (gethash k cs 0) v))
        (finally (return cs))))

(defun counts (pairs last-char)
  (iter (with cs = (make-hash-table))
        (for ((k1 . nil) v) in-hashtable pairs)
        (incf (gethash k1 cs 0) v)
        (finally (incf (gethash last-char cs 0))
                 (return cs))))

(defun solve (init rules last-char n)
  (iter (repeat n)
        (setf init (poly init rules)))
  (let ((cs (counts init last-char)))
    (- (iter (for (nil v) in-hashtable cs)
             (maximize v))
       (iter (for (nil v) in-hashtable cs)
             (minimize v)))))

(defun part1 (init rules)
  (solve (count-pairs init)
         rules
         (car (last init))
         10))

(defun part2 (init rules)
  (solve (count-pairs init)
         rules
         (car (last init))
         40))

(defparameter *parser* (aoc:map-cons (aoc:chars 'list)
                                     (aoc:boll (aoc:collect-hash-table)
                                               (aoc:each-line 'list (aoc:regex :pair :dchar " -> " :char))
                                               (aoc:trim))))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 3118
                     :unpack :cons)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 4332887448171
                     :unpack :cons)
