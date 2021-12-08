(ql:quickload "aoc")
(import 'alexandria:curry)
(import 'arrows:->>)
(import 'arrows:->)
(import 'arrows:-<>>)
(import 'trivia:ematch)

(defun part1 (input)
  (loop for i in input
        sum (loop for o in (cdr i)
                  for len = (length o)
                  count (or (= len 2)
                            (= len 4)
                            (= len 3)
                            (= len 7)))))

(defun cind (char)
  (- (char-code char)
     (char-code #\a)))

(defun indc (num)
  (code-char (+ num (char-code #\a))))

(defun find-translation (data)
  (let* ((counts (loop with cs = (make-array 7 :initial-element 0)
                       for str in data
                       do (loop for c across str
                                do (incf (aref cs (cind c))))
                       finally (return cs)))
         (f (position 9 counts))
         (c (->> data
                 (find-if (lambda (x)
                            (= 2 (length x))))
                 (find-if-not (curry #'eql (indc f)))
                 (cind)))
         (a (-<>> data
                  (find-if (lambda (x)
                             (= 3 (length x))))
                  (coerce <> 'list)
                  (nset-difference <> (list (indc f) (indc c)))
                  (car)
                  (cind)))
         (b (position 6 counts))
         (e (position 4 counts))
         (d (-<>> data
                  (find-if (lambda (x)
                             (= 4 (length x))))
                  (coerce <> 'list)
                  (nset-difference <> (mapcar #'indc (list b c f)))
                  (car)
                  (cind)))
         (g (->> (list a b c d e f)
                 (mapcar #'indc)
                 (nset-difference (coerce "abcdefg" 'list))
                 (car)
                 (cind))))
    (vector a b c d e f g)))

(defun decode (trans str)
  (let ((xxx (map 'string
                  (lambda (c)
                    (-<>> c
                         (cind)
                         (position <> trans)
                         (indc)))
                  (coerce str 'list))))
    (ematch (sort xxx #'char<)
      ("abcefg" 0)
      ("cf" 1)
      ("acdeg" 2)
      ("acdfg" 3)
      ("bcdf" 4)
      ("abdfg" 5)
      ("abdefg" 6)
      ("acf" 7)
      ("abcdefg" 8)
      ("abcdfg" 9))))

(defun decode-display (trans str1 str2 str3 str4)
  (+ (decode trans str4)
     (* 10 (decode trans str3))
     (* 100 (decode trans str2))
     (* 1000 (decode trans str1))))

(defun part2 (input)
  (loop for i in input
        for trans = (find-translation (car i))
        sum (apply #'decode-display trans (cdr i))))

(defparameter *parser* (aoc:each-line (aoc:boll
                                       (aoc:map-cons #'aoc:words #'aoc:words)
                                       (aoc:split-sep " | "))))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 412)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 978171)
