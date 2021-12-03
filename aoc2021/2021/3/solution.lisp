(ql:quickload "aoc")
(rename-package :alexandria :alexandria '(:a))

(defun most-common (bits tie-breaker)
  (loop with ones = (make-array (length (car bits)) :initial-element 0)
        for b in bits
        do (map-into ones #'+ ones b)
        finally (let* ((bits-len (length bits))
                       (zeros (map 'vector
                                   (a:curry #'- bits-len)
                                   ones)))
                  (return (map 'vector
                               (lambda (o z)
                                 (cond ((> o z)
                                        1)
                                       ((= o z)
                                        tie-breaker)
                                       (t
                                        0)))
                               ones
                               zeros)))))

(defun part1 (bits)
  (let* ((common (most-common bits 1))
         (gamma (map 'string #'digit-char common))
         (epsilon (map 'string (a:compose #'digit-char (a:rcurry #'mod 2) #'1+) common)))
    (* (parse-integer gamma :radix 2)
       (parse-integer epsilon :radix 2))))

(defun fobricate (bits f)
  (dotimes (i (length (car bits)))
    (when (null (cdr bits))
      (return (car bits)))
    (let ((common (map 'vector f (most-common bits 1))))
      (setf bits
            (delete-if-not (lambda (b) (= (aref common i)
                                          (aref b i)))
                           bits))))
  (car bits))

(defun part2 (bits)
  (let ((oxygen (fobricate (copy-list bits) #'identity))
        (koldioxid (fobricate (copy-list bits) (a:compose (a:rcurry #'mod 2) #'1+))))
    (* (parse-integer (map 'string #'digit-char oxygen) :radix 2)
       (parse-integer (map 'string #'digit-char koldioxid) :radix 2))))

(defparameter *parser* (aoc:each-line #'aoc:numbervector))
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 749376)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 2372923)
