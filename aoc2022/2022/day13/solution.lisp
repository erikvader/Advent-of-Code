(defun monoid (a b)
  (if (eq a 'dunno)
      b
      a))

(defun in-order-p (left right)
  (cond ((and (integerp left)
              (integerp right))
         (cond ((< left right)
                'in-order)
               ((> left right)
                'out-of-order)
               (t 'dunno)))
        ((and (listp left)
              (listp right))
         (cond ((and (null left) (null right))
                'dunno)
               ((null left)
                'in-order)
               ((null right)
                'out-of-order)
               (t (monoid (in-order-p (car left) (car right))
                          (in-order-p (cdr left) (cdr right))))))
        ((integerp left)
         (in-order-p (list left) right))
        ((integerp right)
         (in-order-p left (list right)))))

(defun part1 (signals)
  (iter (for (left right) in signals)
        (for i from 1)
        (when (eq 'in-order (in-order-p left right))
          (sum i))))

(defun part2 (signals)
  (let* ((div1 '((2)))
         (div2 '((6)))
         (sorted (->
                  (apply #'nconc (list div1 div2) signals)
                  (sort (lambda (l r) (eq 'in-order (in-order-p l r)))))))
    (*
     (1+ (position div1 sorted :test #'equalp))
     (1+ (position div2 sorted :test #'equalp)))))

(defparameter *parser* (aoc:boll
                        (aoc:each-line 'list
                                       (aoc:each-line 'list
                                                      (aoc:parse-lists "[" "]")))
                        (aoc:paragraphs)))

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 6272)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 22288)
