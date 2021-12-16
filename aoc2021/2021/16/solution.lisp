(ql:quickload "aoc")
(use-package :iterate)
(import 'arrows:->>)
(import 'arrows:->*)
(import 'arrows:->)

(defun hex-to-bin (hexes)
  (iter (for h in-string hexes)
        (nconcing (->> (digit-char-p h 16)
                       (format nil "~4,'0b")
                       (->* (coerce 'list))))))

(defmacro popn (ls n)
  `(iter (for i below ,n)
         (while ls)
         (collect (pop ,ls))))

(defun popper (ls)
  (lambda (n)
    (when ls
      (popn ls n))))

(defun pop-num (p n)
  (alexandria:when-let (raw (funcall p n))
    (bin-to-num raw)))

(defun bin-to-num (bins)
  (-> bins
      (coerce 'string)
      (parse-integer :radix 2)))

(defun pop-raw (p n)
  (funcall p n))

(defun parse-packet (bits mapper)
  (let* ((version (pop-num bits 3))
         (type (pop-num bits 3)))
    (when version
      (funcall mapper
               version
               type
               (cond ((= type 4)
                      (parse-num bits))
                     (t
                      (parse-subpackets bits mapper)))))))

(defun parse-num (bits)
  (-> (iter (for group next (pop-raw bits 5))
            (nconcing (cdr group))
            (while (char= #\1 (car group))))
      (bin-to-num)))

(defun parse-subpackets (bits mapper)
  (let ((length-type-id (pop-num bits 1)))
    (if (= length-type-id 0)
        (parse-subpacket-0 bits mapper)
        (parse-subpacket-1 bits mapper))))

(defun parse-subpacket-0 (bits mapper)
  (let* ((bit-len (pop-num bits 15))
         (load (pop-raw bits bit-len))
         (p (popper load)))
    (iter (for x next (parse-packet p mapper))
          (while x)
          (collect x))))

(defun parse-subpacket-1 (bits mapper)
  (let ((num-pack (pop-num bits 11)))
    (iter (for i below num-pack)
          (collect (parse-packet bits mapper)))))

(defun part1 (hexes)
  (let* ((p (popper (hex-to-bin hexes))))
    (parse-packet p (lambda (version type sub)
                      (+ version (if (/= type 4)
                                     (reduce #'+ sub)
                                     0))))))

(defun lt (a b)
  (if (< a b) 1 0))
(defun gt (a b)
  (if (> a b) 1 0))
(defun equ (a b)
  (if (= a b) 1 0))

(defun part2 (hexes)
  (let* ((p (popper (hex-to-bin hexes)))
         (expr (parse-packet p (lambda (version type sub)
                                 (declare (ignore version))
                                 (if (= type 4)
                                     sub
                                     (cons (case type
                                             (0 '+)
                                             (1 '*)
                                             (2 'min)
                                             (3 'max)
                                             (5 'gt)
                                             (6 'lt)
                                             (7 'equ))
                                           sub))))))
    (eval expr)))

(defparameter *parser* #'car)
(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 1007)

(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 834151779165)
