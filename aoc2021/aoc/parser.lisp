(in-package :aoc)

(defun each-line (f &key (type 'list))
  "Returns a function that parses each line with F. The desired return type, e.g., list or
vector, is specified with TYPE."
  (lambda (lines)
    (map type f lines)))

(defun words (line)
  (delete-if #'uiop:emptyp
             (uiop:split-string line :separator " ")))

(defmacro regex (return-type &rest string-or-cons)
  "Cool macro"
  (let* ((expanded (->> string-or-cons
                        (mapcar (lambda (x)
                                  (case x
                                    (number '("[0-9]+" . #'parse-integer))
                                    (t x))))))
         (regex-string (->> expanded
                            (mapcan (lambda (sc)
                                      (if (consp sc)
                                          (list "(" (car sc) ")")
                                          (list sc))))
                            (apply #'concatenate 'string)))
         (parsers (->> expanded
                       (remove-if-not #'consp)
                       (mapcar #'cdr)
                       (mapcar (lambda (s) (if (symbolp s)
                                               `(function ,s)
                                               s))))))
    `(regex-parse ,regex-string (list ,@parsers) :return-type ,return-type)))

(defun regex-parse (regex parsers &key (return-type 'vector))
  "Run regex on line and return all capture groups parsed by their respective function
from PARSERS. A value of nil in PARSERS is the same as #'identity. The length of PARSERS
must be the same as the number of capture groups in REGEX."
  (let ((ptrn (ppcre:create-scanner regex)))
    (lambda (line)
      (when-let (groups (nth-value 1 (ppcre:scan-to-strings ptrn line)))
        (map return-type
             (lambda (g p)
               (funcall (or p #'identity) g))
             groups
             parsers)))))

(defun branch (&rest parsers)
  "Runs each parser in sequence and returns the result and the index of the first
  one that succeeds, i.e. returns non-nil and doesn't signal."
  (lambda (line)
    (loop for p in parsers
          for i from 0
          for x = (ignore-errors (funcall p line))
          when x
            return (values x i))))

(defun char-grid (lines)
  "Parses all lines into a matrix of characters"
  (let* ((height (length lines))
         (width (length (car lines)))
         (array (make-array (list height width))))
    (loop for l in lines
          for h from 0
          do (loop for c across l
                   for w from 0
                   do (setf (aref array h w) c)))
    array))

(defun integer-grid (lines)
  "Parses a grid of integers into a matrix. Each column is separated by one or more
  spaces and each row is on a separate line."
  (let* ((height (length lines))
         (width (-> (car lines)
                    (words)
                    (length)))
         (array (make-array (list height width))))
    (loop for l in lines
          for h from 0
          do (loop for word in (words l)
                   for int = (parse-integer word)
                   for w from 0
                   do (setf (aref array h w) int)))
    array))

(defun integer-grid-tight (lines)
  "Parses a grid of integers into a matrix. Each column is one character wide and each row
is on a separate line."
  (let* ((height (length lines))
         (width (length (car lines)))
         (array (make-array (list height width))))
    (loop for l in lines
          for h from 0
          do (loop for c across l
                   for int = (digit-char-p c)
                   for w from 0
                   do (setf (aref array h w) int)))
    array))

(defun group-by-reverse (to-group split-here-p)
  "Finds non-empty groups delimited by items where SPLIT-HERE-P is non-nil."
  (labels ((f (elements groups group)
             (match* (elements groups group)
               ((nil gs nil)
                gs)
               ((nil gs g)
                (f nil (cons g gs) nil))
               (((cons x xs) gs g)
                (if (funcall split-here-p x)
                    (f xs (if (null g) gs (cons g gs)) nil)
                    (f xs gs (cons x g)))))))
    (f to-group nil nil)))

(defun group-by (to-group split-here-p)
  (mapcar #'reverse (reverse (group-by-reverse to-group split-here-p))))

(defun paragraphs (lines)
  "Groups lines into paragraphs"
  (group-by lines (lambda (s) (string= s ""))))

(defun commas (line)
  (uiop:split-string line :separator ","))

(defun boll (&rest funcs)
  (apply #'alexandria:compose funcs))

(defun header (car-parser cdr-parser)
  "Parses the first line with `car-parser' and the rest with `cdr-parser'. The return
  value is a list. The first parser receives a single line and the other receives a list
  of lines."
  (lambda (lines)
    (cons (funcall car-parser (car lines))
          (funcall cdr-parser (cdr lines)))))

(defun to-lines (line-or-lines)
  (if (stringp line-or-lines)
      (list line-or-lines)
      line-or-lines))

(defun collect-alist (line-or-lines)
  "Finds whitespace-separated words on the form 'k:v' and collects them into an alist"
  (let ((lines (to-lines line-or-lines)))
    (loop for l in lines
          nconc (loop for w in (words l)
                      for kv = (uiop:split-string w :separator ":" :max 2)
                      collect (cons (car kv) (cadr kv))))))

(defun collect-hash-table (line-or-lines)
  (alexandria:alist-hash-table (collect-alist line-or-lines)
                               :test 'equal))

(defun split-at (index)
  "Split the line at the given index into two parts. INDEX is where the second string
starts."
  (lambda (line)
    (cons (subseq line 0 index)
          (subseq line index))))

(defun split-sep (sep)
  (lambda (line)
    (when-let ((x (search sep line)))
      (cons (subseq line 0 x)
            (subseq line (+ x (length sep)))))))

(defun map-cons (left right)
  (lambda (con)
    (cons (funcall left (car con))
          (funcall right (cdr con)))))

(defun bitvector (line)
  (map '(vector bit) #'digit-char-p line))

(defun numbervector (line)
  (map 'vector #'digit-char-p line))

(defun single-line-numbers (&key (type 'list))
  "Returns a function (lambda (lines) ...) which will parse the first line as a comma
separated list of integers."
  (boll (each-line #'parse-integer :type type) #'commas #'car))
