(in-package :aoc)

(defun each-line (f)
  (lambda (lines)
    (mapcar f lines)))

(defmacro regex (return-type &rest string-or-cons)
  "Cool macro"
  (let ((regex-string (->> string-or-cons
                           (mapcan (lambda (sc)
                                     (if (consp sc)
                                         (list "(" (car sc) ")")
                                         (list sc))))
                           (apply #'concatenate 'string)))
        (parsers (->> string-or-cons
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

(defun words (line)
  (uiop:split-string line :separator " "))

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
