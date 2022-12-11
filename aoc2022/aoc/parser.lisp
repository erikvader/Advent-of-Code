(in-package :aoc)

(defmacro defun-curry (name args &body body)
  "`defun' a function with currying. ARGS does not support &optional, &rest or &key, only
simple arguments are allowed."
  (let ((body-fun (gensym))
        (optional-args (mapcar (lambda (x)
                                 (list x nil (gensym)))
                               args))
        (docstring "")
        (actual-body body))
    (when (stringp (car body))
      (setf docstring (car body)
            actual-body (cdr body)))

    `(defun ,name (&optional ,@optional-args) ,docstring
       (flet ((,body-fun ,args ,@actual-body))
         ,(if (null args)
              `(,body-fun)
              `(cond
                 (,(nth 2 (car (last optional-args))) (,body-fun ,@args))
                 ,@(maplist (lambda (xs)
                              `(,(nth 2 (car xs)) (curry #',name
                                                         ,@(nreverse (mapcar #'car xs)))))
                            (cdr (reverse optional-args)))
                 (t #',name)))))))

(defmacro defcurry (&body fundef)
  "Wrapper around a `defun' which replaces `defun' with `defun-curry'. Handy cuz it
doesn't need any extra modification/settings to have proper syntax highlighting
appropriate for a `defun'.

The &body is there to make Slime/Emacs indent everything with few spaces. If it
doesn't, evaluate: (function-put 'defcurry 'common-lisp-indent-function '(&body))"
  `(defun-curry ,@(cdar fundef)))

(defcurry
  (defun each-line (type f lines)
    "Returns a function that parses each line with F. The desired return type, e.g., list or
vector, is specified with TYPE."
    (map type f lines)))

(defcurry
  (defun trim (lines)
    "Filter out empty lines"
    (remove-if #'uiop:emptyp lines)))

(defcurry
  (defun words (line)
    (delete-if #'uiop:emptyp
               (uiop:split-string line :separator " "))))

(defmacro regex (return-type &rest string-or-cons)
  "Cool macro"
  (let* ((expanded (->> string-or-cons
                        (mapcar (lambda (x)
                                  (case x
                                    (:number '("[0-9]+" . #'parse-integer))
                                    (:inumber '("-?[0-9]+" . #'parse-integer))
                                    (:char '("[a-zA-Z]" . (rcurry #'aref 0)))
                                    (:dchar '("[a-zA-Z][a-zA-Z]" . (lambda (cs)
                                                                     (cons (aref cs 0)
                                                                           (aref cs 1)))))
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
must be the same as the number of capture groups in REGEX.

RETURN-TYPE is a valid argument to the function MAP, or the special value PAIR. If it is
:PAIR, then exactly two capture groups are expected and their parsed values are returned in
a cons."
  (assert (and (or (not (eq return-type :pair))
                   (= (length parsers) 2))
               (or (not (eq return-type :single))
                   (= (length parsers) 1)))
          nil
          "There must be exactly two capture groups if RETURN-TYPE is :pair and one if :single")
  (let ((ptrn (ppcre:create-scanner regex))
        (map-type (case return-type
                    ((:pair :single) 'list)
                    (t return-type))))
    (lambda (line)
      (when-let* ((groups (nth-value 1 (ppcre:scan-to-strings ptrn line)))
                  (res (map map-type
                            (lambda (g p)
                              (funcall (or p #'identity) g))
                            groups
                            parsers)))
        (case return-type
          (:pair (cons (car res) (cadr res)))
          (:single (car res))
          (t res))))))

(defun branch (&rest parsers)
  "Runs each parser in sequence and returns the result and the index of the first
  one that succeeds, i.e. returns non-nil and doesn't signal."
  (lambda (line)
    (loop for p in parsers
          for i from 0
          for x = (ignore-errors (funcall p line))
          when x
            return (values x i))))

(defcurry
  (defun guard (pred line)
    "Aborts a branch of `branch' if PRED returns non-nil when given LINE as its argument."
    (unless (funcall pred line)
      (error "Guard predicate ~s failed on line ~s" pred line))
    line))

(defcurry
  (defun line-equal (test line)
    "Curried version of `string='"
    (string= test line)))

(defcurry
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
      array)))

(defcurry
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
      array)))

(defcurry
  (defun grid-map (f grid)
    (let ((view (make-array (reduce #'* (array-dimensions grid))
                            :displaced-to grid)))
      (map-into view f view)
      grid)))

(defcurry
  (defun integer-grid-tight (lines)
    "Parses a grid of integers into a matrix. Each column is one character wide and each row
is on a separate line."
    (->> (char-grid lines)
         (grid-map #'digit-char-p))))

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
  "Same as `group-by-reverse' but preserves the order."
  (mapcar #'nreverse (nreverse (group-by-reverse to-group split-here-p))))

(defcurry
  (defun paragraphs (lines)
    "Groups lines into paragraphs"
    (group-by lines #'uiop:emptyp)))

(defcurry
  (defun group-size (size lines)
    "Groups LINES into groups of size SIZE. The length of LINES must be a multiple of
SIZE."
    (iter (for tail on lines by (lambda (xs)
                                  (subseq xs size)))
          (collect (subseq tail 0 size)))))

(defcurry
  (defun commas (line)
    (uiop:split-string line :separator ",")))

(defun boll (&rest funcs)
  (apply #'alexandria:compose funcs))

(defcurry
  (defun header (car-parser cdr-parser lines)
    "Applies car-parser to the first element of lines and cdr-parser to the rest."
    (cons (funcall car-parser (car lines))
          (mapcar cdr-parser (cdr lines)))))

(defcurry
  (defun map-cons (left right con)
    "Maps both values of a cons-cell."
    (cons (funcall left (car con))
          (funcall (or right left) (cdr con)))))

(defun to-lines (line-or-lines)
  (if (stringp line-or-lines)
      (list line-or-lines)
      line-or-lines))

(defcurry
  (defun collect-alist (line-or-lines)
    "Finds whitespace-separated words on the form 'k:v' and collects them into an alist"
    (let ((lines (to-lines line-or-lines)))
      (loop for l in lines
            nconc (loop for w in (words l)
                        for kv = (uiop:split-string w :separator ":" :max 2)
                        collect (cons (car kv) (cadr kv)))))))

(defcurry
  (defun collect-hash-table (conses)
    (alexandria:alist-hash-table conses :test 'equal)))

(defcurry
  (defun split-at (index line)
    "Split the line at the given index into two parts. INDEX is where the second string
starts."
    (cons (subseq line 0 index)
          (subseq line index))))

(defcurry
  (defun split-sep (sep line)
    "Splits a line at a separator, the separator is not included in any half."
    (when-let ((x (search sep line)))
      (cons (subseq line 0 x)
            (subseq line (+ x (length sep)))))))

(defcurry
  (defun split-half (line)
    "Splits a line in half"
    (split-at (floor (length line) 2) line)))

(defcurry
  (defun bitvector (line)
    (map '(vector bit) #'digit-char-p line)))

(defcurry
  (defun numbervector (line)
    (map 'vector #'digit-char-p line)))

(defcurry
  (defun chars (type line)
    "Convert a line into a list or vector of characters."
    (assert (member type '(list vector)))
    (coerce line type)))

(defcurry
  (defun to-char (line)
    "Converts a line of a single character to that character."
    (coerce line 'character)))

(defcurry
  (defun index-of (sequence line)
    "Returns the index of where line is in sequence"
    (position line sequence :test #'equal)))

(defcurry
  (defun single-line-numbers (type lines)
    "Parse the first line as a comma separated list of integers."
    (->> lines
         (car)
         (commas-numbers type))))

(defcurry
  (defun commas-numbers (type line)
    "Parse the line as a comma separated list of integers."
    (->> line
         (commas)
         (map type #'parse-integer))))

(defun interleave (list &rest more-lists)
  "Returns a flat list with the elements from all lists in order."
  (apply #'nconc (apply #'mapcar #'list list more-lists)))

(defcurry
  (defun to-plist (keys elements)
    (interleave keys elements)))

(defcurry
  (defun parse-conses (left-paren right-paren period line)
    "Parses a line that looks like conses, but with different characters."
    (-<> line
     (ppcre:regex-replace-all (ppcre:quote-meta-chars left-paren) <> "(")
     (ppcre:regex-replace-all (ppcre:quote-meta-chars right-paren) <> ")")
     (ppcre:regex-replace-all (ppcre:quote-meta-chars period) <> " . ")
     (read-from-string))))

(defcurry
  (defun lisp-read (lines)
    "Reads a lisp object from LINES. Don't forget about the handy #. i.e. read-time
evaluation."
    ;TODO: is this open necessary?
    (with-open-stream (s (->> lines
                              (mapcar #'make-string-input-stream)
                              (apply #'make-concatenated-stream)))
      (read s))))
