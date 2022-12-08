(defclass direntry ()
  ((files
    :initform nil
    :accessor files)
   (dirs
    :initform nil
    :accessor dirs)))

(defclass file ()
  ((name
    :initarg :name
    :accessor filename)
   (size
    :initarg :size
    :accessor filesize)))

(defmethod print-object ((obj file) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "name: ~s, size: ~a" (filename obj) (filesize obj))))

(defmethod print-object ((obj direntry) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "files: ~s, dirs: ~a" (files obj) (dirs obj))))

(defun commandp (line)
  (when (string= "$ " line :end2 2)
    (subseq line 2)))

(defun lsp (cmd)
  (string= cmd "ls"))

(defun cdp (cmd)
  (when (string= "cd " cmd :end2 3)
    (subseq cmd 3)))

(defun dirp (line)
  (when (string= "dir " line :end2 4)
    (subseq line 4)))

(defun create-file (line)
  (destructuring-bind (size name) (uiop:split-string line)
    (make-instance 'file :name name :size (parse-integer size))))

(defun read-tree (commands)
  (let* ((tree-root (make-instance 'direntry))
         (tree tree-root)
         stack)
    (setq commands (cdr commands)) ;; remove cd /
    (iter (for line in commands)
          (cond
            ((dirp line)
             (push (cons (dirp line) (make-instance 'direntry))
                   (dirs tree)))
            ((not (commandp line))
             (push (create-file line)
                   (files tree)))
            ((lsp (commandp line)))
            ((cdp (commandp line))
             (if (string= ".." (cdp (commandp line)))
                 (progn
                   (unless stack
                     (error "aj aj aj"))
                   (setq tree (pop stack)))
                 (progn
                   (push tree stack)
                   (setq tree (cdr (assoc (cdp (commandp line)) (dirs tree) :test #'string=))))))
            (t (error "'~s' is not a valid line" line))))
    tree-root))

(defun calc-all-sizes (cur tree)
  (let* ((dir-sizes (iter (for (dir . dirtree) in (dirs tree))
                          (for xs = (calc-all-sizes dir dirtree))
                          (collect (car xs) into direct-children)
                          (nconcing (cdr xs) into grandchildren)
                          (finally (return (nconc direct-children grandchildren)))))
         (files-size (iter (for f in (files tree))
                           (sum (filesize f))))
         (total-size (+ files-size
                        (iter (for x in dir-sizes)
                              (for i below (length (dirs tree)))
                              (sum (cdr x))))))
    (cons (cons cur total-size)
          dir-sizes)))

(defun sum-under-10k (dir-sizes)
  (iter (for (nil . s) in dir-sizes)
        (when (<= s 100000)
          (sum s))))

(defun dir-to-delete (dir-sizes)
  (iter (with tofree = (- (cdar dir-sizes) 40000000))
        (for (nil . size) in dir-sizes)
        (when (>= size tofree)
          (minimize size))))

(defun part1 (commands)
  (->>
   (read-tree commands)
   (calc-all-sizes "/")
   (sum-under-10k)))

(defun part2 (commands)
  (->>
   (read-tree commands)
   (calc-all-sizes "/")
   (dir-to-delete)))

(defparameter *parser* #'identity)

(aoc:run-day #'part1 :parser *parser*
                     :expected-answer 1644735)
(aoc:run-day #'part2 :parser *parser*
                     :expected-answer 1300850)
