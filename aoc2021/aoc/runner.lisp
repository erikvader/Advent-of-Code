(in-package :aoc)

(defun run-day (part &key (input-file "./input") solution-file (parse-line #'identity))
  (let ((solution (->> (uiop:read-file-lines input-file)
                       (mapcar parse-line)
                       (funcall part))))
    (format t "Answer: ~a~%" solution)
    (when solution-file
      (if (equal solution
                 (uiop:read-file-form solution-file))
          (uiop:println "The answer is correct!")
          (uiop:println "the answer is not correct!")))
    solution))

(defun capture-stdout (f)
  (lambda (&rest args)
    (with-output-to-string (*standard-output*)
      (apply f args))))

(defun run-day-stdout (part &rest args)
  (apply #'run-day (capture-stdout part) args))
