(in-package :aoc)

(defmacro real-time (place &body body)
  `(let* ((now (osicat:get-monotonic-time)) ;; is not the raw one :(
          (res (progn ,@body))
          (elapsed (- (osicat:get-monotonic-time) now)))
     (setf ,place elapsed)
     res))

(defun apply-cons (f con)
  (funcall f (car con) (cdr con)))

(defun run-day (part &key (input-file "./input") expected-answer (parser #'identity) unpack)
  (format t "~&Running ~s...~%" part)
  (sb-ext:gc :full t)
  (let* ((input (uiop:read-file-lines input-file))
         parse-time
         (parsed-input (real-time parse-time
                         (funcall parser input)))
         solution-time
         (solution (real-time solution-time
                     (funcall (ecase unpack
                                (:cons #'apply-cons)
                                ((nil) #'funcall)
                                ((t :list) #'apply))
                              part
                              parsed-input))))
    (format t "Answer: ~s~%" solution)
    (when expected-answer
      (format t "The answer is~a correct~%" (if (equal solution expected-answer) "" " not")))
    (format t "It took ~6$ seconds to parse the input~%" parse-time)
    (format t "It took ~6$ seconds to find the answer~%" solution-time)
    solution))

(defun capture-stdout (f)
  (lambda (&rest args)
    (with-output-to-string (*standard-output*)
      (apply f args))))

(defun run-day-stdout (part &rest args)
  (apply #'run-day (capture-stdout part) args))
