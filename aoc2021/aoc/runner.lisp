(in-package :aoc)

(defmacro real-time (place &body body)
  `(let* ((now (monotonic-now :raw))
          (res (progn ,@body))
          (elapsed (- (monotonic-now :raw) now)))
     (setf ,place (/ elapsed (monotonic-time-units-per-second)))
     res))

(defun run-day (part &key (input-file "./input") expected-answer (parser #'identity) unpack)
  (format t "~&Running ~s...~%" part)
  (sb-ext:gc :full t)
  (let* ((input (uiop:read-file-lines input-file))
         parse-time
         (parsed-input (real-time parse-time
                         (funcall parser input)))
         solution-time
         (solution (real-time solution-time
                     (funcall (if unpack
                                  #'apply
                                  #'funcall)
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
