(in-package :aoc)

(defmacro real-time (place &body body)
  `(let* ((now (get-internal-real-time))
          (res (progn ,@body))
          (elapsed (- (get-internal-real-time) now)))
     (setf ,place (/ elapsed internal-time-units-per-second))
     res))

(defun run-day (part &key (input-file "./input") expected-answer (parser #'identity))
  (let* ((input (uiop:read-file-lines input-file))
         parse-time
         (parsed-input (real-time parse-time
                         (funcall parser input)))
         solution-time
         (solution (real-time solution-time
                     (funcall part parsed-input))))
    (format t "~&Answer: ~a~%" solution)
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
