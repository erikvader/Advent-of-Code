(defpackage :aoc
  (:use :cl :alexandria :arrows)
  (:import-from :trivia :match*)
  ;; (:local-nicknames (:t :trivia))
  (:export :run-day
           :run-day-stdout
           :capture-stdout))
(in-package :aoc)
