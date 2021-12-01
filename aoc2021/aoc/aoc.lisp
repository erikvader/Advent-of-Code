(defpackage :aoc
  (:use :cl :alexandria :arrows)
  (:import-from :trivia :match*)
  ;; (:local-nicknames (:t :trivia))
  (:export :run-day
           :run-day-stdout
           :capture-stdout
           :each-line
           :regex
           :branch
           :char-grid
           :paragraphs
           :commas
           :words
           :collect-alist
           :collect-hash-table
           :split-at))
(in-package :aoc)
