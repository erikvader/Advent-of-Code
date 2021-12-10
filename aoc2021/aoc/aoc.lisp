(defpackage :aoc
  (:use :cl :alexandria :arrows)
  (:import-from :trivia :match*)
  (:import-from :monotonic-clock :monotonic-time-units-per-second :monotonic-now)
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
           :split-at
           :bitvector
           :numbervector
           :boll
           :header
           :single-line-numbers
           :map-cons
           :split-sep
           :chars
           :integer-grid
           :integer-grid-tight))
(in-package :aoc)
