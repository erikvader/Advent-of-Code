(defpackage :aoc
  (:use :cl :alexandria :arrows :iterate)
  (:import-from :trivia :match*)
  ;; (:local-nicknames (:t :trivia))
  (:export :run-day
           :run-day-stdout
           :capture-stdout
           :each-line
           :guard
           :line-equal
           :lisp-read
           :regex
           :branch
           :char-grid
           :paragraphs
           :commas
           :words
           :collect-alist
           :collect-hash-table
           :split-at
           :split-half
           :group-size
           :bitvector
           :numbervector
           :boll
           :header
           :single-line-numbers
           :map-cons
           :split-sep
           :split-sep-list
           :chars
           :to-char
           :index-of
           :trim
           :to-plist
           :parse-conses
           :parse-lists
           :commas-numbers
           :integer-grid
           :integer-grid-tight))
(in-package :aoc)
