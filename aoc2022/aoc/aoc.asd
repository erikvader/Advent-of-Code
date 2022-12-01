(defsystem "aoc"
  :description "hej"
  :version "0.0.0"
  :author ""
  :licence ""
  :depends-on ("alexandria" ;; generally convenient functions
               "arrows"     ;; ->
               "cl-ppcre"   ;; regex
               "trivia"     ;; pattern matching
               "osicat"     ;; monotonic clock etc
               "iterate"    ;; nicer loop
               )
  :serial t
  :components ((:file "aoc")
               (:file "parser")
               (:file "runner")))
