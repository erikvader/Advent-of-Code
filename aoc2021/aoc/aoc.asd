(defsystem "aoc"
  :description "hej"
  :version "0.0.0"
  :author ""
  :licence ""
  :depends-on ("alexandria" ;; generally convenient functions
               "arrows"     ;; ->
               "cl-ppcre"   ;; regex
               "trivia"     ;; pattern matching
               "monotonic-clock" ;; better performance clock
               )
  :serial t
  :components ((:file "aoc")
               (:file "parser")
               (:file "runner")))
