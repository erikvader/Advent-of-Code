(pushnew (merge-pathnames "aoc/")
         asdf:*central-registry*
         :test #'equalp)
