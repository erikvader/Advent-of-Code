;;TODO: search all parent directories for aoc/
(pushnew (merge-pathnames "aoc/")
         asdf:*central-registry*
         :test #'equalp)

(ql:quickload "aoc")
(use-package :iterate)
(use-package :arrows)
(use-package :alexandria)
(use-package :queues)
(require :queues.simple-queue)
