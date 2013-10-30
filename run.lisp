(ql:quickload "checkers")

(in-package checkers)

(setf *random-state* (make-random-state t))
(defparameter b (initial-board))
(print-board b)

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                               :report :flat
                               :loop nil)
  (checkers (alpha-beta-iterative-deepening-searcher 1 #'aggregate-eval-fun)
            (alpha-beta-iterative-deepening-searcher 1 #'simple-eval-fun)))
