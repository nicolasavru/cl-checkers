(ql:quickload "checkers")

(in-package checkers)

(setf *random-state* (make-random-state t))
(defparameter b (initial-board))
(print-board b)
;; (checkers #'random-strategy #'human)
(checkers (alpha-beta-searcher 3 #'aggregate-eval-fun) #'random-strategy)
