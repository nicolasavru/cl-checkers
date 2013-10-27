(ql:quickload "checkers")

(in-package checkers)

(setf *random-state* (make-random-state t))
(defparameter b (initial-board))
(print-board b)
;; (checkers #'random-strategy #'human)
(checkers (alpha-beta-iterative-deepening-searcher 1 #'aggregate-eval-fun)
          (alpha-beta-iterative-deepening-searcher 1 #'piece-difference))
