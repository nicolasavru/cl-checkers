(ql:quickload "checkers")

(in-package checkers)

(setf *random-state* (make-random-state t))
(defparameter b (initial-board))
(print-board b)
;; (checkers #'random-strategy #'human)
(checkers (minimax-search 3 #'count-difference)  #'random-strategy)
