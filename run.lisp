(ql:quickload "checkers")

(in-package checkers)

(setf *random-state* (make-random-state t))
(defparameter b (initial-board))
(print-board b)
;; (checkers #'random-strategy #'human)
(format t "~%~a wins!~%" (checkers #'random-strategy #'random-strategy))
