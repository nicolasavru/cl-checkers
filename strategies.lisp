(in-package #:checkers)

(defun human (player board)
  "A human player for the game of Checkers"
  ;; (declare (ignore board))
  ;; (format t "~&~c to move: " (char-of player))
  (format t "Legal moves for player ~a: ~%~{~a~%~}" player (legal-moves player board))
  (format *query-io* "[Move]$ ")
  (force-output *query-io*)
  (read *query-io*))

(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))
