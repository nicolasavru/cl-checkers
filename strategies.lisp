(in-package #:checkers)

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move, apply
  EVAL-FN to each resulting board, and choose the move for which
  EVAL-FN returns the best score. EVAL-FN takes two arguments: the
  player-to-move and board. MAXIMIZER is equivalent to MINIMAX with
  PLY 1."
  #'(lambda (player board)
      (let* ((moves (legal-moves player board))
             (scores (mapcar (lambda (move)
                               (make-move move board)
                               (funcall eval-fn player
                                        (make-move move (copy-board board))))
                             moves))
             (best (apply #'max scores)))
        (elt moves (position best scores)))))

(defun minimax (player board ply eval-fn)
  "Find the best move for PLAYER according to EVAL-FN, searching PLY
  levels deep. Don't call it with PLY 0."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (null (legal-moves (opponent player) board))
                (- (minimax (opponent player) board
                            (- ply 1) eval-fn))
                0) ;; do something useful here?
            (let ((best-move nil)
                  (best-val nil))
              (dolist (move moves)
                (let* ((board2 (make-move move (copy-board board)))
                       (val (- (minimax (opponent player) board2 (- ply 1) eval-fn))))
                  (when (or (null best-val)
                            (> val best-val))
                    (setf best-val val)
                    (setf best-move move))))
              (values best-val best-move))))))

(defun minimax-searcher (ply eval-fn)
  "A streatgy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (minimax player board ply eval-fn)
        (declare (ignore value))
        move)))

(defun alpha-beta (player board achievable cutoff ply eval-fn)
  "Find the best move for PLAYER according to EVAL-FN, searching PLY
  levels deep. Don't call it with PLY 0."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        (if (null moves)
            (if (null (legal-moves (opponent player) board))
                (- (alpha-beta (opponent player) board
                            (- cutoff) (- achievable)
                            (- ply 1) eval-fn))
                0) ;; do something useful here?
            (do* ((best-move (car moves))
                  (moves2 moves (cdr moves2))
                  (move (car moves2) (car moves2))
                  (board2 (make-move move (copy-board board)))
                  (val (- (alpha-beta (opponent player) board2
                                      (- cutoff) (- achievable)
                                      (- ply 1) eval-fn))))
                 ((or (null moves2) (>= achievable cutoff))
                  (values achievable best-move))
              (when (> val achievable)
                (setf achievable val)
                (setf best-move move)))))))

(defun alpha-beta-searcher (ply eval-fn)
  "A streatgy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (alpha-beta player board
                      most-negative-fixnum most-positive-fixnum
                      ply eval-fn)
        (declare (ignore value))
        move)))

(defun human (player board)
  "A human player for the game of Checkers"
  (format t "Legal moves for player ~a: ~%~{~a~%~}" player (legal-moves player board))
  (format *query-io* "[Move]$ ")
  (force-output *query-io*)
  (read *query-io*))

(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))
