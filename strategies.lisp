(in-package #:checkers)

(defun piece-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count-pieces player board)
     (count-pieces (opponent player) board)))

(defun piece-ratio (player board)
  "Count player's pieces divided by opponent's pieces."
  (/ (count-pieces player board)
     (count-pieces (opponent player) board)))

(defun aggregate-eval-fun (player board)
  (let ((dif (piece-difference player board))
        (rat (piece-ratio player board)))
    (+
     ;; maximize piece difference
     dif
     ;; trade pieces (maximize rat) if up in pieces
     (* (if (>= dif 0) 1 -1) rat)
     ;; TODO: proximity function for king moves
     )))

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
            most-negative-fixnum
            (let ((best-move nil)
                  (best-val nil))
              (dolist (move moves)
                (multiple-value-bind (board2 kingedp)
                    (make-move move (copy-board board))
                  (let* ((npl (next-player player move board2 kingedp))
                         (val (funcall (if (eql npl player) #'+ #'-)
                                       (minimax npl board2 (- ply 1) eval-fn))))
                    (when (or (null best-val)
                              (> val best-val))
                      (setf best-val val)
                      (setf best-move move)))))
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
        ;; (format t "player: ~a; ply: ~a; moves: ~a; nm: ~a~%" player ply moves (null moves))
        (if (null moves)
            most-negative-fixnum
            (do* ((best-move (random-elt moves))
                  (moves2 moves (cdr moves2))
                  (move (car moves2) (car moves2)))
                 ((or (null move) (>= achievable cutoff)) (values achievable best-move))
              (multiple-value-bind (board2 kingedp)
                  (make-move move (copy-board board))
                (let* ((npl (next-player player move board2 kingedp))
                       (val (funcall (if (eql npl player) #'+ #'-)
                                     (alpha-beta
                                      npl board2
                                      (funcall (if (eql npl player) #'+ #'-) cutoff)
                                      (funcall (if (eql npl player) #'+ #'-) achievable)
                                      (- ply 1) eval-fn))))
                  ;; (print-board board2)
                  ;; (format t "player: ~a; ply: ~a; val: ~a; achievable: ~a; cutoff: ~a; move: ~a~%"
                  ;;     player ply val achievable cutoff move)
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))))))))

(defun alpha-beta-paip (player board achievable cutoff ply eval-fn)
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
        ;; (format t "player: ~a; ply: ~a; moves: ~a; nm: ~a~%" player ply moves (null moves))
        (if (null moves)
            (if (null (legal-moves (opponent player) board))
                (- (alpha-beta-paip (opponent player) board
                                    (- cutoff) (- achievable)
                                    (- ply 1) eval-fn))
                0)
            (let ((best-move (first moves)))
              (loop for move in moves do
                (let* ((board2 (make-move move (copy-board board)))
                       (val (- (alpha-beta-paip
                                (opponent player) board2
                                (- cutoff) (- achievable)
                                (- ply 1) eval-fn))))
                  ;; (print-board board2)
                  ;; (format t "player: ~a; ply: ~a; val: ~a; achievable: ~a; cutoff: ~a; move: ~a~%"
                  ;;     player ply val achievable cutoff move)
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))
                until (>= achievable cutoff))
              (values achievable best-move))))))

(defun alpha-beta-iterative-deepening (player board achievable cutoff ply end-time eval-fn)
  "Find the best move for PLAYER according to EVAL-FN, searching PLY
  levels deep. Don't call it with PLY 0."
  (if (= ply 0)
      (progn
        ;; (format t "---------------------~%")
        ;; (format t "Current position for player ~a has score: ~a~%" player (funcall eval-fn player board))
        ;; (print-board board)
        ;; (format t "---------------------~%")
        (funcall eval-fn player board))
      (let ((moves (legal-moves player board)))
        ;; (format t "---------------------~%")
        ;; (print-board board)
        ;; (format t "player: ~a; ply: ~a; moves: ~a; ~%" player ply moves)
        ;; (format t "---------------------~%")
        (if (null moves)
            most-negative-fixnum
            (do* ((best-move (random-elt moves))
                  (moves2 moves (cdr moves2))
                  (move (car moves2) (car moves2)))
                 ((or
                   (null move)
                   (>= achievable cutoff)
                   (>= (get-internal-real-time) end-time)) ;; do a partial search if we have time
                  (progn
                   ;; (format t "player, ply, move, achievable, cutoff: ~a ~a ~a ~a ~a~%" player ply best-move achievable cutoff)
                    (values achievable best-move (>= (get-internal-real-time) end-time))))
              (multiple-value-bind (board2 kingedp)
                  (make-move move (copy-board board))
                (let* ((npl (next-player player move board2 kingedp))
                       (val (funcall (if (eql npl player) #'+ #'-)
                                     (alpha-beta-iterative-deepening
                                      npl board2
                                      (if (eql npl player) achievable (- cutoff))
                                      (if (eql npl player) cutoff (- achievable))
                                      (- ply 1) end-time eval-fn))))
                  ;; (format t "---------------------")
                  ;; (print-board board2)
                  ;; (format t "player: ~a; ply: ~a; val: ~a; achievable: ~a; cutoff: ~a; move: ~a~%"
                  ;;     player ply val achievable cutoff move)
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))))))))

(defun alpha-beta-iterative-deepening-wrapper (player board time eval-fn)
  (do* ((moves (legal-moves player board))
        (move (car moves))
        (val)
        (start-time (get-internal-real-time))
        (end-time (+ (get-internal-real-time) time))
        (ply 3 (incf ply)))
       ((or (>= (get-internal-real-time) end-time)
            (> ply 100)
            (= (length moves) 1)) ;; if we only have one move, just play it
        (progn
          (format t "~a searched ~a plys in ~a seconds and found move ~a with val ~a.;~%"
                  player
                  (- ply 2)
                  (float (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second))
                  move val)
          move))
    (multiple-value-bind (new-value new-move out-of-time-p)
        (alpha-beta-iterative-deepening player board
                    most-negative-fixnum most-positive-fixnum
                    ply end-time eval-fn)
      (format t "~a searched ~a plys and found move ~a with val ~a.~%" player ply new-move new-value)
      (unless out-of-time-p
        (setf val new-value)
        (setf move new-move)))))

(defun alpha-beta-searcher (ply eval-fn)
  "A streatgy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (alpha-beta player board
                      most-negative-fixnum most-positive-fixnum
                      ply eval-fn)
        (declare (ignore value))
        move)))

(defun alpha-beta-search-paip (ply eval-fn)
  "A streatgy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
          (alpha-beta-paip player board
                           most-negative-fixnum most-positive-fixnum
                           ply eval-fn)
        (declare (ignore value))
        move)))

(defun alpha-beta-iterative-deepening-searcher (time eval-fn)
  "A streatgy that searches for TIME seconds and then uses EVAL-FN."
  #'(lambda (player board)
      (alpha-beta-iterative-deepening-wrapper player board
                                              (* time internal-time-units-per-second)
                                              eval-fn)))

(defun human (player board)
  "A human player for the game of Checkers"
  (format t "Legal moves for player ~a: ~%~{~a~%~}" player (legal-moves player board))
  (format *query-io* "cl-checkers$ ")
  (force-output *query-io*)
  (read *query-io*))

(defun random-strategy (player board)
  "Make any legal move."
  (format t "Legal moves for player ~a: ~%~{~a~%~}" player (legal-moves player board))
  (random-elt (legal-moves player board)))
