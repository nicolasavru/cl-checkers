(in-package #:checkers)

(defun count-pieces (player board)
  "Count PLAYER's pieces."
  (+ (if (eql player 'black) (boardt-num-black-men board) (boardt-num-white-men board))
     (if (eql player 'black) (boardt-num-black-kings board) (boardt-num-white-kings board))))

(defun piece-difference (player board)
  "Count PLAYER's pieces minus opponent's pieces."
  (- (count-pieces player board)
     (count-pieces (opponent player) board)))

(defun row-weight-fun (pos-piece)
  "Return row weight associated with piece."
  (let ((row (floor (car pos-piece) 10))
        (piece (cdr pos-piece)))
    (cond
      ((= piece black-man) row)
      ((= piece white-man) (- 9 row))
      (t 0))))

(defun piece-value (piece)
  "Return intrinsic value of piece."
  (cond
    ((= piece black-man) 15)
    ((= piece white-man) 15)
    ((= piece black-king) 30)
    ((= piece white-king) 30)
    (t 0)))

(defun proximity-weight-fun (pos-piece player board)
  "Weight piece proportional to its proximity to enemy pieces."
  (multiple-value-bind (row col)
      (floor (car pos-piece) 10)
    (let* ((num-pieces (+ (boardt-num-white-men board)
                           (boardt-num-white-kings board)
                           (boardt-num-black-men board)
                           (boardt-num-black-kings board)))
           (enemy-pos-pieces (player-pieces (opponent player) board))
           (sd (float (+ 1 (reduce #'+ (hash-table-keys enemy-pos-pieces)
                                   :key #'(lambda (enpos)
                                            (multiple-value-bind (enrow encol)
                                                (floor enpos 10)
                                              (let ((rowdist (abs (- row enrow)))
                                                    (coldist (abs (- col encol))))
                                                (+ (if (> num-pieces 5)
                                                       (* rowdist rowdist rowdist)
                                                       rowdist)
                                                   (if (> num-pieces 5)
                                                       (* coldist coldist coldist)
                                                       coldist)))))
                                   :initial-value 0)))))
      (/ 128.0 sd))))

(defun total-piece-value (pos-piece player board)
  "Return total value associated with piece."
  (+ (piece-value (cdr pos-piece))
     ;; activate row weighting if enemy has 5 or fewer pieces
     (if (<= (if (eql player 'black)
                 (+ (boardt-num-white-men board)
                    (boardt-num-white-kings board))
                 (+ (boardt-num-black-men board)
                    (boardt-num-black-kings board)))
             5)
         (row-weight-fun pos-piece)
         0)
     ;; apply proximity weighting only to kings
     (if (or (eql (cdr pos-piece) black-king)
             (eql (cdr pos-piece) white-king))
         (proximity-weight-fun pos-piece player board)
         0)))

(defun weighted-piece-difference (player board value-fun)
  "Count player's pieces minus opponent's pieces."
  (let ((player-pos-pieces (player-pieces player board))
        (opponent-pos-pieces (player-pieces (opponent player) board)))
    (- (reduce #'+ (hash-table-values player-pos-pieces)
               :key #'(lambda (pos-piece) (funcall value-fun pos-piece player board))
               :initial-value 0)
       (reduce #'+ (hash-table-values opponent-pos-pieces)
               :key #'(lambda (pos-piece) (funcall value-fun pos-piece (opponent player) board))
               :initial-value 0))))

(defun piece-ratio (player board)
  "Count player's pieces divided by opponent's pieces."
  (/ (float (count-pieces player board))
     (float (count-pieces (opponent player) board))))

(defun simple-eval-fun (player board)
  (weighted-piece-difference
   player
   board
   #'(lambda (pos-piece player board)
       (declare (ignore player board))
       (piece-value (cdr pos-piece)))))

(defun aggregate-eval-fun (player board)
  (let ((cached-val (gethash board (if (eql player 'black)
                                       *black-transposition-table*
                                       *white-transposition-table*))))
    (if cached-val
        cached-val
        (setf (gethash board (if (eql player 'black)
                                 *black-transposition-table*
                                 *white-transposition-table*))
              ;; treat 1 piece vs 1 piece as a draw and evaluate it as 0
              (if (= (+ (boardt-num-white-men board)
                        (boardt-num-white-kings board))
                     (+ (boardt-num-black-men board)
                        (boardt-num-black-kings board))
                     1)
                  0
                  (let ((wdif (weighted-piece-difference player board #'total-piece-value))
                        (dif (piece-difference player board))
                        (rat (piece-ratio player board)))
                    (+
                     ;; maximize piece value difference
                     wdif
                     0
                     ;; trade pieces (maximize rat) if up in pieces
                     (* (cond ((> dif 0) 100)
                              ((= dif 0) 0)
                              ((< dif 0) -100)) rat))))))))

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move, apply
  EVAL-FN to each resulting board, and choose the move for which
  EVAL-FN returns the best score. EVAL-FN takes two arguments: the
  player-to-move and board. MAXIMIZER is equivalent to MINIMAX with
  PLY 1."
  #'(lambda (player board)
      (let* ((moves (legal-moves player board))
             (scores (mapcar (lambda (move)
                               (make-move player move board)
                               (funcall eval-fn player
                                        (make-move player move (copy-boardt board))))
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
                    (make-move player move (copy-boardt board))
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

(defun alpha-beta (player board achievable cutoff ply eval-fn &key piece type)
  "Find the best move for PLAYER according to EVAL-FN, searching PLY
  levels deep. Don't call it with PLY 0."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (if piece
                       (legal-moves player board :piece piece :type type)
                       (legal-moves player board))))
        (if (null moves)
            most-negative-fixnum
            (do* ((best-move (random-elt moves))
                  (moves2 moves (cdr moves2))
                  (move (car moves2) (car moves2)))
                 ((or (null move) (>= achievable cutoff)) (values achievable best-move))
              (multiple-value-bind (board2 kingedp)
                  (make-move player move (copy-boardt board))
                (let* ((npl (next-player player move board2 kingedp))
                       (val (funcall (if (eql npl player) #'+ #'-)
                                     (alpha-beta
                                      npl board2
                                      (funcall (if (eql npl player) #'+ #'-) cutoff)
                                      (funcall (if (eql npl player) #'+ #'-) achievable)
                                      (- ply 1) eval-fn))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))))))))

(defun alpha-beta-iterative-deepening (player board achievable cutoff ply end-time eval-fn
                                       &key piece type)
  "Find the best move for PLAYER according to EVAL-FN, searching PLY
  levels deep. Don't call it with PLY 0."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (if piece
                       (legal-moves player board :piece piece :type type)
                       (legal-moves player board))))
        (if (null moves)
            most-negative-fixnum                            ; PLAYER lost
            (do* ((best-move (random-elt moves))
                  (moves2 moves (cdr moves2))
                  (move (car moves2) (car moves2)))
                 ((or
                   (null move)
                   (>= achievable cutoff)
                   (>= (get-internal-real-time) end-time))
                  (progn
                    (values achievable best-move (>= (get-internal-real-time) end-time))))
              (multiple-value-bind (board2 kingedp)
                  (make-move player move (copy-board board))
                (let* ((npl (next-player player move board2 kingedp))
                       (val (if (eql npl player)
                                (alpha-beta-iterative-deepening
                                 npl board2 achievable cutoff
                                 ply                        ; count multiple jumps as same ply
                                 end-time eval-fn
                                 :piece (+ (car move) (cdr move)) :type all-jumps)
                                (- (alpha-beta-iterative-deepening
                                    npl board2 (- cutoff) (- achievable)
                                    (1- ply) end-time eval-fn)))))
                  (when (> val achievable)
                    (setf achievable val)
                    (setf best-move move)))))))))

(defun alpha-beta-iterative-deepening-wrapper (player board time eval-fn &key piece type)
  (do* ((*black-transposition-table* (make-hash-table))
        (*white-transposition-table* (make-hash-table))
        (moves (if piece
                   (legal-moves player board :piece piece :type type)
                   (legal-moves player board)))
        (move (car moves))
        (val)
        (start-time (get-internal-real-time))
        (end-time (+ (get-internal-real-time) time))
        (ply 3 (incf ply)))
       ((or (>= (get-internal-real-time) end-time)
            (> ply 100)
            (= (length moves) 1)) ;; if we only have one move, just play it
        (progn
          (format t "~a searched ~a plys in ~a seconds and found move ~a with val ~a.~%"
                  player
                  (- ply 2)
                  (float (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second))
                  move val)
          move))
    (multiple-value-bind (new-value new-move out-of-time-p)
        (alpha-beta-iterative-deepening player board
                    most-negative-fixnum most-positive-fixnum
                    ply end-time eval-fn :piece piece :type type)
      (if *debug*
          (format t "~a searched ~a plys and found move ~a with val ~a.~%"
                  player ply new-move new-value))
      (unless out-of-time-p
        (setf val new-value)
        (setf move new-move)))))

(defun alpha-beta-searcher (ply eval-fn)
  "A streatgy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board &key piece type)
      (multiple-value-bind (value move)
          (alpha-beta player board
                      most-negative-fixnum most-positive-fixnum
                      ply eval-fn :piece piece :type type)
        (declare (ignore value))
        move)))

(defun alpha-beta-iterative-deepening-searcher (time eval-fn)
  "A streatgy that searches for TIME seconds and then uses EVAL-FN."
  #'(lambda (player board &key piece type)
      (alpha-beta-iterative-deepening-wrapper player board
                                              (* (- time .03) ; margin of error
                                                 internal-time-units-per-second)
                                              eval-fn
                                              :piece piece :type type)))

(defun human (player board &key piece type)
  "A human player for the game of Checkers"
  (format t "Legal moves for player ~a: ~%~{~a~%~}" player (legal-moves player board))
  (format *query-io* "cl-checkers$ ")
  (force-output *query-io*)
  (read *query-io*))

(defun random-strategy (player board)
  "Make any legal move."
  (format t "Legal moves for player ~a: ~%~{~a~%~}" player (legal-moves player board))
  (random-elt (legal-moves player board)))
