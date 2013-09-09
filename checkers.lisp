;;;; checkers.lisp

(in-package #:checkers)

;;; "checkers" goes here. Hacks and glory await!

(defparameter black-steps '(9 11))
(defparameter white-steps '(-9 -11))
(defparameter black-jumps '(18 22))
(defparameter white-jumps '(-18 -22))
(defparameter all-steps '(-11 -9 9 11))
(defparameter all-jumps '(-22 -18 18 22))

(defconstant empty 0 "An empty square")
(defconstant black-man 1 "A black man")
(defconstant black-king 2 "A black king")
(defconstant white-man 3 "A white man")
(defconstant white-king 4 "A white king")
(defconstant outer 5 "Marks squares outside the 8x8 board")

(deftype piece () `(integer ,empty ,outer))
(deftype player-piece () `(integer ,black-man ,white-king))
(deftype black-piece () `(integer ,black-man ,black-king))
(deftype white-piece () `(integer ,white-man ,white-king))

(defun char-of (piece) (char ".bBwW0" piece))
(defun symb-of (piece) (nth piece '(empty black-man black-king white-man white-king outer)))

(defun opponent (player) (if (eql player 'black) 'white 'black))

(defun next-player (player move board &optional kingedp)
  "Select next player to move given the current PLAYER, his previous
  MOVE, the BOARD, and whether a piece was kinged during MOVE."
  (if (and (member (cdr move) all-jumps)
           (legal-moves player board :piece (+ (car move) (cdr move)) :type all-jumps)
           (not kingedp))
      player
      (opponent player)))

(deftype board () '(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defun copy-board (board)
  (copy-seq board))

(defparameter all-squares
  (do ((i 11 (1+ i))
       (l ()))
      ((> i 88) (nreverse l))
    (if (<= 1 (mod i 10) 8)
        (push i l))))

(defparameter black-squares
  (remove-if-not #'(lambda (square)
                     (multiple-value-bind (q r) (floor square 10)
                       (or
                        (and (oddp q) (evenp r))
                        (and (evenp q) (oddp r)))))
                 all-squares))

(defun nth-row-squares (n)
  (remove-if-not (lambda (square) (eql (floor square 10) n)) black-squares))

(defparameter white-kingrow (nth-row-squares 1))
(defparameter black-kingrow (nth-row-squares 8))

(defun count-pieces (player board)
  "Count player's pieces."
  (+ (count (symbol-value (symb player '-man)) board)
     ;; weight kings twice as much as pieces
     (* 2 (count (symbol-value (symb player '-king)) board))))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count-pieces player board)
     (count-pieces (opponent player) board)))

(defun initial-board ()
  "Return a board with initial positions of both players"
  ;; Boards are 100-element vectors with elements
  ;; (<= 1 (mod 11-88 10) 8) used and the others marked with the sentinel OUTER.
  (let* ((board (make-array 100 :element-type 'piece :initial-element outer))
         (black-starting-squares
           (subseq black-squares 0 12))
        (white-starting-squares
           (subseq black-squares (- (length black-squares) 12))))
    (dolist (square all-squares)
      (setf (bref board square) empty))
    (dolist (square black-starting-squares)
      (setf (bref board square) black-man))
    (dolist (square white-starting-squares)
      (setf (bref board square) white-man))
    board))

(defun print-board (board)
  "Print a board."
  (format t "    ~{~d ~}" '(1 2 3 4 5 6 7 8))
  (dotimes (row 8)
    (format t "~& ~d " (* 10 (1+ row)))
    (dotimes (col 8)
      (format t "~c " (char-of (bref board (+ (* 10 (1+ row)) (1+ col)))))))
  (format t "~2&"))

(defun valid-p (move)
  "Valid moves are numbers in the range 11-88 and (<= 1 (mod 11-88 10) 8)"
  (and (consp move)
       (<= 11 (car move) 88) (<= 11 (+ (car move) (cdr move)) 88)
       (<= 1 (mod (car move) 10) 8) (<= 1 (mod (+ (car move) (cdr move)) 10) 8)))

(defun legal-p (move player board &key piece type)
  "Check if MOVE is legal for PLAYER given board. PIECE, if provided,
  specifies piece that must have been moved and TYPE, if provided,
  specifies the moves that must have been made. MOVE is of format
  (src . vec).

  A move is legal if:
  1) the piece at SRC belongs to player
  2) the piece at DST is empty
  3) if PIECE is specified, the piece at SRC is PIECE
  4) if TYPE is specified, VEC is TYPE
  5) if SRC is a BLACK- (WHITE-) MAN, VEC is a BLACK- (WHITE-) STEP or
       VEC is a BLACK- (WHITE-) JUMP and the piece being jumped over is white (black)
  6) if SRC is a BLACK- (WHITE-) KING, VEC is a STEP or
       VEC is a JUMP and the piece being jumped over is white (black)
 "
  ;; (print (list move player board piece type))
  (let* ((src (car move))
         (vec (cdr move))
         (dst (+ src vec))
         (p (bref board src))
         (d (bref board dst))
         (split-piece (split-sequence #\- (symbol-name (symb-of p))))
         (piece-owner (symb (car split-piece)))
         (piece-type (symb (cadr split-piece))))
    (flet ((test-vec (piece-owner piece-type vec)
             "Test if vec is a valid move for PIECE-OWNER's piece of type PIECE-TYPE"
             (cond ((member vec
                            (symbol-value (symb (if (eql piece-type 'king) 'all piece-owner) '-steps)))
                    t)
                   ((member vec
                            (symbol-value (symb (if (eql piece-type 'king) 'all piece-owner) '-jumps)))
                    (typep (bref board (+ (/ vec 2) src)) (symb (opponent piece-owner) '-piece)))
                   (t nil))))
      (and (typep p (if (eql player 'black) 'black-piece 'white-piece))
           (eql d empty)
           (if piece (eql src piece) t)
           (if type (member vec type) t)
           (test-vec piece-owner piece-type vec)))))

(defun legal-piece-moves (pos player board &key (type (append all-steps all-jumps)))
  "Return legal moves of TYPE for PLAYER for piece at POS on BOARD."
  (let ((result ()))
    (dolist (vec type result)
      (let ((dst (+ pos vec))) ; re-write with remove-if
        (when (and (< 11 dst 88)
                   (legal-p (cons pos vec) player board))
          (push vec result))))))

(defun legal-moves-maybe (player board &key piece (type (append all-steps all-jumps)))
  "Return all potential legal moves for PLAYER on BOARD. If PIECE or TYPE
  is spcified, return moves only for that piece and only of that type."
  (let ((result ())
        (player-piece-type (if (eql player 'black) 'black-piece 'white-piece)))
    (if piece
        (mapcar #'(lambda (vec) (cons piece vec))
                      (legal-piece-moves piece player board :type type))
        (dolist (square black-squares result)
          (when (typep (bref board square) player-piece-type)
            (setf result (append (mapcar #'(lambda (vec) (cons square vec))
                                         (legal-piece-moves square player board
                                                            :type type)) result)))))))

(defun legal-moves (player board &key piece (type (append all-steps all-jumps)))
  "Return all definitely legal moves for PLAYER on BOARD. If PIECE or TYPE
  is spcified, return moves only for that piece and only of that type."
  (let* ((moves (legal-moves-maybe player board :piece piece :type type))
         (jumps (remove-if-not (lambda (move) (member (cdr move) all-jumps)) moves)))
    (if jumps jumps moves)))

(defun king-maybe (piece dst)
  "If PIECE has reached its kingrow, return a king of the same colour
  as PIECE, else return PIECE. Also return T if PIECE was kinged, else NIL."
  (cond ((and (eql piece black-man) (member dst black-kingrow))
         (values black-king t))
        ((and (eql piece white-man) (member dst white-kingrow))
         (values white-king t))
        (t (values piece nil))))

(defun make-move (move board)
  "Update BOARD to reflect move by player. Assume MOVE is
  legal. Return the updated board and T if a piece was kinged, else
  NIL."
  (let* ((src (car move))
         (vec (cdr move))
         (dst (+ src vec)))
    ;; (print move)
    (multiple-value-bind (res-piece kingedp) (king-maybe (bref board src) dst)
      (setf (bref board dst) res-piece)
      (setf (bref board src) empty)
      (if (member vec all-jumps)
          (setf (bref board (+ (/ vec 2) src)) empty))
      (values board kingedp))))

(defun get-move (strategy player board &key piece (type (append all-steps all-jumps)))
  "Call PLAYER's STRATEGY to get a move. PIECE, if provided, specifies
  the piece that must be moved. TYPE is a list of moves to consider (and
  check for legality). Keep calling until a legal move is made. Return
  NIL if there are no valid movies."
  (let* ((moves (legal-moves player board :piece piece :type type))
         (move (if moves (funcall strategy player (copy-board board)))))
    (if (null moves)
        nil
        (if (member move moves :test #'equal)
            move
            (progn
              ;; (print moves)
              ;; (print (list move player board piece type))
              (warn "Illegal move: ~a" move)
              (get-move strategy player board :piece piece :type type))))))

(defun checkers (black-strategy white-strategy &key (board (initial-board)) (print t))
  "Play a game of checkers. Return the victor (though not in a vector). "
    (do* ((prev-player () player)
          (player 'black (next-player player move board kingedp))
          (strategy black-strategy (if (eql player 'black) black-strategy white-strategy))
          (move (get-move strategy player board)
                (apply #'get-move strategy player board
                       (if (eql player prev-player)
                           (progn (format t "DOUBLE JUMP~%") (force-output *standard-output*)
                                  (list :piece (+ (car move) (cdr move))
                                        :type all-jumps)))))
          (kingedp (when move
                     (multiple-value-bind (board kingedp)
                         (make-move move board)
                       (declare (ignore board))
                       kingedp))
                   (when move
                     (multiple-value-bind (board kingedp)
                         (make-move move board)
                       (declare (ignore board))
                       kingedp))))
         ((null (legal-moves player board)) (opponent player))
      (when (and move print)
        (format t "Move ~a made by player ~a.~%Updated board:~%" move player)
        (print-board board))))
