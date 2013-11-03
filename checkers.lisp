;;;; checkers.lisp

(in-package #:checkers)

;;; "checkers" goes here. Hacks and glory await!

(defparameter black-steps '(9 11))
(defparameter white-steps '(-9 -11))
(defparameter black-jumps '(18 22))
(defparameter white-jumps '(-18 -22))
(defparameter all-steps '(-11 -9 9 11))
(defparameter all-jumps '(-22 -18 18 22))

(defconstant empty 0)
(defconstant black-man 1)
(defconstant black-king 3)
(defconstant white-man 2)
(defconstant white-king 4)
(defconstant outer 5)

(defconstant white 1)
(defconstant black 2)

(deftype piece () `(integer ,empty ,outer))

;; declare transposition tables dynamic
(defparameter *black-transposition-table* nil)
(defparameter *white-transposition-table* nil)

(defstruct boardt
  (squares (make-array 100 :element-type 'piece :initial-element outer) :type (vector piece))
  (num-black-men 0 :type fixnum)
  (num-black-kings 0 :type fixnum)
  (num-white-men 0 :type fixnum)
  (num-white-kings 0 :type fixnum)
  (black-pos-piece-table (make-hash-table) :type hash-table)
  (white-pos-piece-table (make-hash-table) :type hash-table))

;; default parameters
(defparameter *xscale* 1)
(defparameter *yscale* 1)
(defparameter *debug* nil)
(defparameter piece-alist-list
  (list
   '((str . "."))
   '((str . "b") (fg . :red))
   '((str . "w") (fg . :white))
   '((str . "B") (fg . :red))
   '((str . "W") (fg . :white))
   '((str . "O")))
  "List of alists containing properties for each piece type.")

(defun alist-of (piece)
  "Return alist associated with PIECE."
  (nth piece piece-alist-list))

(defun string-of (piece)
  "Return a string representing PIECE."
  (let ((piece-alist (alist-of piece)))
    (color-string (cdr (assoc 'str piece-alist))
                  :fg (cdr (assoc 'fg piece-alist)))))

(defun opponent (player) (if (eql player 'black) 'white 'black))

(defun next-player (player move board &optional kingedp)
  "Select next player to move given the current PLAYER, his previous
  MOVE, the BOARD, and whether a piece was kinged during MOVE."
  (let ((dst (+ (car move) (cdr move))))
    (if (and (member (cdr move) all-jumps)
             (legal-moves player board :piece dst :type all-jumps)
             (not kingedp))
        player
        (opponent player))))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defun copy-squares (squares)
  (copy-seq squares))

(defun copy-board (board)
  "Deep copy BOARD."
  (let ((new-board (copy-boardt board)))
    (setf (boardt-squares new-board) (copy-squares (boardt-squares board)))
    (setf (boardt-black-pos-piece-table new-board)
          (copy-hash-table (boardt-black-pos-piece-table board)))
    (setf (boardt-white-pos-piece-table new-board)
          (copy-hash-table (boardt-white-pos-piece-table board)))
    new-board))

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

(defun player-pieces (player board)
  "Return a list of (POS . PIECE) cons cells."
  (if (eql player 'black)
      (boardt-black-pos-piece-table board)
      (boardt-white-pos-piece-table board))
  )

(defun initial-board ()
  "Return a board with initial positions of both players"
  ;; Boards are 100-element vectors with elements
  ;; (<= 1 (mod 11-88 10) 8) used and the others marked with the sentinel OUTER.
  (let* ((board (make-boardt :num-black-men 12
                             :num-white-men 12))
         (black-starting-squares
           (subseq black-squares 0 12))
         (white-starting-squares
           (subseq black-squares (- (length black-squares) 12))))

    (dolist (square all-squares)
      (setf (bref (boardt-squares board) square) empty))

    (dolist (square black-starting-squares)
      (setf (bref (boardt-squares board) square) black-man)
      (setf (gethash square (boardt-black-pos-piece-table board)) (cons square black-man)))

    (dolist (square white-starting-squares)
      (setf (bref (boardt-squares board) square) white-man)
      (setf (gethash square (boardt-white-pos-piece-table board)) (cons square white-man)))
    board))

(defun load-game (filename black-strategy white-strategy)
  (let ((in (open filename))
        (input-squares)
        (player-to-move)
        (time)
        (line)
        (board))

    (dotimes (row 8)
      (setf line (string-trim '(#\Return) (read-line in)))
      (setf input-squares
            (nconc input-squares
                   (read-from-string (concatenate 'string "(" line ")")))))

    (setf board (load-board input-squares))

    (setf line (string-trim '(#\Return) (read-line in)))
    (setf player-to-move (if (= (parse-integer line) 1) 'black 'white))

    (setf line (string-trim '(#\Return) (read-line in)))
    (setf time (parse-integer line))

    (checkers (funcall black-strategy time)
              (funcall white-strategy time)
              :player-to-move player-to-move
              :board board)))

(defun load-board (input-squares)
  (let ((i 0)
        (board (make-boardt)))

    (dolist (square all-squares)
      (setf (bref (boardt-squares board) square) empty))

    (dolist (square black-squares)
      (let ((piece (elt input-squares i)))
        (setf (bref (boardt-squares board) square) piece)
        (cond ((= piece black-man)
               (setf (gethash square (boardt-black-pos-piece-table board)) (cons square black-man))
               (incf (boardt-num-black-men board)))

              ((= piece black-king)
               (setf (gethash square (boardt-black-pos-piece-table board)) (cons square black-king))
               (incf (boardt-num-black-kings board)))

              ((= piece white-man)
               (setf (gethash square (boardt-white-pos-piece-table board)) (cons square white-man))
               (incf (boardt-num-white-men board)))

              ((= piece white-king)
               (setf (gethash square (boardt-white-pos-piece-table board)) (cons square white-king))
               (incf (boardt-num-white-kings board)))))

      (incf i))
    board))

(defun print-board (board &key (xscale *xscale*) (yscale *yscale*))
  "Print a board."
  (format t "    ")
  (dotimes (col 8)
    (format t "~d~a" (1+ col) (make-string (if (> xscale 1) (1+ xscale) 1)
                                      :initial-element #\space)))
  (dotimes (row 8)
    (format t "~a ~d "
            (make-string (if (> xscale 1) 2 1) :initial-element #\newline)
            (* 10 (1+ row)))
    (let ((line ""))
      (dotimes (col 8)
        (setf line
              (concatenate 'string line
                           (format nil "~A~a"
                                   (string-of (bref (boardt-squares board)
                                                               (+ (* 10 (1+ row)) (1+ col))))
                                   (make-string (if (> xscale 1) 2 1)
                                                :initial-element #\space)))))
      (format t "~A" (scale-text line :xscale xscale :yscale yscale
                                      :line-sep (format nil "~C    "
                                                        #\newline)
                                      :chars '(#\. #\b #\B #\w #\W)))))
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
  (let* ((src (car move))
         (vec (cdr move))
         (dst (+ src vec))
         (p (bref (boardt-squares board) src))
         (d (bref (boardt-squares board) dst)))
    (flet ((test-vec (p vec)
             "Test if vec is a valid move for PIECE-OWNER's piece of type PIECE-TYPE"
             (cond ((= p black-man)
                    (cond ((member vec black-steps) t)
                          ((member vec black-jumps)
                           (member (bref (boardt-squares board) (+ (/ vec 2) src))
                                   (list white-man white-king)))
                          (t nil)))
                   ((= p black-king)
                    (cond ((member vec all-steps) t)
                          ((member vec all-jumps)
                           (member (bref (boardt-squares board) (+ (/ vec 2) src))
                                   (list white-man white-king)))
                          (t nil)))
                   ((= p white-man)
                    (cond ((member vec white-steps) t)
                          ((member vec white-jumps)
                           (member (bref (boardt-squares board) (+ (/ vec 2) src))
                                   (list black-man black-king)))
                          (t nil)))
                   ((= p white-king)
                    (cond ((member vec all-steps) t)
                          ((member vec all-jumps)
                           (member (bref (boardt-squares board) (+ (/ vec 2) src))
                                   (list black-man black-king)))
                          (t nil)))
                   (t nil))))
      (and (member p (if (eql player 'black)
                         (list black-man black-king)
                         (list white-man white-king)))
           (eql d empty)
           (if piece (eql src piece) t)
           (if type (member vec type) t)
           (test-vec p vec)))))

(defun legal-piece-moves (pos player board &key (type (append all-steps all-jumps)))
  "Return legal moves of TYPE for PLAYER for piece at POS on BOARD."
  (let ((result ()))
    (dolist (vec type result)
      (let ((dst (+ pos vec))) ; re-write with remove-if
        (when (and (< 11 dst 88)
                   (legal-p (cons pos vec) player board))
          (push vec result))))))

;; slower for some reason?!?
;; (defun legal-moves-maybe (player board &key piece (type (append all-steps all-jumps)))
;;   "Return all potential legal moves for PLAYER on BOARD. If PIECE or TYPE
;;   is spcified, return moves only for that piece and only of that type."
;;   (let ((result ())
;;         (player-piece-type (if (eql player 'black)
;;                                (list black-man black-king)
;;                                (list white-man white-king))))
;;     (if piece
;;         (mapcar #'(lambda (vec) (cons piece vec))
;;                       (legal-piece-moves piece player board :type type))
;;         (dolist (pos-piece (player-pieces player board) result)
;;           (let ((pos (car pos-piece)))
;;                 (setf result
;;                       (append (mapcar #'(lambda (vec) (cons pos vec))
;;                                       (legal-piece-moves pos player board
;;                                                          :type type)) result)))))))

(defun legal-moves-maybe (player board &key piece (type (append all-steps all-jumps)))
  "Return all potential legal moves for PLAYER on BOARD. If PIECE or TYPE
  is spcified, return moves only for that piece and only of that type."
  (let ((result ())
        (player-piece-type (if (eql player 'black)
                               (list black-man black-king)
                               (list white-man white-king))))
    (if piece
        (mapcar #'(lambda (vec) (cons piece vec))
                      (legal-piece-moves piece player board :type type))
        (dolist (square black-squares result)
          (when (member (bref (boardt-squares board) square) player-piece-type)
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

(defun make-move (player move board)
  "Update BOARD to reflect move by player. Assume MOVE is
  legal. Return the updated board and T if a piece was kinged, else
  NIL."
  (let* ((src (car move))
         (vec (cdr move))
         (dst (+ src vec))
         (piece (bref (boardt-squares board) src)))
    (multiple-value-bind (res-piece kingedp) (king-maybe (bref (boardt-squares board) src) dst)
      (setf (bref (boardt-squares board) dst) res-piece)
      (setf (bref (boardt-squares board) src) empty)

      ;; remove src from pos-piece table
      (if (eql player 'black)
          (remhash src (boardt-black-pos-piece-table board))
          (remhash src (boardt-white-pos-piece-table board)))

      ;; add dst to pos-piece table
      (if (eql player 'black)
          (setf (gethash dst (boardt-black-pos-piece-table board)) (cons dst res-piece))
          (setf (gethash dst (boardt-white-pos-piece-table board)) (cons dst res-piece)))

      (if (member vec all-jumps)
          (let* ((jumped-pos (+ (/ vec 2) src))
                 (captured-piece (bref (boardt-squares board) jumped-pos)))
            (setf (bref (boardt-squares board) jumped-pos) empty)

            ;; remove captured piece from pos-piece table
            (if (eql player 'black)
                      (remhash jumped-pos (boardt-white-pos-piece-table board))
                      (remhash jumped-pos (boardt-black-pos-piece-table board)))))

      (if kingedp
          (progn
            (if (eql player 'black)
                (incf (boardt-num-black-kings board))
                (incf (boardt-num-white-kings board)))
            (if (eql player 'black)
                (decf (boardt-num-black-men board))
                (decf (boardt-num-white-men board)))))
      (values board kingedp))))

(defun get-move (strategy player board &key piece (type (append all-steps all-jumps)))
  "Call PLAYER's STRATEGY to get a move. PIECE, if provided, specifies
  the piece that must be moved. TYPE is a list of moves to consider (and
  check for legality). Keep calling until a legal move is made. Return
  NIL if there are no valid movies."
  (let* ((moves (legal-moves player board :piece piece :type type))
         (move (if moves (funcall strategy player (copy-board board)
                                  :piece piece :type type)))
         (command (if (listp move)
                      (symb (car move) '-command))))
    (if (null moves)
        nil
        (if (member command *command-list*)
            (progn
              (funcall command :board board)
              (force-output *standard-output*)
              (get-move strategy player board :piece piece :type type))
            (if (member move moves :test #'equal)
                move
                (progn
                  (warn "Illegal move or unknown command: ~a~%" move)
                  (get-move strategy player board :piece piece :type type)))))))

(defun checkers (black-strategy white-strategy
                 &key (player-to-move 'black) (board (initial-board)) (print t))
  "Play a game of checkers. Return the victor (though not in a vector). "
  (print-board board)
  (do* ((prev-player nil player)
        (player player-to-move (next-player player move board kingedp))
        (strategy (if (eql player 'black) black-strategy white-strategy)
                  (if (eql player 'black) black-strategy white-strategy))
        (move (get-move strategy player board)
              (apply #'get-move strategy player board
                     (if (eql player prev-player)
                         (progn (format t "DOUBLE JUMP~%")
                                (force-output *standard-output*)
                                (list :piece (+ (car move) (cdr move))
                                      :type all-jumps)))))
        (kingedp (when move
                   (multiple-value-bind (board kingedp)
                       (make-move player move board)
                     (declare (ignore board))
                     kingedp))
                 (when move
                   (multiple-value-bind (board kingedp)
                       (make-move player move board)
                     (declare (ignore board))
                     kingedp))))
       ((null move)
        (progn
          (if print (format t "~%~a wins!~%" (opponent player)))
          (opponent player)))
    (when (and move print)
      (format t "Move ~a made by player ~a.~%Updated board:~%" move player)
      (print-board board))))
