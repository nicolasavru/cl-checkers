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
(defconstant black-king 2)
(defconstant white-man 3)
(defconstant white-king 4)
(defconstant outer 5)

(defconstant white 1)
(defconstant black 2)

(deftype piece () `(integer ,empty ,outer))

(defstruct boardt
  (squares (make-array 100 :element-type 'piece :initial-element outer) :type (vector piece))
  (num-black-men 12 :type fixnum)
  (num-black-kings 0 :type fixnum)
  (num-white-men 12 :type fixnum)
  (num-white-kings 0 :type fixnum)
  (black-pos-piece-alist () :type list)
  (white-pos-piece-alist () :type list)
  (bm 0 :type integer)
  (bk 0 :type integer)
  (wm 0 :type integer)
  (wk 0 :type integer)
  (color black :type integer))

(defparameter piece-alist-list
  (list
   '((symb . empty)      (str . "."))
   '((symb . black-man)  (str . "b") (fg . :red))
   '((symb . black-king) (str . "B") (fg . :red))
   '((symb . white-man)  (str . "w") (fg . :white))
   '((symb . white-king) (str . "W") (fg . :white))
   '((symb . outer)      (str . "O")))
  "List of alists containing properties for each piece type.")

(defun alist-of (piece)
  "Return alist associated with PIECE."
  (nth piece piece-alist-list))

(defun symb-of (piece)
  "Return a symbol representing PIECE."
  (cdr (assoc 'symb (alist-of piece))))

(defun string-of (piece)
  "Return a string representing PIECE."
  (let ((piece-alist (alist-of piece)))
    (color-string (cdr (assoc 'str piece-alist))
                  :fg (cdr (assoc 'fg piece-alist)))))

(defun opponent (player) (if (eql player 'black) 'white 'black))

(defun next-player (player move board &optional kingedp)
  "Select next player to move given the current PLAYER, his previous
  MOVE, the BOARD, and whether a piece was kinged during MOVE."
  (if (and (member (cdr move) all-jumps)
           (legal-moves player board :piece (+ (car move) (cdr move)) :type all-jumps)
           (not kingedp))
      player
      (opponent player)))

(deftype boardarr () '(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defun copy-squares (squares)
  (copy-seq squares))

(defun copy-board (board)
  (let ((new-board (copy-boardt board)))
    (setf (boardt-squares new-board) (copy-squares (boardt-squares board)))
    new-board))

(defun square-to-cake (square)
  (multiple-value-bind (row col)
      (floor square 10)
    (+ (* 4 (- row 1))
       (if (oddp row)
           (/ (- 8 col) 2)
           (/ (- 8 (+ col 1)) 2)))))

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

;; (defun player-pieces (player board)
;;   "Return a list of (POS . PIECE) cons cells."
;;   (do ((len (length (boardt-squares board)))
;;        (pos 0 (1+ pos))
;;        (res))
;;       ((= pos len) res)
;;     (let ((piece (bref (boardt-squares board) pos)))
;;       ;; (if (typep piece (symb player '-piece))
;;       (if (member piece (if (eql player 'black)
;;                             (list black-man black-king)
;;                             (list white-man white-king)))
;;           (push (cons pos piece) res)))))

(defun player-pieces (player board)
  "Return a list of (POS . PIECE) cons cells."
  (if (eql player 'black)
      (boardt-black-pos-piece-alist board)
      (boardt-white-pos-piece-alist board))
  )

(defun initial-board ()
  "Return a board with initial positions of both players"
  ;; Boards are 100-element vectors with elements
  ;; (<= 1 (mod 11-88 10) 8) used and the others marked with the sentinel OUTER.
  (let* ((board (make-boardt))
         (black-starting-squares
           (subseq black-squares 0 12))
         (white-starting-squares
           (subseq black-squares (- (length black-squares) 12))))
    (dolist (square all-squares)
      (setf (bref (boardt-squares board) square) empty))
    (dolist (square black-starting-squares)
      (setf (bref (boardt-squares board) square) black-man)
      (setf (boardt-black-pos-piece-alist board)
            (acons square black-man (boardt-black-pos-piece-alist board)))
      (setf (boardt-bm board) (logior (boardt-bm board) (ash 1 (square-to-cake square)))))
    (dolist (square white-starting-squares)
      (setf (bref (boardt-squares board) square) white-man)
      (setf (boardt-white-pos-piece-alist board)
            (acons square white-man (boardt-white-pos-piece-alist board)))
      (setf (boardt-wm board) (logior (boardt-wm board) (ash 1 (square-to-cake square)))))
    board))

;; (defun load-board (filename)
;;   (let ((in (open filename))
;;         (input-squares)
;;         (i 0)
;;         (board (make-array 100 :element-type 'piece :initial-element outer)))
;;     (dotimes (row 8)
;;       (let ((line (string-trim '(#\Return) (read-line in))))
;;         (setf input-squares
;;               (nconc input-squares
;;                      (read-from-string (concatenate 'string "(" line ")"))))))
;;     (dolist (square all-squares)
;;       (setf (bref (boardt-squares board) square) empty))
;;     (dolist (square black-squares)
;;       (setf (bref (boardt-squares board) square) (elt input-squares i))
;;       (incf i))
;;     board))

(defun print-board (board)
  "Print a board."
  (format t "    ~{~d ~}" '(1 2 3 4 5 6 7 8))
  (dotimes (row 8)
    (format t "~& ~d " (* 10 (1+ row)))
    (dotimes (col 8)
      (format t "~A "  (string-of (bref (boardt-squares board) (+ (* 10 (1+ row)) (1+ col)))))))
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
         (d (bref (boardt-squares board) dst))
         )
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

;; (defmacro )

(defun make-move (player move board)
  "Update BOARD to reflect move by player. Assume MOVE is
  legal. Return the updated board and T if a piece was kinged, else
  NIL."
  (let* ((src (car move))
         (vec (cdr move))
         (dst (+ src vec))
         (piece (bref (boardt-squares board) src))
         (pos-piece-alist (if (eql player 'black)
                              (boardt-black-pos-piece-alist board)
                              (boardt-white-pos-piece-alist board))))
    (setf (boardt-color board) (symbol-value (opponent player)))
    (multiple-value-bind (res-piece kingedp) (king-maybe (bref (boardt-squares board) src) dst)
      (setf (bref (boardt-squares board) dst) res-piece)
      (setf (bref (boardt-squares board) src) empty)

      ;; remove src from pos-piece alist
      (if (eql player 'black)
          (setf (boardt-black-pos-piece-alist board)
                (remove src (boardt-black-pos-piece-alist board) :test #'= :key #'car))
          (setf (boardt-white-pos-piece-alist board)
                (remove src (boardt-white-pos-piece-alist board) :test #'= :key #'car)))

      ;; remove src from cake representation
      (cond ((= piece black-man)
             (setf (boardt-bm board) (logxor (boardt-bm board) (ash 1 (square-to-cake src)))))
            ((= piece black-king)
             (setf (boardt-bk board) (logxor (boardt-bk board) (ash 1 (square-to-cake src)))))
            ((= piece white-man)
             (setf (boardt-wm board) (logxor (boardt-wm board) (ash 1 (square-to-cake src)))))
            ((= piece white-king)
             (setf (boardt-wk board) (logxor (boardt-wk board) (ash 1 (square-to-cake src))))))

      ;; add dst to pos-piece alist
      (if (eql player 'black)
          (setf (boardt-black-pos-piece-alist board)
                (acons dst res-piece (boardt-black-pos-piece-alist board)))
          (setf (boardt-white-pos-piece-alist board)
                (acons dst res-piece (boardt-white-pos-piece-alist board))))

      ;; add dest to cake representation
      (if (or kingedp (= piece black-king) (= piece white-king))
          (if (eql player 'black)
              (setf (boardt-bk board) (logxor (boardt-bk board) (ash 1 (square-to-cake dst))))
              (setf (boardt-wk board) (logxor (boardt-wk board) (ash 1 (square-to-cake dst)))))
          (if (eql player 'black)
              (setf (boardt-bm board) (logxor (boardt-bm board) (ash 1 (square-to-cake dst))))
              (setf (boardt-wm board) (logxor (boardt-wm board) (ash 1 (square-to-cake dst))))))

      (if (member vec all-jumps)
          (let* ((jumped-pos (+ (/ vec 2) src))
                 (captured-piece (bref (boardt-squares board) jumped-pos)))
            (setf (bref (boardt-squares board) jumped-pos) empty)

            ;; remove captured piece from pos-piece alist
            (if (eql player 'black)
                (setf (boardt-white-pos-piece-alist board)
                      (remove jumped-pos (boardt-white-pos-piece-alist board) :test #'= :key #'car))
                (setf (boardt-black-pos-piece-alist board)
                      (remove jumped-pos (boardt-black-pos-piece-alist board) :test #'= :key #'car)))
            (cond ((= captured-piece black-man)
                   (setf (boardt-bm board) (logxor (boardt-bm board) (ash 1 (square-to-cake jumped-pos))))
                   (decf (boardt-num-black-men board)))
                  ((= captured-piece black-king)
                   (setf (boardt-bk board) (logxor (boardt-bk board) (ash 1 (square-to-cake jumped-pos))))
                   (decf (boardt-num-black-kings board)))
                  ((= captured-piece white-man)
                   (setf (boardt-wm board) (logxor (boardt-wm board) (ash 1 (square-to-cake jumped-pos))))
                   (decf (boardt-num-white-men board)))
                  ((= captured-piece white-king)
                   (setf (boardt-wk board) (logxor (boardt-wk board) (ash 1 (square-to-cake jumped-pos))))
                   (decf (boardt-num-white-kings board))))
            ))

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
         (move (if moves (funcall strategy player (copy-board board))))
         (command (symb (car move) '-command)))
    (if (null moves)
        nil
        (if (member command *command-list*)
            (progn
              (apply command :board (boardt-squares board) board (cdr move))
              (force-output *standard-output*)
              (get-move strategy player board :piece piece :type type))
            (if (member move moves :test #'equal)
                move
                (progn
                  ;; (print moves)
                  ;; (print (list move player board piece type))
                  (warn "Illegal move or unknown command: ~a~%Moves: ~a" move moves)
                  (get-move strategy player board :piece piece :type type)))))))

(defun checkers (black-strategy white-strategy
                 &key (player-to-move 'black) (board (initial-board)) (print t))
  "Play a game of checkers. Return the victor (though not in a vector). "
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
