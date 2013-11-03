(in-package #:checkers)

(defvar *command-list*)

(defun help-command (&rest rest)
  "Print this help message."
  (declare (ignore rest))
  (format t "~a~%"
          (mapcar (lambda (command)
                    (let ((s (symbol-name command)))
                      (subseq s 0 (search "-command" s :test #'equalp))))
                  *command-list*)))

(defun print-board-command (&rest rest &key board)
  "Print the board."
  (declare (ignore rest))
  (print-board board))

(defun dump-board-command (&rest rest &key board)
  "Print the board."
  (declare (ignore rest))
  (format t "~a~%" board))

(defun exit-command (&rest rest)
  "Exit cl-checkers."
  (declare (ignore rest))
  (sb-ext:exit))

(setf *command-list* (apropos-list "-command" *package*))
