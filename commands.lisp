(in-package #:checkers)

(defvar *command-list*)

(defun help-command (&rest rest)
  (declare (ignore rest))
  "Print this help message."
  (format t "~a~%"
          (mapcar (lambda (command)
                    (let ((s (symbol-name command)))
                      (subseq s 0 (search "-command" s :test #'equalp))))
                  *command-list*)))

(defun print-board-command (&rest rest &key board )
  (declare (ignore rest))
  "Print the board."
  (print-board board))

(defun dump-board-command (&rest rest &key board )
  (declare (ignore rest))
  "Print the board."
  (format t "~a~%" board))

(setf *command-list* (apropos-list "-command" *package*))
