(ql:quickload "checkers")
(in-package checkers)

(setf *random-state* (make-random-state t))

(defparameter *str* (foreign-string-alloc (make-string 256 :initial-element #\null)))

(db-init 768 *str*)
;; (foreign-string-to-lisp *str*)

(format t "~%~%")

(defun computer-strategy (time)
  (alpha-beta-iterative-deepening-searcher time #'aggregate-eval-fun))

(defun computer-strategy2 (time)
  (alpha-beta-iterative-deepening-searcher time #'simple-eval-fun))

(defun human-wrapper (time)
  (declare (ignore time))
  #'human)

;; configuration
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

(defparameter *black-computer-p* (y-or-n-p "Is black player a computer?"))
(defparameter *white-computer-p* (y-or-n-p "Is white player a computer?"))

(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                               :report :flat
                               :loop nil)
(if (y-or-n-p "Load a game?")
    (let ((fname))
      (format *query-io* "Enter the board file: ")
      (force-output *query-io*)
      (setf fname (read *query-io*))
      (load-game fname
                 (if *black-computer-p* #'computer-strategy #'human-wrapper)
                 (if *white-computer-p* #'computer-strategy #'human-wrapper)))

    (let ((time))
      (format *query-io* "Enter per-move time limit (in seconds) for computer: ")
      (force-output *query-io*)
      (setf time (read *query-io*))
      (checkers
       (if *black-computer-p* (computer-strategy time) #'human)
       (if *white-computer-p* (computer-strategy time) #'human)
       )))
)


;; (checkers (alpha-beta-iterative-deepening-searcher 1 #'aggregate-eval-fun)
;;           (alpha-beta-iterative-deepening-searcher 1 #'simple-eval-fun))

(sb-ext:exit)
