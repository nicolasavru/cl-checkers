(ql:quickload "checkers")

(in-package checkers)

(setf *random-state* (make-random-state t))
(defparameter b (initial-board))
(print-board b)

(defparameter *str* (foreign-string-alloc (make-string 256 :initial-element #\null)))

(db-init 768 *str*)
(foreign-string-to-lisp *str*)


(require :sb-sprof)
(sb-sprof:with-profiling (:max-samples 10000
                               :report :flat
                               :loop nil)
  (checkers (alpha-beta-iterative-deepening-searcher 1 #'aggregate-eval-fun)
            (alpha-beta-iterative-deepening-searcher 1 #'simple-eval-fun)))
