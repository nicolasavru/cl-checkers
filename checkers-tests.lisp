(ql:quickload :lisp-unit)

(in-package :checkers)

(define-test opponenet-test1
  (assert-true (eql 'white (opponent 'black))))

(define-test opponenet-test2
  (assert-false (eql 'white (opponent 'white))))

(run-tests)
