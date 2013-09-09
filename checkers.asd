;;;; checkers.asd

(asdf:defsystem #:checkers
  :serial t
  :description "A checkers game and AI written in Common Lisp."
  :author "Nicolas Avrutin <nicolasavru@gmail.com>"
  :license "Specify license here"
  :depends-on (#:lisp-unit
               #:split-sequence)
  :components ((:file "package")
               (:file "utils")
               (:file "checkers")
               (:file "strategies")
               (:file "checkers-tests")))

