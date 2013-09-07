;;;; checkers.asd

(asdf:defsystem #:checkers
  :serial t
  :description "Describe checkers here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lisp-unit #:split-sequence)
  :components ((:file "package")
               (:file "utils")
               (:file "checkers")
               (:file "strategies")
               (:file "checkers-tests")))

