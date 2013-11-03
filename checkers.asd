;;;; checkers.asd

(asdf:defsystem #:checkers
  :serial t
  :description "A checkers game and AI written in Common Lisp."
  :author "Nicolas Avrutin <nicolasavru@gmail.com>"
  :depends-on ()
  :components ((:file "package")
               (:file "utils")
               (:file "commands")
               (:file "checkers")
               (:file "strategies")))

