(in-package #:checkers)

(define-foreign-library (libdblookup :search-path ".")
  (t (:default "libdblookup")))

(use-foreign-library libdblookup)

(defcfun "db_init" :int
  (dbmegabtes :int)
  (progress-string :pointer))

(defcfun "db_infostring" :void
  (info-string :pointer))

(defcstruct position
  "Cake endgame database position structure."
  (bm :unsigned-int)
  (bk :unsigned-int)
  (wm :unsigned-int)
  (wk :unsigned-int)
  (color :int))

(defcfun "dblookup" :int
  (pos :pointer)
  (conditional-lookup :int))
