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

;; 0: 18
;; 1: 16
;; 2: 14
;; 3: 12

;; 4: 27
;; 5: 25
;; 6: 23
;; 7: 21

;; 8: 38
;; 9: 36
;; 10: 34
;; 11: 32



;; (with-foreign-object (pos 'position)
;;   (setf (foreign-slot-value pos 'position 'bm) 9)
;;   (setf (foreign-slot-value pos 'position 'bk) 31)
;;   ;; (format t "db: ~b~%" (boardt-wm board))
;;   (setf (foreign-slot-value pos 'position 'wm) 18)
;;   (setf (foreign-slot-value pos 'position 'wk) 8)
;;   (setf (foreign-slot-value pos 'position 'color) 1)
;;   (dblookup pos 0))
