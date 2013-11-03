(in-package #:checkers)

;; (defun random-elt (lst)
;;   (let ((len (length lst)))
;;     (if (> len 0)
;;         (elt lst (random len)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro inner-slot (object &rest slot-names)
  "Return an inner slot of a nested structure OBJET:
   (inner-slot foo 'bar 'baz) -> (slot-value (slot-value foo 'bar) 'baz)"
  (labels ((inner-slot-fun (object rev-slot-names)
             (if (null rev-slot-names)
                 object
                 `(slot-value ,(inner-slot-fun object (cdr rev-slot-names)) ,(car rev-slot-names)))))
    (inner-slot-fun object (reverse slot-names))))

(defparameter esc-str (string #\Escape) "shell escape string")
(defparameter rst-str (concatenate 'string esc-str "[0m") "shell color reset string")
(defparameter ansi-colors (list
                           :black
                           :red
                           :green
                           :yellow
                           :blue
                           :magenta
                           :cyan
                           :white))

(defun color-code (color)
  "Return position of COLOR in ANSI-COLORS list."
  (position color ansi-colors))

(defun color-string (str &key (fg nil) (bg nil) (bold nil bold-p))
  "Return a string which, when printed to a terminal, will have a
  foreground folor of FG, a background color of BG, and a bold
  attribute of BOLD."
  (concatenate 'string
               esc-str
               (format nil "[~A~A~A~Am"
                       (if fg (+ 30 (color-code fg)) "")
                       (if (and fg bg) ";" "")
                       (if bg (+ 40 (color-code bg)) "")
                       (if bold-p ";1" ""))
               str
               rst-str))

(defun scale-text (line &key (xscale 2) (yscale 2) (line-sep (string #\newline)) chars)
  "Replicate characters of LINE by XSCALE and YSCALE if they are in
  CHARS using LINE-SEP to separate each line."
  (let* ((dstr
           (apply #'concatenate 'string 
                  (map 'list #'(lambda (x)
                                 (let ((outstr ""))
                                   (dotimes (i
                                             (if (member x chars)
                                                 xscale
                                                 1))
                                     (setf outstr
                                           (concatenate 'string outstr (string x))))
                                   outstr))
                       line)))
          (outstr dstr))
    (dotimes (i (1- yscale))
      (setf outstr (concatenate 'string outstr line-sep dstr)))
    outstr))
