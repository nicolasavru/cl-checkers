(in-package #:checkers)

(defun random-elt (lst)
  (let ((len (length lst)))
    (if (> len 0)
        (elt lst (random len)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))


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
