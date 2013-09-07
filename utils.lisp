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

