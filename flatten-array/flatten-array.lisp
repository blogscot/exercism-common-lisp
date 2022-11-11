(defpackage :flatten-array
  (:use :cl)
  (:export :flatten))

(in-package :flatten-array)

(defun flatten (nested)
  (mapcan (lambda (x) (if (listp x)
                          (flatten x)
                          (list x))) nested))
