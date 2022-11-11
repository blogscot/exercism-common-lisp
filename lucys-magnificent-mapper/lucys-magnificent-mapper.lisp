(defpackage :lucys-magnificent-mapper
  (:use :cl)
  (:export :make-magnificent-maybe :only-the-best))

(in-package :lucys-magnificent-mapper)

(defun make-magnificent-maybe (f list)
  (mapcar f list))

(defun only-the-best (f list)
  (remove-if f (remove 1 list)))
