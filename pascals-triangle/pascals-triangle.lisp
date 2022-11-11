(defpackage :pascals-triangle
  (:use :cl)
  (:export :rows))

(in-package :pascals-triangle)

(defun transform (list)
  (let ((new-list (cons 0 list)))
    (mapcar #'+ new-list (reverse new-list))))

(defun rows (n &optional (row '(1)))
  (when (plusp n)
    (cons row (rows (1- n) (transform row)))))
