(defpackage :prime-factors
  (:use :cl)
  (:export :factors))

(in-package :prime-factors)

(defun factors* (start end)
  (loop for i from start to end
        when (= i end)
          return (list end)
        when (zerop (mod end i))
          return (cons i (factors* i (/ end i)))))

(defun factors (n)
  (factors* 2 n))
