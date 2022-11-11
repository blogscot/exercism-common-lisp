(defpackage :darts
  (:use :cl)
  (:export :score))

(in-package :darts)

(defun score (x y)
  (let ((distance (sqrt (+ (* x x) (* y y)))))
    (cond
      ((<= distance 1) 10)
      ((<= distance 5) 5)
      ((<= distance 10) 1)
      (t 0))))
