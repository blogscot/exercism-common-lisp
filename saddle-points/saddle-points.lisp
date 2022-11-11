(defpackage :saddle-points
  (:use :cl)
  (:export :saddle-points))

(in-package :saddle-points)

(defun get-row-max (matrix row)
  (loop for c from 0 below (array-dimension matrix 1)
        maximize (aref matrix row c)))

(defun get-column-min (matrix col)
  (loop for r from 0 below (array-dimension matrix 0)
        minimize (aref matrix r col)))

(defun saddle-points (matrix)
  (multiple-value-bind (num-rows num-cols) (values-list (array-dimensions matrix))
    (loop for r from 0 below num-rows
          append (loop for c from 0 below num-cols
                       when (and (= (aref matrix r c) (get-row-max matrix r))
                                 (= (aref matrix r c) (get-column-min matrix c)))
                         collect (list (1+ r) (1+ c))))))
