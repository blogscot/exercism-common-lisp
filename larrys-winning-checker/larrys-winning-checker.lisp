(defpackage :larrys-winning-checker
  (:use :cl)
  (:export
   :make-empty-board
   :make-board-from-list
   :all-the-same-p
   :row
   :column))

(in-package :larrys-winning-checker)

(defun make-empty-board ()
  (make-array '(3 3) :initial-element nil))

(defun make-board-from-list (list)
  (make-array '(3 3) :initial-contents list))

(defun all-the-same-p (row-or-col)
  (every (lambda (x) (eq x (aref row-or-col 0))) row-or-col))

(defun row (board row-num)
  (coerce (loop for i from 0 below 3
                collect (aref board row-num i)) 'vector))

(defun column (board col-num)
  (coerce (loop for i from 0 below 3
                collect (aref board i col-num)) 'vector))
