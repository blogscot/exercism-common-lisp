(defpackage :queen-attack
  (:use :cl)
  (:export :valid-position-p
           :attackp))

(in-package :queen-attack)

(defun valid-position-p (coordinates)
  (destructuring-bind (row . col) coordinates
    (and (<= 0 row 7) (<= 0 col 7))))

(defun attackp (white-queen black-queen)
  (destructuring-bind (white-row . white-col) white-queen
    (destructuring-bind (black-row . black-col) black-queen
      (let ((delta-row (abs (- white-row black-row)))
            (delta-col (abs (- white-col black-col))))
        (cond
          ((= white-col black-col) t)
          ((= white-row black-row) t)
          ((= delta-row delta-col) t))))))
