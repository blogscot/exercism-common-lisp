(defpackage :robot-simulator
  (:use :cl)
  (:export :+north+ :+east+ :+south+ :+west+ :execute-sequence
           :robot :robot-position :robot-bearing :make-robot))

(in-package :robot-simulator)

(defconstant +north+ 0)
(defconstant +east+  1)
(defconstant +south+ 2)
(defconstant +west+  3)

(defstruct robot
  (position '(0 . 0))
  (bearing 0))

(defun advance (robot)
  (case (robot-bearing robot)
    (0 (incf (cdr (robot-position robot))))
    (2 (decf (cdr (robot-position robot))))
    (1 (incf (car (robot-position robot))))
    (3 (decf (car (robot-position robot))))))

(defun execute-order (robot order)
  (case order
    (#\R (setf (robot-bearing robot) (mod (1+ (robot-bearing robot)) 4)))
    (#\L (setf (robot-bearing robot) (mod (1- (robot-bearing robot)) 4)))
    (#\A (advance robot))))

(defun execute-sequence (robot seq)
  (loop for order across seq
        do (execute-order robot order)))
