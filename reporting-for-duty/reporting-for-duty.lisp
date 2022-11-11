(defpackage :reporting-for-duty
  (:use :cl)
  (:export :format-quarter-value :format-two-quarters
           :format-two-quarters-for-reading))

(in-package :reporting-for-duty)

(defun format-quarter-value (quarter value)
  (format nil "The value ~a quarter: ~a" quarter value))

(defun format-two-quarters (out quarter1 value1 quarter2 value2)
  (format out "~%~a~%~a~%"
          (format-quarter-value quarter1 value1)
          (format-quarter-value quarter2 value2)))

(defun format-two-quarters-for-reading (out quarter1 value1 quarter2 value2)
  (format out "(~s ~s)"
          (format-quarter-value quarter1 value1)
          (format-quarter-value quarter2 value2)))
