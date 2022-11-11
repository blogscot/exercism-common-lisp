(defpackage :gigasecond-anniversary
  (:use :cl)
  (:export :from))
(in-package :gigasecond-anniversary)

(defun from (year month day hour minute second)
  (multiple-value-bind (s m h d mon y)
      (decode-universal-time
       (+ 1000000000
          (encode-universal-time second minute hour day month year 0)) 0)
    (list y mon d h m s)))
