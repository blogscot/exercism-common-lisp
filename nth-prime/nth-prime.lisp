(defpackage :nth-prime
  (:use :cl)
  (:export :find-prime))

(in-package :nth-prime)

(defun find-prime (number)
  (when (plusp number)
   (loop for n from 2
         when (primep n) sum 1 into count
         when (= number count) return n)))

(defun primep (num)
  (cond
    ((= num 2) t)
    ((evenp num) nil)
    (t (loop for n from 3 to (sqrt num) by 2
             never (zerop (mod num n))))))
