(defpackage :collatz-conjecture
  (:use :cl)
  (:export :collatz))

(in-package :collatz-conjecture)

(defun collatz (n)
  (labels ((helper (n count)
             (cond
               ((= 1 n) count)
               ((evenp n) (helper (/ n 2) (1+ count)))
               (t (helper (1+ (* n 3)) (1+ count))))))
    (when (plusp n)
      (helper n 0))))
