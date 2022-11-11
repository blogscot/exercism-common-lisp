(defpackage :perfect-numbers
  (:use :cl)
  (:export :classify))

(in-package :perfect-numbers)

(defun factors (n)
  "Calculates the factors of n including n itself"
  (loop for i from 1 to (sqrt n)
        for j = (/ n i)
        when (zerop (mod n i))
          collecting i and
        when (/= i j)
          collecting j))

(defun classify (n)
  "Classifies the aliquot sum of n"
  (when (plusp n)
    (let ((total (- (apply #'+ (factors n)) n)))
     (cond
       ((= total n) "perfect")
       ((< total n) "deficient")
       (t           "abundant")))))
