(defpackage :sum-of-multiples
  (:use :cl)
  (:export :sum))

(in-package :sum-of-multiples)

(defun sum (factors limit)
  (apply #'+ (reduce (lambda (acc lst) (union acc lst))
                     (loop for factor in factors
                           when (plusp factor)
                             collect
                             (loop for n from factor below limit by factor
                                   collect n)) :initial-value '())))
