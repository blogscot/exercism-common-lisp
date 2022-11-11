(defpackage :largest-series-product
  (:use :cl)
  (:export :largest-product))

(in-package :largest-series-product)

(defun largest-product (digits span)
  (cond
    ((< span 0) nil)
    ((> span (length digits)) nil)
    ((notevery #'digit-char-p digits) nil)
    (t (loop for i from 0 to (- (length digits) span)
             maximizing (product (subseq digits i (+ i span)))))))

(defun product (digits)
 (loop for ch across digits
       collect (- (char-code ch) 48) into digits
       finally (return (apply #'* digits))))

