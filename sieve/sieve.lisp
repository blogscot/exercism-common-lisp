(defpackage :sieve
  (:use :cl)
  (:export :primes-to)
  (:documentation "Generates a list of primes up to a given limit."))

(in-package :sieve)

;; A minor variation on jinwoo's solution
(defun primes-to (n)
  (let ((candidates (make-array (1+ n) :element-type 'boolean :initial-element t)))
    (loop for i from 2 to (sqrt n)
          when (aref candidates i)
            do (loop for j from (+ i i) to n by i
                     do (setf (aref candidates j) nil)))
    (loop for i from 2 to n
          when (aref candidates i)
            collect i)))
