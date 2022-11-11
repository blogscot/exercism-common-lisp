(defpackage :logans-numeric-partition
  (:use :cl)
  (:export :categorize-number :partition-numbers))

(in-package :logans-numeric-partition)

(defun categorize-number (odds-evens number)
  (if (evenp number)
      (cons (car odds-evens) (cons number (cdr odds-evens)))
      (cons (cons number (car odds-evens)) (cdr odds-evens))))

(defun partition-numbers (xs)
  (reduce #'categorize-number xs :initial-value '(() . ())))
