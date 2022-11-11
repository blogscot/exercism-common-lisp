(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

(defun make-school ()
  (make-hash-table :test 'equal))

(defun add (school name grade)
  (unless (member name (roster school) :test 'equal)
    (push name (gethash grade school))))

(defun grade (school grade)
  (sort (gethash grade school) 'string-lessp))

(defun roster (school)
  (let ((grades (loop for k being the hash-key of school collect k)))
    (apply #'concatenate 'list (loop for grade in (sort grades #'<)
           collect (grade school grade)))))
