(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (table)
  "Transforms hash values into keys with their keys as their values."
  (let ((ht (make-hash-table)))
    (loop for k being each hash-key of table
          do (loop for v in (gethash k table)
                   do (setf (gethash (char-downcase v) ht) k)))
    ht))
