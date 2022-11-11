(defpackage :proverb
  (:use :cl :uiop)
  (:export :recite))

(in-package :proverb)

(defun recite (strings)
  (format nil "~{~a~^~&~}"
          (loop for (a b) on strings
                if b
                  collect (format nil "For want of a ~a the ~a was lost." a b)
                else
                  collect (format nil "And all for the want of a ~a." (first strings)))))
