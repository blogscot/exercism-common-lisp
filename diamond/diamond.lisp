(defpackage :diamond
  (:use :cl)
  (:import-from :uiop :strcat)
  (:export :rows))

(in-package :diamond)

(defparameter *alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun rows (letter)
  (let* ((pos (position letter *alphabet*))
         (letters (subseq *alphabet* 0 (1+ pos)))
         (line (strcat letters (subseq (reverse letters) 1))))
    (loop for i from 0 to pos
          collect (loop for ch across line
                        if (eql ch (char line i))
                          collect ch
                        else
                          collect " ") into lines
          finally (return (apply #'mapcar #'strcat
                                 (append (reverse lines) (rest lines)))))))
