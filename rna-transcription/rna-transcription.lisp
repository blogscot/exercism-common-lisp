(defpackage :rna-transcription
  (:use :cl)
  (:export :to-rna))
(in-package :rna-transcription)

(defvar *table* (make-hash-table))
(setf (gethash #\G *table*) #\C)
(setf (gethash #\C *table*) #\G)
(setf (gethash #\T *table*) #\A)
(setf (gethash #\A *table*) #\U)

(defun to-rna (str)
  "Transcribe a string representing DNA nucleotides to RNA."
  (map 'string (lambda (x) (gethash x *table*)) str))
