(defpackage :robot-name
  (:use :cl)
  (:export :build-robot :robot-name :reset-name))

(in-package :robot-name)

(defvar *robot-count* 0)
(defvar *registry* (make-hash-table :test 'equal))

(defun generate-letters (n)
  (coerce (loop repeat n collect (code-char (+ (char-code #\A) (random 26)))) 'string))

(defun generate-digits (n)
  (apply #'concatenate 'string
   (loop repeat n collect (write-to-string (random 10)))))

(defun generate-name (&optional (num-letters 2) (num-digits 3))
  (concatenate 'string (generate-letters num-letters) (generate-digits num-digits)))

(defun build-robot ()
  (let ((serial-number *robot-count*))
    (setf (gethash serial-number *registry*) (generate-name))
    (incf *robot-count*)
    serial-number))

(defun robot-name (robot)
  (gethash robot *registry*))

(defun reset-name (robot)
  (let ((new-name (generate-name))
        (robot-names (loop for name being the hash-values of *registry*
                           collect name)))
    (if (member new-name robot-names)
        (reset-name robot)
        (setf (gethash robot *registry*) new-name))))
