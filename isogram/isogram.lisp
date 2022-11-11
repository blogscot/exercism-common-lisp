(defpackage :isogram
  (:use :cl)
  (:export :isogram-p))

(in-package :isogram)

(defun isogram-p (string)
  "Is string an Isogram?"
  (let* ((letters (remove-if-not #'alpha-char-p string))
         (unique-letters (remove-duplicates letters :test #'char-equal)))
    (equal unique-letters letters)))
