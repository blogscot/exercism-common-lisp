(defpackage :phone-number
  (:use :cl)
  (:export :clean))

(in-package :phone-number)

(defun verify (number)
  (flet ((valid-code (number position) (char> (aref number position) #\1)))
    (if (and (= 10 (length number)) (valid-code number 0) (valid-code number 3))
        number
        "0000000000")))

(defun clean (phrase)
  "Converts a PHRASE string into a string of digits.
   Will evaluate to \"0000000000\" in case of an invalid input."
  (flet ((sanitise (phrase)
           (remove-if-not #'digit-char-p phrase)))
    (let ((number (sanitise phrase)))
      (cond
        ((and (= 11 (length number)) (char= #\1 (aref number 0)) (verify (subseq number 1))))
        (t (verify number))))))
