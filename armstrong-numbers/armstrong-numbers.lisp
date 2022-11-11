(defpackage :armstrong-numbers
  (:use :cl)
  (:export :armstrong-number-p))
(in-package :armstrong-numbers)

(defun armstrong-number-p (number)
  (let* ((digits (map 'list #'digit-char-p (write-to-string number)))
         (len (length digits)))
    (= number (reduce (lambda (acc d) (+ acc (expt d len))) digits :initial-value 0))))
