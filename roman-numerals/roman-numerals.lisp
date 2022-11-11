(defpackage :roman-numerals
  (:use :cl)
  (:export :romanize))

(in-package :roman-numerals)

(defvar *numerals* '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
                     (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
                     (10 . "X") (9 . "IX") (5 . "V") (4 . "IV")
                     (1 . "I")))

(defun romanize (number)
  (apply #'concatenate 'string
         (loop for n = number then m
               while (> n 0)
               for pair = (find-if (lambda (x) (<= x n)) *numerals* :key 'car)
               for m = (- n (car pair))
               collecting (cdr pair))))

;; (defun romanize (number)
;;   "Returns the Roman numeral representation for a given number."
;;   (format nil "~@r" number))
