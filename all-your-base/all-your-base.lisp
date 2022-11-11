(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

(defun to-decimal (digits in-base)
  (reduce (lambda (acc val) (+ (* in-base acc) val)) digits :initial-value 0))

(defun to-base (number out-base)
  (if (zerop number)
      '()
      (cons (mod number out-base)
            (to-base (floor (/ number out-base)) out-base))))

(defun rebase (list-digits in-base out-base)
  (cond
    ((or (< in-base 2) (< out-base 2)) nil)
    ((some (lambda (x) (or (< x 0) (>= x in-base))) list-digits) nil)
    ((not list-digits) '(0))
    ((every #'zerop list-digits) '(0))
    (t (reverse (to-base (to-decimal list-digits in-base) out-base)))))
