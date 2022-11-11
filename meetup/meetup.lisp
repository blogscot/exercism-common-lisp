(defpackage :meetup
  (:use :cl)
  (:export :meetup))

(in-package :meetup)

(defun cycle (items)
  (setf (cdr (last items)) items))

(defparameter *days-of-week* (cycle '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday)))

(defun leap-year-p (year)
  (cond
    ((zerop (mod year 400)) t)
    ((zerop (mod year 100)) nil)
    ((zerop (mod year 4))   t)
    (t                      nil)))

(defun days-in-month (month year)
  (let* ((month-days '(31 28 31 30 31 30 31 31 30 31 30 31))
         (days (nth (1- month) month-days)))
    (if (leap-year-p year)
        (1+ days)
        days)))

(defun day-of-week (date month year)
  (nth-value 6 (decode-universal-time
                (encode-universal-time 0 0 0 date month year))))

(defun meetup (month year dow schedule)
  "Returns a date in the format (y m d) for a given meetup date."
  (let* ((calendar (loop for n from 1 to (days-in-month month year)
                         collect
                         (cons (nth (day-of-week n month year) *days-of-week*) n)))
         (days (remove-if-not (lambda (x) (eq dow (car x))) calendar)))
    (list year month
          (case schedule
            (:first  (cdr (first days)))
            (:second (cdr (second days)))
            (:third  (cdr (third days)))
            (:fourth (cdr (fourth days)))
            (:last   (cdr (first (last days))))
            (:teenth (cdr (find-if (lambda (x) (>= (cdr x) 13)) days)))
            (otherwise "Invalid schedule")))))
