(defpackage :lillys-lasagna
  (:use :cl)
  (:export :expected-time-in-oven
           :remaining-minutes-in-oven
           :preparation-time-in-minutes
           :elapsed-time-in-minutes))

(in-package :lillys-lasagna)

(defun expected-time-in-oven ()
  "Expect oven time in minutes"
  337)

(defun remaining-minutes-in-oven (minutes-in-oven)
  "Remaining oven time in minutes"
  (- (expected-time-in-oven) minutes-in-oven))

(defun preparation-time-in-minutes (number-of-layers)
  "Preparation time in minutes"
  (* number-of-layers 19))

(defun elapsed-time-in-minutes (number-of-layers minutes-in-oven)
  "Elapsed time in minutes"
  (+ (preparation-time-in-minutes number-of-layers) minutes-in-oven))
