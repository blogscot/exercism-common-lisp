(defpackage :space-age
  (:use :cl)
  (:export :on-mercury
           :on-venus
           :on-earth
           :on-mars
           :on-jupiter
           :on-saturn
           :on-uranus
           :on-neptune))

(in-package :space-age)

(defun on-mercury (seconds) (/ (on-earth seconds) 0.2408467))
(defun on-venus   (seconds) (/ (on-earth seconds) 0.61519726))
(defun on-earth   (seconds) (/ seconds 31557600))
(defun on-mars    (seconds) (/ (on-earth seconds) 1.8808158))
(defun on-jupiter (seconds) (/ (on-earth seconds) 11.862615))
(defun on-saturn  (seconds) (/ (on-earth seconds) 29.447498))
(defun on-uranus  (seconds) (/ (on-earth seconds) 84.016846))
(defun on-neptune (seconds) (/ (on-earth seconds) 164.79132))
