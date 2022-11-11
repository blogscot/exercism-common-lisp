(defpackage :allergies
  (:use :cl)
  (:shadow :list)
  (:export :allergic-to-p :list))

(in-package :allergies)

(defvar *allergies*
  '("eggs"
    "peanuts"
    "shellfish"
    "strawberries"
    "tomatoes"
    "chocolate"
    "pollen"
    "cats"))

(defun allergic-to-p (score allergen)
  "Returns true if given allergy score includes given allergen."
  (let ((bit (position allergen *allergies* :test 'equal)))
    (logbitp bit score)))

(defun list (score)
  "Returns a list of allergens for a given allergy score."
  (loop for bit from 0 to 7
        when (logbitp bit score)
          collect (nth bit *allergies*)))
