(defpackage :acronym
  (:use :cl)
  (:export :acronym))

(in-package :acronym)

(defun acronym (str)
  "Returns the acronym for a noun of tech jargon."
  (remove-if-not #'upper-case-p (format nil "~:(~a~)" str)))
