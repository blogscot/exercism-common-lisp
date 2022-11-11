(defpackage :lillys-lasagna-leftovers
  (:use :cl)
  (:export
   :preparation-time
   :remaining-minutes-in-oven
   :split-leftovers))

(in-package :lillys-lasagna-leftovers)

(defun preparation-time (&rest rest)
  (* 19 (length rest)))

(defun remaining-minutes-in-oven (&optional (duration :normal))
  (let ((minutes 337))
    (case duration
      (:very-long  (+ minutes 200))
      (:longer     (+ minutes 100))
      (:shorter    (- minutes 100))
      (:very-short (- minutes 200))
      (:normal     minutes)
      (otherwise   0))))

(defun split-leftovers (&key (weight nil weight-supplied-p) (human 10) (alien 10))
  (if weight
      (- weight human alien)
      (if weight-supplied-p
          :LOOKS-LIKE-SOMEONE-WAS-HUNGRY
          :JUST-SPLIT-IT )))
