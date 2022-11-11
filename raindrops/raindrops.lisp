(defpackage :raindrops
  (:use :cl)
  (:export :convert))

(in-package :raindrops)

(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let ((raindrops (format nil "~[Pling~:;~]~[Plang~:;~]~[Plong~:;~]"
                           (mod n 3) (mod n 5) (mod n 7))))
    (if (uiop:emptyp raindrops)
        (write-to-string n)
        raindrops)))
