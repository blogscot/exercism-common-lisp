(defpackage :crypto-square
  (:use :cl)
  (:export :encipher))

(in-package :crypto-square)

(defun encipher (plaintext)
  (if (string= "" plaintext)
      ""
      (let* ((santised (remove-if-not #'alphanumericp (string-downcase plaintext)))
             (len (length santised))
             (columns (ceiling (sqrt len)))
             (rows (ceiling (/ len columns)))
             (padding (- (* columns rows) len))
             (padded (format nil "~a~{~a~}" santised (loop repeat padding collect " ")))
             (ciphered (loop for c from 0 below columns
                             collect
                             (loop for r from 0 below rows
                                   collect (aref padded (+ (* r columns) c)) into s
                                   finally (return (coerce s 'string))))))
        (format nil "~{~a~^ ~}" ciphered))))
