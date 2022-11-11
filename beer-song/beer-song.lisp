(defpackage :beer-song
  (:use :cl)
  (:export :verse :sing))

(in-package :beer-song)

(defun line1 (n)
  (format nil "~[No more~:;~a~] bottle~:p of beer on the wall, ~[no more~:;~a~] bottle~:p of beer.~%" n n n n))

(defun line2 (n)
  (if (zerop n)
      "Go to the store and buy some more, 99 bottles of beer on the wall."
      (format nil "Take ~[~;it~:;one~] down and pass it around, ~[no more~:;~a~] bottle~:p of beer on the wall.~%" n (1- n) (1- n))))

(defun verse (n)
  "Returns a string verse for a given number."
  (format nil "~a~&~a~&" (line1 n) (line2 n)))

(defun sing (start &optional (end 0))
  "Returns a string of verses for a given range of numbers."
  (apply #'concatenate 'string
   (loop for number from start downto end
         collect (format nil "~a~%" (verse number)))))
