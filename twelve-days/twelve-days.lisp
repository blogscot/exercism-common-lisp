(defpackage :twelve-days
  (:use :cl)
  (:export :recite))

(in-package :twelve-days)

(defvar *gifts*
  '("a Partridge in a Pear Tree"
    "two Turtle Doves"
    "three French Hens"
    "four Calling Birds"
    "five Gold Rings"
    "six Geese-a-Laying"
    "seven Swans-a-Swimming"
    "eight Maids-a-Milking"
    "nine Ladies Dancing"
    "ten Lords-a-Leaping"
    "eleven Pipers Piping"
    "twelve Drummers Drumming"))

(defun verse (number)
  (let ((gifts (reverse (subseq *gifts* 0 number))))
    (format nil "On the ~:r day of Christmas my true love gave to me: ~{~a~#[~;, and ~:;, ~]~}." number gifts)))

(defun recite (&optional (begin 1 begin-set-p) (end (if begin-set-p begin 12)))
  "Returns a string of the requested verses for the 12 Days of Christmas."
  (format nil "~{~&~A~}"
          (loop for day from begin to end
                collect (verse day))))
