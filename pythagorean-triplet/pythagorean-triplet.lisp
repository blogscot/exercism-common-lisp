(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun triplets-with-sum (n)
  (loop for a from 1 to n
        for middle = (floor (- n a) 2)
        append (loop for b from middle downto a
                     for c = (- n b a)
                     when (= (* c c) (+ (* a a) (* b b)))
                       collect (list a b c))))
