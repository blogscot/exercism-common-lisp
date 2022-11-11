(defpackage :scrabble-score
  (:use :cl)
  (:export :score-word))

(in-package :scrabble-score)

(defvar *letter-groups* '(("AEIOULNRST" . 1) ("DG" . 2) ("BCMP" . 3)
                          ("FHVWY" . 4 ) ("K" . 5) ("JX" . 8) ("QZ" . 10)))

(defun score-word (word)
  "Computes the score for an entire word."
  (loop for letter across (string-upcase word)
        sum (loop for group in *letter-groups*
                      when (find letter (car group))
                        sum (cdr group))))
