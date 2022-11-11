(defpackage :protein-translation
  (:use :cl :uiop)
  (:export :proteins
           :invalid-protein))

(in-package :protein-translation)

(define-condition invalid-protein (error) nil)

(defparameter +amino-acids+ '((("AUG")                   . "Methionine")
                              (("UUU" "UUC")             . "Phenylalanine")
                              (("UUA" "UUG")             . "Leucine")
                              (("UCU" "UCC" "UCA" "UCG") . "Serine")
                              (("UAU" "UAC")             . "Tyrosine")
                              (("UGU" "UGC")             . "Cysteine")
                              (("UGG")                   . "Tryptophan")
                              (("UAA" "UAG" "UGA")       . "STOP")))

(defun protein (codon)
  (if-let (protein (cdr (assoc-if (lambda (x) (member codon x :test #'equal)) +amino-acids+)))
    protein
    (error 'invalid-protein)))

(defun proteins (strand)
  (loop for s = strand then (subseq s 3)
        until (emptyp s)
        as p = (protein (subseq s 0 (min (length s) 3)))
        until (equal "STOP" p)
        collect p))
