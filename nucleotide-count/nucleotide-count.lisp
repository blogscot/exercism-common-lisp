(defpackage :nucleotide-count
  (:use :cl)
  (:export :dna-count :nucleotide-counts :invalid-nucleotide))

(in-package :nucleotide-count)

(define-condition invalid-nucleotide (error)
  nil)

(defun dna-count (nucleotide strand)
  "Returns a count of the given nucleotide appearing in a DNA strand."
  (if (find nucleotide "AGCT")
      (count nucleotide strand)
      (error 'invalid-nucleotide)))

(defun nucleotide-counts (strand)
  "Returns a hash of nucleotides and their counts in a given DNA strand."
  (let ((table (make-hash-table)))
    (loop for key in '(#\A #\T #\G #\C)
         do (setf (gethash key table) (dna-count key strand)))
    table))
