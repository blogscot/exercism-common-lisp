(defpackage :isbn-verifier
  (:use :cl)
  (:export :validp))

(in-package :isbn-verifier)

(defun check (digits)
  (let ((offset (if (= 9 (length digits)) 10 0))
        (total (loop for digit across digits
                     as n = 10 then (1- n)
                     sum (* (- (char-code digit) 48) n))))
    (zerop (mod (+ offset total) 11))))

(defun validp (isbn)
  (let ((digits (remove #\- isbn)))
    (when (= 10 (length digits))
      (if (char= #\X (elt (reverse digits) 0))
          (check (subseq digits 0 9))
          (check digits)))))
