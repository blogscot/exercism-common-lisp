(defpackage :luhn
  (:use :cl)
  (:export :validp))

(in-package :luhn)

(defun double (n)
  (let ((doubled (* 2 n)))
    (- doubled (if (> n 4) 9 0))))

(defun validp (input)
  (let ((spaceless (remove #\Space input)))
    (when (and (string-not-equal "0" spaceless) (every #'digit-char-p spaceless))
      (let* ((digits (map 'list (lambda (d) (- (char-code d) 48)) spaceless))
            (indexed (mapcar 'list (loop for i from (1- (length digits)) downto 0 collect i) digits))
            (doubled (map 'list (lambda (x) (if (oddp (car x)) (double (cadr x)) (cadr x))) indexed)))
       (zerop (mod (apply #'+ doubled) 10))))))
