(defpackage :run-length-encoding
  (:use :cl)
  (:export :encode
   :decode))

(in-package :run-length-encoding)

(defun encode (plain) (format nil "~{~a~}" (compress plain)))

(defun decode (compressed)
  (format nil "~{~a~}" (loop for (num . letter)  in (get-groups compressed)
                             collect (format nil "~v@{~a~:*~}" num letter))))

(defun take-while (predicate list)
  (loop for x across list
        while (funcall predicate x)
        collect x))

(defun compress (string)
  (when (plusp (length string))
      (let ((substring (take-while #'(lambda (x) (equal x (aref string 0))) string)))
        (append
         (list (format nil "~a~a" (if (= 1 (length substring)) "" (length substring)) (aref string 0)))
         (compress (subseq string (length substring)))))))

(defun get-groups (string)
  (when (plusp (length string))
    (let* ((digits (coerce (take-while #'digit-char-p string) 'string))
           (num-digits (length digits)))
      (append
        (if (plusp num-digits)
          (list (cons (parse-integer digits) (aref string num-digits)))
          (list (cons 1 (aref string num-digits))))
        (get-groups (subseq string (1+ num-digits)))))))
