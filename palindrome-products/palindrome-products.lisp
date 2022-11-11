(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)

(defun get-factors (value min-factor max-factor)
  (loop for cur from min-factor to (isqrt value)
        for q = (/ value cur)
        when (and (zerop (mod value cur))
                  (<= min-factor q max-factor))
          collect (list cur q)))

(defun smallest (min-factor max-factor)
  (when (< min-factor max-factor)
    (loop for cur from (* min-factor min-factor) to (* max-factor max-factor)
          when (and (string= (write-to-string cur) (reverse (write-to-string cur)))
                    (get-factors cur min-factor max-factor))
            return (values cur (get-factors cur min-factor max-factor)))))

(defun largest (min-factor max-factor)
  (when (< min-factor max-factor)
   (loop for cur from (* max-factor max-factor) downto (* min-factor min-factor)
         when (and (string= (write-to-string cur) (reverse (write-to-string cur)))
                   (get-factors cur min-factor max-factor))
           return (values cur (get-factors cur min-factor max-factor)))))
