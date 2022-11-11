(defpackage :book-store
  (:use :cl)
  (:export :calculate-price))

(in-package :book-store)

(defparameter *discounts* '(1 0.95 0.9 0.8 0.75))

(defun frequencies (coll)
  (let ((ht (make-hash-table)))
    (reduce (lambda (ht book) (let ((value (gethash book ht 0)))
                                (setf (gethash book ht) (1+ value))
                                ht))
            coll :initial-value ht)))

(defun calculate-book-frequencies (books)
  (sort (loop for values being the hash-values of (frequencies books)
              collect values) #'>))

(defun find-largest-group (book-freqs &key (limit (length book-freqs)))
  (loop for i from 0 below (length book-freqs)
        for frequencies = book-freqs
        if (< i limit)
          collect (1- (elt frequencies i)) into start
        else
          collect (elt frequencies i) into end
        finally (return (values limit (remove-if #'zerop (append start end))))))

(defun group-by (book-freqs &key (by-four nil))
  (if (null book-freqs)
      nil
      (multiple-value-bind (largest-group book-freqs)
          (if by-four
              (find-largest-group book-freqs :limit (min 4 (length book-freqs)))
              (find-largest-group book-freqs))
        (cons largest-group (group-by book-freqs :by-four by-four)))))

(defun calculate-price* (groups)
  (let ((total 0))
    (maphash #'(lambda (key value)
                 (setf total (+ total (* 800 value key (nth (1- key) *discounts*)))))
             (frequencies groups))
    (round total)))

(defun calculate-price (basket)
  (let ((book-freqs (calculate-book-frequencies basket)))
    (min (calculate-price* (group-by book-freqs))
         (calculate-price* (group-by book-freqs :by-four t)))))
