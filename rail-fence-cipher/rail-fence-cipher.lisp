(defpackage :rail-fence-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :rail-fence-cipher)

(defun next-step (num-rails rail step)
  (let* ((last-rail (1- num-rails))
        (total (* 2 last-rail)))
    (if (or (zerop rail) (= rail last-rail))
        total
        (if (zerop step)
            (- total (* 2 rail))
            (- total step)))))

(defun build-mapping (letters rails)
  (loop for rail below rails
        append (loop
                 as step = 0 then (next-step rails rail step)
                 as i = 0 then (+ step i)
                 while (< (+ rail i) (length letters))
                 collect (+ rail i))))

(defun encode (msg rails)
    (let* ((letters (remove-if-not #'alpha-char-p msg))
           (mapping (build-mapping letters rails)))
      (loop for pos in mapping
            collect (aref letters pos) into encoded
            finally (return (coerce encoded 'string)))))

(defun decode (msg rails)
  (let ((mapping (build-mapping msg rails))
        (decoded (make-array (length msg) :element-type 'character )))
    (loop for i from 0 below (length msg)
          do (setf (aref decoded (elt mapping i)) (aref msg i)))
    decoded))
