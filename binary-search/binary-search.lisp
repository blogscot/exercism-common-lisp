(defpackage :binary-search
  (:use :cl)
  (:export :binary-find :value-error))

(in-package :binary-search)

(defun binary-find (arr el)
  (labels ((helper (start end)
             (let* ((middle (floor (+ start end) 2))
                    (value (aref arr middle)))
               (cond
                 ((= el value) middle)
                 ((= start middle) nil)
                 ((< el value) (helper start middle))
                 (t            (helper middle end))))))
    (when (plusp (length arr))
     (helper 0 (length arr)))))

;; find 1
;; 1 3 4 6 8 9 11 start 0 end 7 middle 3
;; 1 3 4 6        start 0 end 3 middle 1
;; 1 3            start 0 end 1 middle 1
;; 1              start 0 end 0 middle 0

;; find 11
;; 1 3 4 6 8 9 11 start 0 end 7 middle 3
;;       6 8 9 11 start 3 end 7 middle 5
;;             11 start 5 end 7 middle 6
