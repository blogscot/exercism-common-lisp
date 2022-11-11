(defpackage :bob
  (:use :cl)
  (:export :response))
(in-package :bob)

(defun questionp (response)
  (char= #\? (uiop:last-char response)))

(defun shoutingp (response)
  (and
   (find-if #'alpha-char-p response)
   (equal (string-upcase response) response)))

(defun response (hey-bob)
  (let ((saying (string-trim '(#\Space #\Tab #\Newline) hey-bob)))
   (cond
     ((zerop (length saying)) "Fine. Be that way!")
     ((and (shoutingp saying) (questionp saying)) "Calm down, I know what I'm doing!")
     ((questionp saying) "Sure.")
     ((shoutingp saying) "Whoa, chill out!" )
     (t "Whatever."))))
