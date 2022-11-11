(defpackage :secret-handshake
  (:use :cl)
  (:export :commands))

(in-package :secret-handshake)

(defparameter secret-handshakes '("wink" "double blink" "close your eyes" "jump"))
(defparameter reverse-bit 4)

(defun commands (number)
  (let ((actions (loop for index from 0 below (length secret-handshakes)
                       when (logbitp index number)
                         collect (nth index secret-handshakes))))
    (if (logbitp reverse-bit number)
        (reverse actions)
        actions)))
