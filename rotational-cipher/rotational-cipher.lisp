(defpackage :rotational-cipher
  (:use :cl)
  (:export :rotate))

(in-package :rotational-cipher)

(defun rotate-char (ch key &key (uppercase nil))
  (let* ((base (char-code (if uppercase #\A #\a)))
         (code (- (char-code ch) base)))
    (code-char (+ (mod (+ code key) 26) base))))

(defun rotate (text key)
  (map 'string (lambda (ch)
                 (if (alpha-char-p ch)
                     (rotate-char ch key :uppercase (upper-case-p ch))
                     ch))
       text))
