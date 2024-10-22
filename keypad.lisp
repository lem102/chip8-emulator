(defpackage :keypad
  (:use :cl)
  (:export #:initialise
           #:press
           #:release))

(in-package :keypad)



(defun press (key)
  (setf (aref *keypad* key) t))

(defun release (key)
  (setf (aref *keypad* key) nil))



