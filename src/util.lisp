(defpackage incognia-wrapper.util
  (:use :cl)
  (:nicknames :incognia.util)
  (:export :plist-remove-null-values))

(in-package :incognia-wrapper.util)

(defun plist-remove-null-values (plist &optional (remove-in-depth t))
  (loop with new-plist = nil
        with tmp-value = nil ;; will hold the result when value is a 'cons
        for (key value) on plist by #'cddr
        when value
          do (setf tmp-value value)
             (if (and remove-in-depth
                      (eq (type-of tmp-value) 'cons))
                 (setf tmp-value (plist-remove-null-values value)))
             (if tmp-value
                 (setf new-plist (append new-plist (list key tmp-value))))
        finally (return new-plist)))
