(defpackage incognia-wrapper.util
  (:use :cl)
  (:nicknames :incognia-apis.util)
  (:export :plist-remove-null-values
           :parse-yaml-file
           :pretty-print))
(in-package :incognia-wrapper.util)

(defun hash-table-print-key-value-with-tab (tab key &optional value (stream t))
  ;; ex. for tab equals 1 "~%1@t~a: ~a:"
  (format stream "~@[~%~v@t~a: ~] ~@[~a~]" tab key value))

(defun hash-table-pretty-print (hash-table &optional (tab 0) (stream t))
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        do (if (equal (type-of value) 'hash-table)
               (progn
                 (hash-table-print-key-value-with-tab tab key nil stream)
                 (hash-table-pretty-print value (+ tab 1)))
               (hash-table-print-key-value-with-tab tab key value stream))))

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

(defun pretty-print (response-string)
  (hash-table-pretty-print (jonathan:parse response-string :as :hash-table)))

(defun parse-yaml-file (filepath)
  (yaml:parse (asdf:system-relative-pathname :incognia-wrapper filepath)))
