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

(defun hash-table-pretty-print (hash-table &optional (tab 0))
  (loop for key being the hash-keys of hash-table
          using (hash-value value)
        do (if (equal (type-of value) 'hash-table)
               (progn
                 (hash-table-print-key-value-with-tab tab key)
                 (hash-table-pretty-print value (+ tab 1)))
               (hash-table-print-key-value-with-tab tab key value))))

(defun plist-remove-null-values (plist)
  (loop for (key value) on plist by #'cddr
        when value
          append (list key value)))

(defun pretty-print (response-string)
  (hash-table-pretty-print (jonathan:parse response-string :as :hash-table)))

(defun parse-yaml-file (filepath)
  (yaml:parse (asdf:system-relative-pathname :incognia-wrapper filepath)))
