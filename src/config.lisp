(defpackage incognia-wrapper.config
  (:use :cl)
  (:nicknames :incognia-apis.config)
  (:export :load-credentials-from-yaml))
(in-package :incognia-wrapper.config)

(defun load-credentials-from-yaml ()
  (let* ((yaml-file (incognia-apis.util:parse-yaml-file #p"./credentials.yaml")))
    (cons (gethash "client-id" yaml-file) (gethash "client-secret" yaml-file))))
