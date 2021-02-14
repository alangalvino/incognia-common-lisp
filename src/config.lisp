(defpackage incognia-wrapper.config
  (:use :cl)
  (:nicknames :incognia.config)
  (:export :load-credentials-from-yaml))
(in-package :incognia-wrapper.config)

(defun load-credentials-from-yaml (&optional (yaml-filepath #p"./credentials.yaml"))
  "Loads credentials from yaml-filepath or ./crendentials.yaml (for a example, see credentials.yaml.example)"
  (let* ((credentials (incognia.util:parse-yaml-file yaml-filepath)))
    (cons (gethash "client-id" credentials) (gethash "client-secret" credentials))))
