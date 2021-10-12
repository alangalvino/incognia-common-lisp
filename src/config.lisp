(in-package :cl-incognia)

(defvar *api-config* ())

(deftype region-type () '(member :br :us))

(defun client-id ()
  (getf *api-config* :client-id))

(defun client-secret ()
  (getf *api-config* :client-secret))

(defun region ()
  (getf *api-config* :region))

(defun configure (&key client-id client-secret (region :us))
  (check-type region region-type)
  (if client-id (setf (getf *api-config* :client-id) client-id))
  (if client-secret (setf (getf *api-config* :client-secret) client-secret))
  (if region (setf (getf *api-config* :region) region))
  (revoke-auth-token))

(defun credentials ()
  (cons (client-id) (client-secret)))

(defun us-regionp ()
  (eq (region) :us))
