(in-package :incognia-wrapper)

(defvar *api-config* ())

(deftype region-type () '(member :br :us))

(defun client-id ()
  (getf *api-config* :client-id))

(defun client-secret ()
  (getf *api-config* :client-secret))

(defun region ()
  (getf *api-config* :region))

(defun configure (&key client-id client-secret region)
  (check-type region region-type)
  (if client-id (setf (getf *api-config* :client-id) client-id))
  (if client-secret (setf (getf *api-config* :client-secret) client-secret))
  (if region (setf (getf *api-config* :region) region))
  (revoke-token))

(defun credentials ()
  (cons (client-id) (client-secret)))

(defun us-regionp ()
  (eq (region) :us))

(defun incognia-uri (resource-uri)
  (concatenate 'string (if (us-regionp)
                           *incognia-us-uri*
                           *incognia-br-uri*) resource-uri))
