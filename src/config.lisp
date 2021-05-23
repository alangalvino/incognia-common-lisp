(in-package :incognia-wrapper)

(defvar *api-config* ())

(deftype region-type () '(member :br :us))

(defun us-region-p ()
  (eq (getf *api-config* :region) :us))

(defun incognia-uri (resource-uri)
  (concatenate 'string (if (us-region-p) *incognia-us-uri*
                           *incognia-br-uri*) resource-uri))

(defun credentials ()
  (let ((client-id (getf *api-config* :client-id))
        (client-secret (getf *api-config* :client-secret)))
    (cons client-id client-secret)))

(defun configure (&key client-id client-secret region)
  (check-type region region-type)
  (if client-id (setf (getf *api-config* :client-id) client-id))
  (if client-secret (setf (getf *api-config* :client-secret) client-secret))
  (if region (setf (getf *api-config* :region) region))
  (revoke-token))
